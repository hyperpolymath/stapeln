# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)

defmodule Stapeln.SecurityScanner do
  @moduledoc """
  Security scanning for container stacks.

  Produces SecurityInspector-compatible findings by analysing services for
  common container security anti-patterns: root containers, exposed databases,
  missing encryption, default credentials, absent resource limits, and more.
  """

  alias Stapeln.ValidationEngine

  @known_db_kinds ~w(db database postgres postgresql mysql mariadb mongo mongodb redis memcached cassandra elasticsearch)
  @sensitive_ports [3306, 5432, 27017, 6379, 9200, 11211, 9042]
  @default_cred_images ~w(adminer phpmyadmin mongo-express redis-commander)

  @doc """
  Scan a stack map and return a security report.

  Returns a map with:
  - `:metrics` — security / performance / reliability / compliance scores (0-100)
  - `:grade` — letter grade A+ through F
  - `:vulnerabilities` — list of vulnerability maps
  - `:checks` — list of check-result maps
  - `:exposedPorts` — list of exposed-port maps
  """
  @spec scan(map()) :: map()
  def scan(stack) when is_map(stack) do
    services = Map.get(stack, :services, [])
    validation = ValidationEngine.validate(stack)

    vulnerabilities = detect_vulnerabilities(services)
    checks = run_security_checks(services)
    exposed_ports = detect_exposed_ports(services)
    metrics = calculate_metrics(vulnerabilities, checks, validation)
    grade = calculate_grade(metrics)

    %{
      metrics: metrics,
      grade: grade,
      vulnerabilities: vulnerabilities,
      checks: checks,
      exposedPorts: exposed_ports
    }
  end

  # ---------------------------------------------------------------------------
  # Vulnerability Detection
  # ---------------------------------------------------------------------------

  defp detect_vulnerabilities(services) do
    Enum.flat_map(services, fn service ->
      check_root_container_vuln(service) ++
        check_unencrypted_connection(service) ++
        check_exposed_database(service) ++
        check_default_credentials(service) ++
        check_missing_firewall(service)
    end)
    |> Enum.with_index(1)
    |> Enum.map(fn {vuln, idx} -> Map.put(vuln, :id, "VULN-#{String.pad_leading(Integer.to_string(idx), 4, "0")}") end)
  end

  defp check_root_container_vuln(service) do
    name = service_name(service)
    user = Map.get(service, :user) || Map.get(service, "user")

    if is_nil(user) or user == "root" or user == "0" do
      [
        %{
          title: "Container '#{name}' runs as root",
          severity: "high",
          description:
            "Service '#{name}' either has no user specified or explicitly runs as root (UID 0). " <>
              "A compromised root container can escalate to host-level access.",
          affectedComponent: name,
          cveId: nil,
          fixAvailable: true,
          fixDescription: "Add a non-root USER directive or set `user:` to a numeric UID > 0."
        }
      ]
    else
      []
    end
  end

  defp check_unencrypted_connection(service) do
    name = service_name(service)
    port = service_port(service)
    kind = service_kind(service)

    cond do
      port == 80 ->
        [
          %{
            title: "Unencrypted HTTP on '#{name}'",
            severity: "medium",
            description:
              "Service '#{name}' exposes port 80 (plain HTTP). Traffic is unencrypted and " <>
                "susceptible to man-in-the-middle attacks.",
            affectedComponent: name,
            cveId: nil,
            fixAvailable: true,
            fixDescription: "Terminate TLS at a reverse proxy or switch to port 443 with a certificate."
          }
        ]

      kind in @known_db_kinds and not tls_configured?(service) ->
        [
          %{
            title: "Unencrypted database connection on '#{name}'",
            severity: "high",
            description:
              "Database service '#{name}' (#{kind}) has no TLS/SSL configuration. " <>
                "Credentials and query data travel in plaintext.",
            affectedComponent: name,
            cveId: nil,
            fixAvailable: true,
            fixDescription: "Enable SSL/TLS on the database listener and require encrypted client connections."
          }
        ]

      true ->
        []
    end
  end

  defp check_exposed_database(service) do
    name = service_name(service)
    port = service_port(service)
    kind = service_kind(service)

    publicly_accessible =
      Map.get(service, :public, false) == true or
        Map.get(service, "public", false) == true

    if kind in @known_db_kinds and (publicly_accessible or port in @sensitive_ports) do
      [
        %{
          title: "Database '#{name}' exposed to external network",
          severity: "critical",
          description:
            "Service '#{name}' (#{kind}) is reachable from outside the container network on port #{port || "default"}. " <>
              "Direct database access from the internet is a major attack vector.",
          affectedComponent: name,
          cveId: nil,
          fixAvailable: true,
          fixDescription: "Remove public port binding and route database access through an application service or VPN."
        }
      ]
    else
      []
    end
  end

  defp check_default_credentials(service) do
    name = service_name(service)
    image = Map.get(service, :image) || Map.get(service, "image") || ""
    env = Map.get(service, :environment) || Map.get(service, "environment") || %{}

    image_base = image |> String.split("/") |> List.last() |> to_string() |> String.split(":") |> List.first()

    has_default_creds =
      image_base in @default_cred_images or
        has_weak_env_password?(env)

    if has_default_creds do
      [
        %{
          title: "Default or weak credentials on '#{name}'",
          severity: "high",
          description:
            "Service '#{name}' appears to use default or weak credentials. " <>
              "Automated scanners routinely probe for default passwords.",
          affectedComponent: name,
          cveId: nil,
          fixAvailable: true,
          fixDescription: "Use strong, randomly generated credentials stored in a secrets manager."
        }
      ]
    else
      []
    end
  end

  defp check_missing_firewall(service) do
    name = service_name(service)
    network = Map.get(service, :network) || Map.get(service, "network")
    networks = Map.get(service, :networks) || Map.get(service, "networks")

    if is_nil(network) and is_nil(networks) do
      [
        %{
          title: "No network segmentation for '#{name}'",
          severity: "medium",
          description:
            "Service '#{name}' is not assigned to a specific Docker/Podman network. " <>
              "All services share the default bridge, allowing unrestricted lateral movement.",
          affectedComponent: name,
          cveId: nil,
          fixAvailable: true,
          fixDescription: "Define dedicated networks (e.g. frontend, backend, data) and assign services appropriately."
        }
      ]
    else
      []
    end
  end

  # ---------------------------------------------------------------------------
  # Security Checks
  # ---------------------------------------------------------------------------

  defp run_security_checks(services) do
    [
      check_image_signatures(services),
      check_sbom_presence(services),
      check_non_root(services),
      check_health_checks(services),
      check_resource_limits(services),
      check_network_segmentation(services),
      check_read_only_rootfs(services),
      check_privilege_escalation(services)
    ]
  end

  defp check_image_signatures(services) do
    signed =
      Enum.count(services, fn s ->
        Map.get(s, :image_signed, false) == true or
          Map.get(s, "image_signed", false) == true
      end)

    total = max(length(services), 1)

    %{
      name: "Image Signatures",
      description: "Container images are cryptographically signed and verified.",
      result: if(signed == total and total > 0, do: "pass", else: "fail"),
      details: "#{signed}/#{total} images have verified signatures."
    }
  end

  defp check_sbom_presence(services) do
    with_sbom =
      Enum.count(services, fn s ->
        Map.get(s, :sbom, false) == true or
          Map.get(s, "sbom", false) == true
      end)

    total = max(length(services), 1)

    %{
      name: "Software Bill of Materials",
      description: "Each image has a machine-readable SBOM attached.",
      result: if(with_sbom == total and total > 0, do: "pass", else: "warning"),
      details: "#{with_sbom}/#{total} images have an SBOM."
    }
  end

  defp check_non_root(services) do
    non_root =
      Enum.count(services, fn s ->
        user = Map.get(s, :user) || Map.get(s, "user")
        not is_nil(user) and user != "root" and user != "0"
      end)

    total = max(length(services), 1)

    %{
      name: "Non-Root Containers",
      description: "All containers run as non-root users.",
      result: if(non_root == total and total > 0, do: "pass", else: "fail"),
      details: "#{non_root}/#{total} services run as non-root."
    }
  end

  defp check_health_checks(services) do
    with_health =
      Enum.count(services, fn s ->
        hc = Map.get(s, :healthcheck) || Map.get(s, "healthcheck")
        not is_nil(hc)
      end)

    total = max(length(services), 1)

    %{
      name: "Health Checks",
      description: "Services define container-level health checks.",
      result: if(with_health == total and total > 0, do: "pass", else: "warning"),
      details: "#{with_health}/#{total} services have health checks."
    }
  end

  defp check_resource_limits(services) do
    with_limits =
      Enum.count(services, fn s ->
        mem = Map.get(s, :mem_limit) || Map.get(s, "mem_limit") || Map.get(s, :memory) || Map.get(s, "memory")
        cpu = Map.get(s, :cpu_limit) || Map.get(s, "cpu_limit") || Map.get(s, :cpus) || Map.get(s, "cpus")
        not is_nil(mem) or not is_nil(cpu)
      end)

    total = max(length(services), 1)

    %{
      name: "Resource Limits",
      description: "CPU and memory limits are set to prevent resource exhaustion.",
      result: if(with_limits == total and total > 0, do: "pass", else: "fail"),
      details: "#{with_limits}/#{total} services have resource limits."
    }
  end

  defp check_network_segmentation(services) do
    with_network =
      Enum.count(services, fn s ->
        net = Map.get(s, :network) || Map.get(s, "network") || Map.get(s, :networks) || Map.get(s, "networks")
        not is_nil(net)
      end)

    total = max(length(services), 1)

    %{
      name: "Network Segmentation",
      description: "Services are isolated into purpose-specific networks.",
      result: if(with_network == total and total > 0, do: "pass", else: "warning"),
      details: "#{with_network}/#{total} services have explicit network assignments."
    }
  end

  defp check_read_only_rootfs(services) do
    with_ro =
      Enum.count(services, fn s ->
        Map.get(s, :read_only, false) == true or
          Map.get(s, "read_only", false) == true
      end)

    total = max(length(services), 1)

    %{
      name: "Read-Only Root Filesystem",
      description: "Container root filesystems are mounted read-only to prevent tampering.",
      result: if(with_ro == total and total > 0, do: "pass", else: "warning"),
      details: "#{with_ro}/#{total} services use read-only root filesystems."
    }
  end

  defp check_privilege_escalation(services) do
    secured =
      Enum.count(services, fn s ->
        no_new_privs =
          Map.get(s, :no_new_privileges, false) == true or
            Map.get(s, "no_new_privileges", false) == true

        not_privileged =
          Map.get(s, :privileged, false) != true and
            Map.get(s, "privileged", false) != true

        no_new_privs and not_privileged
      end)

    total = max(length(services), 1)

    %{
      name: "Privilege Escalation Prevention",
      description: "Containers cannot gain additional privileges at runtime.",
      result: if(secured == total and total > 0, do: "pass", else: "warning"),
      details: "#{secured}/#{total} services have privilege escalation prevention."
    }
  end

  # ---------------------------------------------------------------------------
  # Exposed Ports
  # ---------------------------------------------------------------------------

  defp detect_exposed_ports(services) do
    Enum.flat_map(services, fn service ->
      port = service_port(service)
      name = service_name(service)
      kind = service_kind(service)

      if port do
        publicly_accessible =
          Map.get(service, :public, false) == true or
            Map.get(service, "public", false) == true

        [
          %{
            port: port,
            protocol: guess_protocol(port, kind),
            service: name,
            risk: port_risk(port, kind, publicly_accessible),
            publiclyAccessible: publicly_accessible
          }
        ]
      else
        []
      end
    end)
  end

  # ---------------------------------------------------------------------------
  # Metrics & Grading
  # ---------------------------------------------------------------------------

  defp calculate_metrics(vulnerabilities, checks, validation) do
    vuln_penalty =
      Enum.reduce(vulnerabilities, 0, fn v, acc ->
        acc +
          case v.severity do
            "critical" -> 25
            "high" -> 15
            "medium" -> 8
            "low" -> 3
            _ -> 5
          end
      end)

    check_pass_count = Enum.count(checks, fn c -> c.result == "pass" end)
    check_total = max(length(checks), 1)
    check_ratio = check_pass_count / check_total

    security = max(0, round(100 - vuln_penalty)) |> min(100)
    performance = round(check_ratio * 80 + 20) |> min(100)
    reliability = max(0, validation.score)

    compliance =
      round(
        (check_ratio * 60 + max(0, 100 - vuln_penalty * 0.5) * 0.4)
      )
      |> max(0)
      |> min(100)

    %{
      security: security,
      performance: performance,
      reliability: reliability,
      compliance: compliance
    }
  end

  defp calculate_grade(metrics) do
    avg =
      (metrics.security + metrics.performance + metrics.reliability + metrics.compliance) / 4

    cond do
      avg >= 97 -> "A+"
      avg >= 93 -> "A"
      avg >= 90 -> "A-"
      avg >= 87 -> "B+"
      avg >= 83 -> "B"
      avg >= 80 -> "B-"
      avg >= 77 -> "C+"
      avg >= 73 -> "C"
      avg >= 70 -> "C-"
      avg >= 67 -> "D+"
      avg >= 63 -> "D"
      avg >= 60 -> "D-"
      true -> "F"
    end
  end

  # ---------------------------------------------------------------------------
  # Helpers
  # ---------------------------------------------------------------------------

  defp service_name(service) do
    Map.get(service, :name) || Map.get(service, "name") || "unnamed"
  end

  defp service_kind(service) do
    (Map.get(service, :kind) || Map.get(service, "kind") || "unknown")
    |> to_string()
    |> String.downcase()
  end

  defp service_port(service) do
    raw = Map.get(service, :port) || Map.get(service, "port")

    case raw do
      p when is_integer(p) -> p
      p when is_binary(p) ->
        case Integer.parse(p) do
          {num, _} -> num
          :error -> nil
        end
      _ -> nil
    end
  end

  defp tls_configured?(service) do
    ssl = Map.get(service, :ssl) || Map.get(service, "ssl")
    tls = Map.get(service, :tls) || Map.get(service, "tls")
    ssl == true or tls == true
  end

  defp has_weak_env_password?(env) when is_map(env) do
    weak_passwords = ~w(password admin 123456 root changeme secret test)

    Enum.any?(env, fn {key, value} ->
      key_str = to_string(key) |> String.downcase()
      val_str = to_string(value) |> String.downcase()

      String.contains?(key_str, "password") and val_str in weak_passwords
    end)
  end

  defp has_weak_env_password?(env) when is_list(env) do
    env
    |> Enum.into(%{}, fn
      {k, v} -> {k, v}
      item when is_binary(item) ->
        case String.split(item, "=", parts: 2) do
          [k, v] -> {k, v}
          _ -> {item, ""}
        end
      _ -> {"", ""}
    end)
    |> has_weak_env_password?()
  end

  defp has_weak_env_password?(_), do: false

  defp guess_protocol(port, kind) do
    cond do
      port in [80, 8080, 8000, 3000, 4000] -> "HTTP"
      port in [443, 8443] -> "HTTPS"
      port == 5432 or kind in ~w(postgres postgresql) -> "PostgreSQL"
      port == 3306 or kind in ~w(mysql mariadb) -> "MySQL"
      port == 27017 or kind in ~w(mongo mongodb) -> "MongoDB"
      port == 6379 or kind == "redis" -> "Redis"
      port == 9200 or kind == "elasticsearch" -> "Elasticsearch"
      port == 11211 or kind == "memcached" -> "Memcached"
      port == 9042 or kind == "cassandra" -> "CQL"
      port == 22 -> "SSH"
      port == 53 -> "DNS"
      port == 25 or port == 587 -> "SMTP"
      port == 5672 or kind in ~w(rabbitmq amqp) -> "AMQP"
      port == 9092 or kind == "kafka" -> "Kafka"
      true -> "TCP"
    end
  end

  defp port_risk(port, kind, publicly_accessible) do
    cond do
      publicly_accessible and port in @sensitive_ports -> "critical"
      publicly_accessible and kind in @known_db_kinds -> "critical"
      port in @sensitive_ports -> "high"
      kind in @known_db_kinds -> "high"
      port == 22 -> "high"
      publicly_accessible -> "medium"
      port in [80, 8080] -> "low"
      port in [443, 8443] -> "low"
      true -> "medium"
    end
  end
end
