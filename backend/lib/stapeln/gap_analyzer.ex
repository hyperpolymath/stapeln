# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)

defmodule Stapeln.GapAnalyzer do
  @moduledoc """
  Gap analysis for container stacks.

  Produces GapAnalysis-compatible findings that identify missing best practices,
  hardening steps, and operational readiness gaps in a stack definition.
  Each gap includes concrete fix commands and effort estimates.
  """

  @known_db_kinds ~w(db database postgres postgresql mysql mariadb mongo mongodb redis memcached cassandra elasticsearch)

  @doc """
  Analyse a stack map and return a gap report.

  Returns `%{gaps: [gap]}` where each gap contains id, title, category,
  severity, description, impact, fix information, effort estimates, and tags.
  """
  @spec analyze(map()) :: %{gaps: [map()]}
  def analyze(stack) when is_map(stack) do
    services = Map.get(stack, :services, [])

    gaps =
      (check_health_endpoints(services) ++
         check_encryption(services) ++
         check_root_containers(services) ++
         check_resource_limits(services) ++
         check_rate_limiting(services) ++
         check_sbom(services) ++
         check_logging(services) ++
         check_backup_strategy(services) ++
         check_restart_policy(services) ++
         check_image_pinning(services) ++
         check_secrets_management(services))
      |> Enum.with_index(1)
      |> Enum.map(fn {gap, idx} ->
        Map.put(gap, :id, "GAP-#{String.pad_leading(Integer.to_string(idx), 4, "0")}")
      end)

    %{gaps: gaps}
  end

  # ---------------------------------------------------------------------------
  # Health Endpoints
  # ---------------------------------------------------------------------------

  defp check_health_endpoints(services) do
    Enum.flat_map(services, fn service ->
      name = svc_name(service)
      healthcheck = Map.get(service, :healthcheck) || Map.get(service, "healthcheck")

      if is_nil(healthcheck) do
        [
          %{
            title: "Missing health check for '#{name}'",
            category: "reliability",
            severity: "medium",
            description:
              "Service '#{name}' has no container-level health check. The orchestrator " <>
                "cannot distinguish a hung process from a healthy one.",
            impact:
              "Failed services are not automatically restarted; cascading failures go undetected.",
            source: "stapeln-gap-analyzer",
            scope: "service",
            affectedComponents: [name],
            fixAvailable: true,
            fixConfidence: "high",
            fixDescription: "Add a HEALTHCHECK instruction to the Containerfile or a healthcheck key in the stack definition.",
            fixCommands: [
              "# In Containerfile:",
              "HEALTHCHECK --interval=30s --timeout=5s --retries=3 CMD curl -f http://localhost:#{svc_port(service) || 8080}/healthz || exit 1",
              "# Or in stack definition, add healthcheck: { test: ['CMD', 'curl', '-f', 'http://localhost/healthz'], interval: '30s' }"
            ],
            estimatedEffort: "15 minutes",
            tags: ["health", "reliability", "observability"]
          }
        ]
      else
        []
      end
    end)
  end

  # ---------------------------------------------------------------------------
  # Encryption
  # ---------------------------------------------------------------------------

  defp check_encryption(services) do
    Enum.flat_map(services, fn service ->
      name = svc_name(service)
      kind = svc_kind(service)
      port = svc_port(service)
      ssl = Map.get(service, :ssl) || Map.get(service, "ssl")
      tls = Map.get(service, :tls) || Map.get(service, "tls")
      has_tls = ssl == true or tls == true

      cond do
        kind in @known_db_kinds and not has_tls ->
          [
            %{
              title: "No TLS on database '#{name}'",
              category: "security",
              severity: "high",
              description:
                "Database service '#{name}' (#{kind}) does not have TLS configured. " <>
                  "All queries and credentials travel in plaintext on the network.",
              impact: "Credentials and sensitive data are exposed to network sniffing.",
              source: "stapeln-gap-analyzer",
              scope: "service",
              affectedComponents: [name],
              fixAvailable: true,
              fixConfidence: "high",
              fixDescription: "Enable TLS on the database listener and set ssl: true in the service definition.",
              fixCommands: [
                "# Add to service definition: ssl: true",
                "# For PostgreSQL: set ssl = on in postgresql.conf",
                "# For MySQL: set require_secure_transport = ON"
              ],
              estimatedEffort: "30 minutes",
              tags: ["encryption", "security", "compliance"]
            }
          ]

        port == 80 ->
          [
            %{
              title: "Plain HTTP on '#{name}'",
              category: "security",
              severity: "medium",
              description:
                "Service '#{name}' listens on port 80 (unencrypted HTTP). " <>
                  "Any data exchanged is visible to network observers.",
              impact: "Session tokens and user data can be intercepted.",
              source: "stapeln-gap-analyzer",
              scope: "service",
              affectedComponents: [name],
              fixAvailable: true,
              fixConfidence: "high",
              fixDescription: "Terminate TLS at a reverse proxy or switch to HTTPS.",
              fixCommands: [
                "# Add a reverse proxy service (e.g. Caddy or Traefik) with automatic TLS",
                "# Or change port to 443 and mount TLS certificates"
              ],
              estimatedEffort: "1 hour",
              tags: ["encryption", "security", "tls"]
            }
          ]

        true ->
          []
      end
    end)
  end

  # ---------------------------------------------------------------------------
  # Root Containers
  # ---------------------------------------------------------------------------

  defp check_root_containers(services) do
    Enum.flat_map(services, fn service ->
      name = svc_name(service)
      user = Map.get(service, :user) || Map.get(service, "user")

      if is_nil(user) or user == "root" or user == "0" do
        [
          %{
            title: "Container '#{name}' runs as root",
            category: "security",
            severity: "high",
            description:
              "Service '#{name}' runs as root (UID 0) or has no user specified, defaulting to root.",
            impact:
              "Container escape vulnerabilities grant full host access when running as root.",
            source: "stapeln-gap-analyzer",
            scope: "service",
            affectedComponents: [name],
            fixAvailable: true,
            fixConfidence: "high",
            fixDescription: "Set a non-root user in the Containerfile or stack definition.",
            fixCommands: [
              "# In Containerfile:",
              "RUN useradd -r -u 1001 appuser",
              "USER appuser",
              "# Or in stack definition: user: '1001'"
            ],
            estimatedEffort: "15 minutes",
            tags: ["security", "hardening", "root"]
          }
        ]
      else
        []
      end
    end)
  end

  # ---------------------------------------------------------------------------
  # Resource Limits
  # ---------------------------------------------------------------------------

  defp check_resource_limits(services) do
    Enum.flat_map(services, fn service ->
      name = svc_name(service)

      mem =
        Map.get(service, :mem_limit) || Map.get(service, "mem_limit") ||
          Map.get(service, :memory) || Map.get(service, "memory")

      cpu =
        Map.get(service, :cpu_limit) || Map.get(service, "cpu_limit") ||
          Map.get(service, :cpus) || Map.get(service, "cpus")

      gaps = []

      gaps =
        if is_nil(mem) do
          [
            %{
              title: "No memory limit on '#{name}'",
              category: "reliability",
              severity: "medium",
              description: "Service '#{name}' has no memory limit. A memory leak can consume all host RAM and trigger the OOM killer.",
              impact: "Uncontrolled memory consumption can crash the host or co-located services.",
              source: "stapeln-gap-analyzer",
              scope: "service",
              affectedComponents: [name],
              fixAvailable: true,
              fixConfidence: "high",
              fixDescription: "Set a memory limit appropriate for the service workload.",
              fixCommands: ["# Add to service definition: mem_limit: '512m'"],
              estimatedEffort: "5 minutes",
              tags: ["resources", "reliability", "limits"]
            }
            | gaps
          ]
        else
          gaps
        end

      if is_nil(cpu) do
        [
          %{
            title: "No CPU limit on '#{name}'",
            category: "reliability",
            severity: "low",
            description: "Service '#{name}' has no CPU limit. A busy-loop or fork bomb can starve other services.",
            impact: "Runaway CPU usage degrades all co-located containers.",
            source: "stapeln-gap-analyzer",
            scope: "service",
            affectedComponents: [name],
            fixAvailable: true,
            fixConfidence: "high",
            fixDescription: "Set a CPU limit to constrain processor usage.",
            fixCommands: ["# Add to service definition: cpus: '0.5'"],
            estimatedEffort: "5 minutes",
            tags: ["resources", "reliability", "limits"]
          }
          | gaps
        ]
      else
        gaps
      end
    end)
  end

  # ---------------------------------------------------------------------------
  # Rate Limiting
  # ---------------------------------------------------------------------------

  defp check_rate_limiting(services) do
    web_services =
      Enum.filter(services, fn s ->
        kind = svc_kind(s)
        port = svc_port(s)
        kind in ~w(web api gateway proxy http) or port in [80, 443, 8080, 3000, 4000, 8000]
      end)

    if length(web_services) > 0 do
      has_proxy =
        Enum.any?(services, fn s ->
          kind = svc_kind(s)
          name = svc_name(s) |> String.downcase()
          kind in ~w(proxy gateway loadbalancer lb nginx caddy traefik haproxy envoy) or
            String.contains?(name, "proxy") or String.contains?(name, "gateway")
        end)

      if not has_proxy do
        affected = Enum.map(web_services, &svc_name/1)

        [
          %{
            title: "No rate limiting or reverse proxy detected",
            category: "security",
            severity: "medium",
            description:
              "Web-facing services exist but no reverse proxy or API gateway is present " <>
                "to enforce rate limiting, request size limits, or abuse prevention.",
            impact: "Services are vulnerable to denial-of-service and brute-force attacks.",
            source: "stapeln-gap-analyzer",
            scope: "stack",
            affectedComponents: affected,
            fixAvailable: true,
            fixConfidence: "medium",
            fixDescription: "Add a reverse proxy (Caddy, Traefik, Nginx) with rate limiting configuration.",
            fixCommands: [
              "# Add a proxy service to the stack, e.g.:",
              "# - name: proxy",
              "#   kind: proxy",
              "#   image: caddy:2-alpine",
              "#   port: 443"
            ],
            estimatedEffort: "2 hours",
            tags: ["security", "rate-limiting", "dos"]
          }
        ]
      else
        []
      end
    else
      []
    end
  end

  # ---------------------------------------------------------------------------
  # SBOM
  # ---------------------------------------------------------------------------

  defp check_sbom(services) do
    without_sbom =
      Enum.filter(services, fn s ->
        sbom = Map.get(s, :sbom, false) == true or Map.get(s, "sbom", false) == true
        not sbom
      end)

    if length(without_sbom) > 0 do
      affected = Enum.map(without_sbom, &svc_name/1)

      [
        %{
          title: "Missing SBOM for #{length(without_sbom)} service(s)",
          category: "compliance",
          severity: "low",
          description:
            "#{length(without_sbom)} service image(s) lack a Software Bill of Materials. " <>
              "SBOM is increasingly required for supply-chain compliance (EO 14028, EU CRA).",
          impact: "Cannot audit transitive dependencies for known vulnerabilities.",
          source: "stapeln-gap-analyzer",
          scope: "stack",
          affectedComponents: affected,
          fixAvailable: true,
          fixConfidence: "high",
          fixDescription: "Generate SBOMs with Syft or Trivy and attach to images.",
          fixCommands: [
            "# Generate SBOM with Syft:",
            "syft <image> -o spdx-json > sbom.spdx.json",
            "# Or with Trivy:",
            "trivy image --format spdx-json -o sbom.spdx.json <image>"
          ],
          estimatedEffort: "30 minutes",
          tags: ["compliance", "sbom", "supply-chain"]
        }
      ]
    else
      []
    end
  end

  # ---------------------------------------------------------------------------
  # Logging
  # ---------------------------------------------------------------------------

  defp check_logging(services) do
    without_logging =
      Enum.filter(services, fn s ->
        log_driver = Map.get(s, :log_driver) || Map.get(s, "log_driver") ||
                     Map.get(s, :logging) || Map.get(s, "logging")
        is_nil(log_driver)
      end)

    if length(without_logging) > 0 do
      affected = Enum.map(without_logging, &svc_name/1)

      [
        %{
          title: "No explicit logging configuration for #{length(without_logging)} service(s)",
          category: "observability",
          severity: "low",
          description:
            "#{length(without_logging)} service(s) rely on the default container log driver. " <>
              "Logs may be lost on container restart and are not forwarded to a central aggregator.",
          impact: "Incident investigation is hampered by missing or incomplete logs.",
          source: "stapeln-gap-analyzer",
          scope: "stack",
          affectedComponents: affected,
          fixAvailable: true,
          fixConfidence: "medium",
          fixDescription: "Configure a structured logging driver (e.g. journald, fluentd, loki).",
          fixCommands: [
            "# Add to each service:",
            "# logging:",
            "#   driver: journald",
            "#   options:",
            "#     tag: '{{.Name}}'"
          ],
          estimatedEffort: "30 minutes",
          tags: ["observability", "logging", "operations"]
        }
      ]
    else
      []
    end
  end

  # ---------------------------------------------------------------------------
  # Backup Strategy
  # ---------------------------------------------------------------------------

  defp check_backup_strategy(services) do
    db_services =
      Enum.filter(services, fn s ->
        svc_kind(s) in @known_db_kinds
      end)

    Enum.flat_map(db_services, fn service ->
      name = svc_name(service)
      volumes = Map.get(service, :volumes) || Map.get(service, "volumes")
      backup = Map.get(service, :backup) || Map.get(service, "backup")

      if is_nil(backup) do
        [
          %{
            title: "No backup strategy for database '#{name}'",
            category: "reliability",
            severity: "high",
            description:
              "Database service '#{name}' has no backup configuration. " <>
                if(is_nil(volumes), do: "Data volumes are also not explicitly defined — data is ephemeral.", else: "Volumes are defined but no backup schedule is configured."),
            impact: "Data loss on container failure, host failure, or accidental deletion.",
            source: "stapeln-gap-analyzer",
            scope: "service",
            affectedComponents: [name],
            fixAvailable: true,
            fixConfidence: "medium",
            fixDescription: "Add a backup sidecar or cron-based backup job for the database.",
            fixCommands: [
              "# For PostgreSQL: pg_dump -Fc > backup.dump",
              "# For MySQL: mysqldump --all-databases > backup.sql",
              "# Add a backup service/sidecar to the stack with a cron schedule"
            ],
            estimatedEffort: "2 hours",
            tags: ["reliability", "backup", "data"]
          }
        ]
      else
        []
      end
    end)
  end

  # ---------------------------------------------------------------------------
  # Restart Policy
  # ---------------------------------------------------------------------------

  defp check_restart_policy(services) do
    Enum.flat_map(services, fn service ->
      name = svc_name(service)
      restart = Map.get(service, :restart) || Map.get(service, "restart")

      if is_nil(restart) do
        [
          %{
            title: "No restart policy for '#{name}'",
            category: "reliability",
            severity: "low",
            description: "Service '#{name}' has no restart policy. If the process crashes, the container stays down.",
            impact: "Transient failures cause permanent service outages until manual intervention.",
            source: "stapeln-gap-analyzer",
            scope: "service",
            affectedComponents: [name],
            fixAvailable: true,
            fixConfidence: "high",
            fixDescription: "Set a restart policy like 'unless-stopped' or 'on-failure'.",
            fixCommands: ["# Add to service definition: restart: 'unless-stopped'"],
            estimatedEffort: "5 minutes",
            tags: ["reliability", "restart", "operations"]
          }
        ]
      else
        []
      end
    end)
  end

  # ---------------------------------------------------------------------------
  # Image Pinning
  # ---------------------------------------------------------------------------

  defp check_image_pinning(services) do
    Enum.flat_map(services, fn service ->
      name = svc_name(service)
      image = Map.get(service, :image) || Map.get(service, "image") || ""

      uses_latest = String.ends_with?(image, ":latest") or not String.contains?(image, ":")
      has_digest = String.contains?(image, "@sha256:")

      if image != "" and uses_latest and not has_digest do
        [
          %{
            title: "Unpinned image tag for '#{name}'",
            category: "security",
            severity: "medium",
            description:
              "Service '#{name}' uses image '#{image}' with the :latest tag or no tag at all. " <>
                "The actual image content can change without notice.",
            impact: "Builds are not reproducible; supply-chain attacks can inject malicious layers.",
            source: "stapeln-gap-analyzer",
            scope: "service",
            affectedComponents: [name],
            fixAvailable: true,
            fixConfidence: "high",
            fixDescription: "Pin the image to a specific version tag or SHA256 digest.",
            fixCommands: [
              "# Use a specific version: image: '#{String.replace(image, ":latest", "")}:1.0.0'",
              "# Or pin by digest: image: '#{String.replace(image, ":latest", "")}@sha256:<digest>'"
            ],
            estimatedEffort: "10 minutes",
            tags: ["security", "supply-chain", "reproducibility"]
          }
        ]
      else
        []
      end
    end)
  end

  # ---------------------------------------------------------------------------
  # Secrets Management
  # ---------------------------------------------------------------------------

  defp check_secrets_management(services) do
    services_with_inline_secrets =
      Enum.filter(services, fn service ->
        env = Map.get(service, :environment) || Map.get(service, "environment") || %{}
        has_inline_secrets?(env)
      end)

    if length(services_with_inline_secrets) > 0 do
      affected = Enum.map(services_with_inline_secrets, &svc_name/1)

      [
        %{
          title: "Inline secrets in environment variables",
          category: "security",
          severity: "high",
          description:
            "#{length(services_with_inline_secrets)} service(s) appear to have passwords or API keys " <>
              "defined directly in environment variables rather than using a secrets manager.",
          impact: "Secrets are visible in container inspect output, logs, and stack definition files.",
          source: "stapeln-gap-analyzer",
          scope: "stack",
          affectedComponents: affected,
          fixAvailable: true,
          fixConfidence: "medium",
          fixDescription: "Use container secrets (Podman/Docker secrets) or an external secrets manager.",
          fixCommands: [
            "# Create a secret: echo 'mypassword' | podman secret create db_password -",
            "# Reference in stack: secrets: [db_password]",
            "# In the service, read from /run/secrets/db_password"
          ],
          estimatedEffort: "1 hour",
          tags: ["security", "secrets", "credentials"]
        }
      ]
    else
      []
    end
  end

  # ---------------------------------------------------------------------------
  # Helpers
  # ---------------------------------------------------------------------------

  defp svc_name(service) do
    Map.get(service, :name) || Map.get(service, "name") || "unnamed"
  end

  defp svc_kind(service) do
    (Map.get(service, :kind) || Map.get(service, "kind") || "unknown")
    |> to_string()
    |> String.downcase()
  end

  defp svc_port(service) do
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

  defp has_inline_secrets?(env) when is_map(env) do
    secret_keys = ~w(password passwd secret api_key apikey token auth_token access_key secret_key private_key)

    Enum.any?(env, fn {key, value} ->
      key_lower = to_string(key) |> String.downcase()
      val_str = to_string(value)

      key_matches = Enum.any?(secret_keys, fn sk -> String.contains?(key_lower, sk) end)
      not_reference = not String.starts_with?(val_str, "${") and not String.starts_with?(val_str, "/run/secrets/")
      value_present = String.length(val_str) > 0

      key_matches and not_reference and value_present
    end)
  end

  defp has_inline_secrets?(env) when is_list(env) do
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
    |> has_inline_secrets?()
  end

  defp has_inline_secrets?(_), do: false
end
