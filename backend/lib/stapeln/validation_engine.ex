# SPDX-License-Identifier: PMPL-1.0-or-later
# Stapeln.ValidationEngine - Comprehensive stack validation with structural,
# security, and resource checks.

defmodule Stapeln.ValidationEngine do
  @moduledoc """
  Comprehensive validation rules for container stack definitions.

  Checks performed:
  - Empty services
  - Duplicate service names
  - Missing / empty kind
  - Port conflict detection
  - Port range validation (1..65535)
  - Privileged port warnings (< 1024)
  - Circular dependency detection
  - Resource limit validation (CPU, memory)
  - Image tag validation (no :latest in production)
  - Network segmentation checks
  - Volume mount path validation
  - Environment variable hygiene (leaked secrets)
  """

  @type finding :: %{
          id: String.t(),
          severity: :low | :medium | :high | :critical,
          message: String.t(),
          hint: String.t()
        }

  @type report :: %{
          score: non_neg_integer(),
          findings: [finding()]
        }

  @spec validate(map()) :: report()
  def validate(stack) when is_map(stack) do
    services = get_services(stack)

    findings =
      List.flatten([
        empty_services_finding(services),
        duplicate_names_findings(services),
        missing_kind_findings(services),
        port_conflict_findings(services),
        port_range_findings(services),
        privileged_port_findings(services),
        circular_dependency_findings(services),
        resource_limit_findings(services),
        image_tag_findings(services),
        network_segmentation_findings(stack, services),
        volume_path_findings(services),
        env_secret_findings(services)
      ])

    %{
      score: score(findings),
      findings: findings
    }
  end

  # ---------------------------------------------------------------------------
  # Service accessor (handles atom & string keys)
  # ---------------------------------------------------------------------------

  defp get_services(stack) do
    case Map.get(stack, :services, Map.get(stack, "services", [])) do
      list when is_list(list) -> list
      _ -> []
    end
  end

  defp svc_name(svc), do: Map.get(svc, :name, Map.get(svc, "name", ""))
  defp svc_kind(svc), do: Map.get(svc, :kind, Map.get(svc, "kind", nil))

  defp svc_port(svc) do
    case Map.get(svc, :port, Map.get(svc, "port", nil)) do
      p when is_integer(p) ->
        p

      p when is_binary(p) ->
        case Integer.parse(p) do
          {n, ""} -> n
          _ -> 0
        end

      _ ->
        0
    end
  end

  defp svc_image(svc), do: Map.get(svc, :image, Map.get(svc, "image", ""))
  defp svc_cpu(svc), do: Map.get(svc, :cpu, Map.get(svc, "cpu", nil))
  defp svc_memory(svc), do: Map.get(svc, :memory, Map.get(svc, "memory", nil))
  defp svc_depends(svc), do: Map.get(svc, :depends_on, Map.get(svc, "depends_on", []))
  defp svc_volumes(svc), do: Map.get(svc, :volumes, Map.get(svc, "volumes", []))

  defp svc_env(svc) do
    case Map.get(svc, :env, Map.get(svc, "env", [])) do
      env when is_map(env) -> Enum.to_list(env)
      env when is_list(env) -> env
      _ -> []
    end
  end

  defp svc_network(svc), do: Map.get(svc, :network, Map.get(svc, "network", nil))

  # ---------------------------------------------------------------------------
  # Check: empty services
  # ---------------------------------------------------------------------------

  defp empty_services_finding([]) do
    [
      %{
        id: "services.empty",
        severity: :high,
        message: "Stack has no services configured.",
        hint: "Define at least one service before simulation or deployment."
      }
    ]
  end

  defp empty_services_finding(_), do: []

  # ---------------------------------------------------------------------------
  # Check: duplicate service names
  # ---------------------------------------------------------------------------

  defp duplicate_names_findings(services) do
    services
    |> Enum.frequencies_by(&svc_name/1)
    |> Enum.filter(fn {name, count} -> name != "" and count > 1 end)
    |> Enum.map(fn {name, count} ->
      %{
        id: "services.duplicate_name.#{name}",
        severity: :high,
        message: "Service name `#{name}` appears #{count} times.",
        hint: "Give each service a unique name to avoid rule collisions."
      }
    end)
  end

  # ---------------------------------------------------------------------------
  # Check: missing or empty kind
  # ---------------------------------------------------------------------------

  defp missing_kind_findings(services) do
    services
    |> Enum.with_index(1)
    |> Enum.flat_map(fn {service, index} ->
      case svc_kind(service) do
        nil ->
          [
            %{
              id: "services.kind_missing.#{index}",
              severity: :medium,
              message: "Service ##{index} has no kind.",
              hint: "Set `kind` (for example `web`, `worker`, or `db`)."
            }
          ]

        "" ->
          [
            %{
              id: "services.kind_empty.#{index}",
              severity: :medium,
              message: "Service ##{index} has an empty kind.",
              hint: "Set a non-empty `kind` value."
            }
          ]

        _ ->
          []
      end
    end)
  end

  # ---------------------------------------------------------------------------
  # Check: port conflicts (two services sharing the same host port)
  # ---------------------------------------------------------------------------

  defp port_conflict_findings(services) do
    services
    |> Enum.map(fn svc -> {svc_name(svc), svc_port(svc)} end)
    |> Enum.filter(fn {_name, port} -> port > 0 end)
    |> Enum.group_by(fn {_name, port} -> port end)
    |> Enum.filter(fn {_port, users} -> length(users) > 1 end)
    |> Enum.map(fn {port, users} ->
      names = Enum.map_join(users, ", ", fn {name, _} -> name end)

      %{
        id: "services.port_conflict.#{port}",
        severity: :high,
        message: "Port #{port} is used by multiple services: #{names}.",
        hint: "Assign a unique host port to each service to prevent binding conflicts."
      }
    end)
  end

  # ---------------------------------------------------------------------------
  # Check: port range (1..65535)
  # ---------------------------------------------------------------------------

  defp port_range_findings(services) do
    services
    |> Enum.with_index(1)
    |> Enum.flat_map(fn {svc, idx} ->
      port = svc_port(svc)

      if port != 0 and (port < 1 or port > 65_535) do
        [
          %{
            id: "services.invalid_port.#{idx}",
            severity: :high,
            message: "Service ##{idx} (#{svc_name(svc)}) has invalid port #{port}.",
            hint: "Use a TCP/UDP port in range 1..65535."
          }
        ]
      else
        []
      end
    end)
  end

  # ---------------------------------------------------------------------------
  # Check: privileged ports (< 1024)
  # ---------------------------------------------------------------------------

  defp privileged_port_findings(services) do
    services
    |> Enum.with_index(1)
    |> Enum.flat_map(fn {svc, idx} ->
      port = svc_port(svc)

      if port > 0 and port < 1024 do
        [
          %{
            id: "services.privileged_port.#{idx}",
            severity: :low,
            message: "Service ##{idx} (#{svc_name(svc)}) uses privileged port #{port}.",
            hint: "Ports below 1024 require elevated privileges. Map to a higher port if running rootless."
          }
        ]
      else
        []
      end
    end)
  end

  # ---------------------------------------------------------------------------
  # Check: circular dependencies
  # ---------------------------------------------------------------------------

  defp circular_dependency_findings(services) do
    # Build adjacency list
    deps_map =
      services
      |> Enum.reduce(%{}, fn svc, acc ->
        name = svc_name(svc)

        depends =
          case svc_depends(svc) do
            list when is_list(list) -> list
            _ -> []
          end

        Map.put(acc, name, depends)
      end)

    # DFS cycle detection
    names = Map.keys(deps_map)

    cycles =
      Enum.reduce(names, [], fn start, found ->
        case find_cycle(deps_map, start, [start], MapSet.new([start])) do
          nil -> found
          cycle -> [cycle | found]
        end
      end)
      |> Enum.uniq_by(&Enum.sort/1)

    Enum.map(cycles, fn cycle ->
      path = Enum.join(cycle, " -> ")

      %{
        id: "services.circular_dependency",
        severity: :critical,
        message: "Circular dependency detected: #{path}.",
        hint: "Break the dependency cycle so services can start in a deterministic order."
      }
    end)
  end

  defp find_cycle(deps_map, current, path, visited) do
    neighbours = Map.get(deps_map, current, [])

    Enum.find_value(neighbours, fn neighbour ->
      cond do
        neighbour == List.last(path) and length(path) > 1 ->
          # Found cycle back to start
          path ++ [neighbour]

        MapSet.member?(visited, neighbour) ->
          nil

        true ->
          find_cycle(deps_map, neighbour, path ++ [neighbour], MapSet.put(visited, neighbour))
      end
    end)
  end

  # ---------------------------------------------------------------------------
  # Check: resource limits
  # ---------------------------------------------------------------------------

  defp resource_limit_findings(services) do
    services
    |> Enum.with_index(1)
    |> Enum.flat_map(fn {svc, idx} ->
      name = svc_name(svc)
      cpu = svc_cpu(svc)
      memory = svc_memory(svc)

      cpu_findings =
        cond do
          is_nil(cpu) ->
            [
              %{
                id: "services.no_cpu_limit.#{idx}",
                severity: :medium,
                message: "Service ##{idx} (#{name}) has no CPU limit set.",
                hint: "Set a CPU limit to prevent resource starvation."
              }
            ]

          is_number(cpu) and cpu > 16 ->
            [
              %{
                id: "services.excessive_cpu.#{idx}",
                severity: :low,
                message: "Service ##{idx} (#{name}) requests #{cpu} CPU cores (unusually high).",
                hint: "Verify this CPU allocation is intentional."
              }
            ]

          true ->
            []
        end

      memory_findings =
        cond do
          is_nil(memory) ->
            [
              %{
                id: "services.no_memory_limit.#{idx}",
                severity: :medium,
                message: "Service ##{idx} (#{name}) has no memory limit set.",
                hint: "Set a memory limit to prevent OOM kills of other services."
              }
            ]

          is_number(memory) and memory > 32_768 ->
            [
              %{
                id: "services.excessive_memory.#{idx}",
                severity: :low,
                message: "Service ##{idx} (#{name}) requests #{memory} MB memory (unusually high).",
                hint: "Verify this memory allocation is intentional."
              }
            ]

          true ->
            []
        end

      cpu_findings ++ memory_findings
    end)
  end

  # ---------------------------------------------------------------------------
  # Check: image tag validation (no :latest in production)
  # ---------------------------------------------------------------------------

  defp image_tag_findings(services) do
    services
    |> Enum.with_index(1)
    |> Enum.flat_map(fn {svc, idx} ->
      image = svc_image(svc)

      cond do
        image == "" ->
          [
            %{
              id: "services.no_image.#{idx}",
              severity: :medium,
              message: "Service ##{idx} (#{svc_name(svc)}) has no image specified.",
              hint: "Specify a container image for the service."
            }
          ]

        String.ends_with?(image, ":latest") ->
          [
            %{
              id: "services.latest_tag.#{idx}",
              severity: :medium,
              message: "Service ##{idx} (#{svc_name(svc)}) uses :latest tag on image `#{image}`.",
              hint: "Pin to a specific image digest or version tag for reproducible builds."
            }
          ]

        not String.contains?(image, ":") ->
          [
            %{
              id: "services.no_tag.#{idx}",
              severity: :low,
              message: "Service ##{idx} (#{svc_name(svc)}) image `#{image}` has no explicit tag.",
              hint: "Add a version tag (e.g. `:3.19`) to pin the image version."
            }
          ]

        true ->
          []
      end
    end)
  end

  # ---------------------------------------------------------------------------
  # Check: network segmentation
  # ---------------------------------------------------------------------------

  defp network_segmentation_findings(stack, services) do
    # Check that services meant for different tiers are on separate networks
    networks =
      services
      |> Enum.map(fn svc -> {svc_name(svc), svc_kind(svc), svc_network(svc)} end)

    # Warn if db and web services share the same network (or no network specified)
    db_services = Enum.filter(networks, fn {_, kind, _} -> kind in ["db", "database", "redis"] end)
    web_services = Enum.filter(networks, fn {_, kind, _} -> kind in ["web", "frontend", "api"] end)

    if db_services != [] and web_services != [] do
      db_nets = Enum.map(db_services, fn {_, _, net} -> net end) |> Enum.uniq()
      web_nets = Enum.map(web_services, fn {_, _, net} -> net end) |> Enum.uniq()

      shared = Enum.filter(db_nets, fn net -> net in web_nets end)

      cond do
        Enum.all?(db_nets ++ web_nets, &is_nil/1) ->
          [
            %{
              id: "network.no_segmentation",
              severity: :medium,
              message: "Database and web services have no network isolation configured.",
              hint: "Place database services on an internal network separate from web-facing services."
            }
          ]

        shared != [] and shared != [nil] ->
          [
            %{
              id: "network.shared_tier",
              severity: :medium,
              message: "Database and web services share the same network.",
              hint: "Use separate networks for database and web tiers (e.g. `internal` and `external`)."
            }
          ]

        true ->
          []
      end
    else
      # Check if any stack-level networks are defined
      stack_networks = Map.get(stack, :networks, Map.get(stack, "networks", nil))

      if is_nil(stack_networks) and length(services) > 1 do
        [
          %{
            id: "network.no_networks_defined",
            severity: :low,
            message: "No explicit networks defined for multi-service stack.",
            hint: "Define named networks to control service communication boundaries."
          }
        ]
      else
        []
      end
    end
  end

  # ---------------------------------------------------------------------------
  # Check: volume mount paths
  # ---------------------------------------------------------------------------

  defp volume_path_findings(services) do
    dangerous_paths = ["/", "/etc", "/var", "/usr", "/bin", "/sbin", "/root", "/proc", "/sys"]

    services
    |> Enum.with_index(1)
    |> Enum.flat_map(fn {svc, idx} ->
      volumes = svc_volumes(svc)

      Enum.flat_map(volumes, fn vol ->
        mount_target =
          case String.split(to_string(vol), ":") do
            [_, target | _] -> target
            _ -> ""
          end

        if mount_target in dangerous_paths do
          [
            %{
              id: "services.dangerous_mount.#{idx}",
              severity: :high,
              message:
                "Service ##{idx} (#{svc_name(svc)}) mounts sensitive path `#{mount_target}`.",
              hint: "Avoid mounting system-critical paths; use specific subdirectories."
            }
          ]
        else
          []
        end
      end)
    end)
  end

  # ---------------------------------------------------------------------------
  # Check: environment variable hygiene (potential secrets)
  # ---------------------------------------------------------------------------

  defp env_secret_findings(services) do
    secret_patterns = ~w(PASSWORD SECRET TOKEN KEY APIKEY API_KEY CREDENTIALS PRIVATE_KEY)

    services
    |> Enum.with_index(1)
    |> Enum.flat_map(fn {svc, idx} ->
      env = svc_env(svc)

      Enum.flat_map(env, fn item ->
        key =
          case item do
            {k, _v} -> to_string(k)
            str when is_binary(str) -> str |> String.split("=") |> List.first()
            _ -> ""
          end

        key_upper = String.upcase(key)

        if Enum.any?(secret_patterns, fn pat -> String.contains?(key_upper, pat) end) do
          [
            %{
              id: "services.hardcoded_secret.#{idx}.#{key}",
              severity: :high,
              message:
                "Service ##{idx} (#{svc_name(svc)}) has potential secret in env var `#{key}`.",
              hint: "Use a secrets manager or external file instead of hardcoding secrets."
            }
          ]
        else
          []
        end
      end)
    end)
  end

  # ---------------------------------------------------------------------------
  # Scoring
  # ---------------------------------------------------------------------------

  defp score(findings) do
    penalty =
      Enum.reduce(findings, 0, fn finding, acc ->
        acc +
          case finding.severity do
            :critical -> 30
            :high -> 20
            :medium -> 10
            :low -> 5
          end
      end)

    max(0, 100 - penalty)
  end
end
