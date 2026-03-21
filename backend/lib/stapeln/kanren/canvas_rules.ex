# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# canvas_rules.ex — miniKanren rules for the visual canvas (Build-a-Container Workshop).
#
# These rules validate drag-and-drop compositions in real-time.
# Every time the user changes the canvas, the engine re-evaluates
# and updates the security score + gap analysis sidebar.

defmodule Stapeln.Kanren.CanvasRules do
  @moduledoc """
  Relational rules for validating visual container stack compositions.

  Called by the canvas interaction layer whenever a component is added,
  connected, or configured. Returns findings for the gap analysis sidebar.
  """

  alias Stapeln.Kanren.Core

  # ---------------------------------------------------------------------------
  # Port conflict detection
  # ---------------------------------------------------------------------------

  @doc """
  Relation: two services binding the same host port is a conflict.
  port_conflict(service_a, service_b, port, severity, message)
  """
  def port_conflict(svc_a, svc_b, port, severity, message) do
    Core.all([
      Core.fresh(2, fn host_port_a, host_port_b ->
        Core.all([
          Core.eq({:host_port, svc_a}, {:host_port, host_port_a}),
          Core.eq({:host_port, svc_b}, {:host_port, host_port_b}),
          Core.eq(host_port_a, port),
          Core.eq(host_port_b, port),
          Core.eq(severity, :critical),
          Core.eq(message, "Port conflict: both services bind the same host port")
        ])
      end)
    ])
  end

  @doc """
  Check all service pairs for port conflicts.
  Returns list of {service_a, service_b, port, severity, message}.
  """
  def check_port_conflicts(services) when is_list(services) do
    # Extract host ports from service definitions
    port_map =
      services
      |> Enum.flat_map(fn svc ->
        name = Map.get(svc, :name, Map.get(svc, "name", "unknown"))
        ports = Map.get(svc, :ports, Map.get(svc, "ports", []))

        Enum.map(ports, fn port_binding ->
          host_port = extract_host_port(port_binding)
          {name, host_port}
        end)
      end)

    # Find conflicts (same host port, different services)
    for {name_a, port_a} <- port_map,
        {name_b, port_b} <- port_map,
        name_a < name_b,
        port_a == port_b,
        port_a != nil do
      %{
        type: :port_conflict,
        service_a: name_a,
        service_b: name_b,
        port: port_a,
        severity: :critical,
        message: "Port #{port_a} bound by both #{name_a} and #{name_b}",
        auto_fix: {:reassign_port, name_b, suggest_free_port(port_a, port_map)},
        source: :kanren
      }
    end
  end

  # ---------------------------------------------------------------------------
  # Network isolation verification
  # ---------------------------------------------------------------------------

  @doc """
  Relation: a database should not be on the same network as a public-facing service
  unless explicitly allowed.
  vuln_network_exposure(db_service, web_service, network, severity, message)
  """
  def vuln_network_exposure(db_svc, web_svc, network, severity, message) do
    Core.all([
      Core.fresh(2, fn db_net, web_net ->
        Core.all([
          Core.eq({:network, db_svc}, {:network, db_net}),
          Core.eq({:network, web_svc}, {:network, web_net}),
          Core.eq(db_net, network),
          Core.eq(web_net, network),
          Core.eq(severity, :high),
          Core.eq(
            message,
            "Database and web service on same network without isolation"
          )
        ])
      end)
    ])
  end

  @doc """
  Check that databases are isolated from public-facing services.
  """
  def check_network_isolation(services) when is_list(services) do
    db_services = Enum.filter(services, &is_database?/1)
    web_services = Enum.filter(services, &is_web_facing?/1)

    for db <- db_services,
        web <- web_services,
        shared_network = shared_networks(db, web),
        shared_network != [] do
      db_name = Map.get(db, :name, Map.get(db, "name", "db"))
      web_name = Map.get(web, :name, Map.get(web, "name", "web"))

      %{
        type: :network_exposure,
        service_a: db_name,
        service_b: web_name,
        networks: shared_network,
        severity: :high,
        message: "#{db_name} shares network with #{web_name} — add an internal network",
        auto_fix: {:create_internal_network, db_name},
        source: :kanren
      }
    end
  end

  # ---------------------------------------------------------------------------
  # Dependency ordering validation
  # ---------------------------------------------------------------------------

  @doc """
  Check that dependency chains are acyclic and all referenced services exist.
  """
  def check_dependencies(services) when is_list(services) do
    service_names = MapSet.new(Enum.map(services, &service_name/1))

    missing_deps =
      services
      |> Enum.flat_map(fn svc ->
        name = service_name(svc)
        deps = Map.get(svc, :depends_on, Map.get(svc, "depends_on", []))

        deps
        |> Enum.reject(&MapSet.member?(service_names, &1))
        |> Enum.map(fn missing ->
          %{
            type: :missing_dependency,
            service: name,
            dependency: missing,
            severity: :critical,
            message: "#{name} depends on #{missing} which is not in the stack",
            auto_fix: {:add_service, missing},
            source: :kanren
          }
        end)
      end)

    cycles = detect_cycles(services)

    cycle_findings =
      Enum.map(cycles, fn cycle ->
        %{
          type: :circular_dependency,
          services: cycle,
          severity: :critical,
          message: "Circular dependency: #{Enum.join(cycle, " → ")}",
          auto_fix: nil,
          source: :kanren
        }
      end)

    missing_deps ++ cycle_findings
  end

  # ---------------------------------------------------------------------------
  # Volume mount validation
  # ---------------------------------------------------------------------------

  @doc """
  Check that volume mounts don't expose sensitive host paths.
  """
  def check_volume_safety(services) when is_list(services) do
    sensitive_paths = ["/", "/etc", "/var", "/root", "/home", "/proc", "/sys", "/dev"]

    services
    |> Enum.flat_map(fn svc ->
      name = service_name(svc)
      volumes = Map.get(svc, :volumes, Map.get(svc, "volumes", []))

      volumes
      |> Enum.filter(fn vol ->
        host_path = extract_host_path(vol)
        host_path != nil and host_path in sensitive_paths
      end)
      |> Enum.map(fn vol ->
        %{
          type: :sensitive_volume,
          service: name,
          volume: vol,
          severity: :critical,
          message: "#{name} mounts sensitive host path: #{extract_host_path(vol)}",
          auto_fix: {:restrict_volume, name, vol},
          source: :kanren
        }
      end)
    end)
  end

  # ---------------------------------------------------------------------------
  # Comprehensive canvas validation (called on every canvas change)
  # ---------------------------------------------------------------------------

  @doc """
  Run all canvas validation rules against a stack definition.
  Returns a map with findings, score, and auto-fix suggestions.

  This is the main entry point called by the visual canvas whenever
  the user drags, drops, connects, or configures a component.
  """
  @spec validate_canvas(map()) :: map()
  def validate_canvas(stack) when is_map(stack) do
    services = Map.get(stack, :services, Map.get(stack, "services", []))

    port_findings = check_port_conflicts(services)
    network_findings = check_network_isolation(services)
    dep_findings = check_dependencies(services)
    volume_findings = check_volume_safety(services)

    all_findings = port_findings ++ network_findings ++ dep_findings ++ volume_findings

    # Compute security score (100 = perfect, deductions per severity)
    deductions =
      all_findings
      |> Enum.map(fn f ->
        case f.severity do
          :critical -> 25
          :high -> 15
          :medium -> 8
          :low -> 3
          _ -> 0
        end
      end)
      |> Enum.sum()

    score = max(0, 100 - deductions)

    %{
      score: score,
      findings: all_findings,
      total_findings: length(all_findings),
      auto_fixable: Enum.count(all_findings, fn f -> f.auto_fix != nil end),
      severity_distribution: %{
        critical: Enum.count(all_findings, &(&1.severity == :critical)),
        high: Enum.count(all_findings, &(&1.severity == :high)),
        medium: Enum.count(all_findings, &(&1.severity == :medium)),
        low: Enum.count(all_findings, &(&1.severity == :low))
      }
    }
  end

  # ---------------------------------------------------------------------------
  # Helpers
  # ---------------------------------------------------------------------------

  defp extract_host_port(binding) when is_binary(binding) do
    case String.split(binding, ":") do
      [host_port, _container_port] ->
        case Integer.parse(host_port) do
          {port, _} -> port
          :error -> nil
        end

      _ ->
        nil
    end
  end

  defp extract_host_port(_), do: nil

  defp extract_host_path(vol) when is_binary(vol) do
    case String.split(vol, ":") do
      [host_path, _container_path | _] -> host_path
      _ -> nil
    end
  end

  defp extract_host_path(_), do: nil

  defp suggest_free_port(conflicting_port, port_map) do
    used = MapSet.new(Enum.map(port_map, fn {_, p} -> p end))
    # Try port + 1, + 2, etc.
    Enum.find(1..100, fn offset ->
      not MapSet.member?(used, conflicting_port + offset)
    end)
    |> case do
      nil -> conflicting_port + 1000
      offset -> conflicting_port + offset
    end
  end

  defp is_database?(svc) do
    image = Map.get(svc, :image, Map.get(svc, "image", ""))
    name = Map.get(svc, :name, Map.get(svc, "name", ""))

    Enum.any?(
      ["postgres", "mysql", "mariadb", "mongo", "redis", "memcached", "clickhouse", "influx"],
      fn db -> String.contains?(image, db) or String.contains?(name, db) end
    )
  end

  defp is_web_facing?(svc) do
    ports = Map.get(svc, :ports, Map.get(svc, "ports", []))
    # If it exposes port 80, 443, 8080, or 3000, it's web-facing
    Enum.any?(ports, fn p ->
      host_port = extract_host_port(p)
      host_port in [80, 443, 8080, 8443, 3000]
    end)
  end

  defp shared_networks(svc_a, svc_b) do
    nets_a = MapSet.new(Map.get(svc_a, :networks, Map.get(svc_a, "networks", [])))
    nets_b = MapSet.new(Map.get(svc_b, :networks, Map.get(svc_b, "networks", [])))
    MapSet.intersection(nets_a, nets_b) |> MapSet.to_list()
  end

  defp service_name(svc), do: Map.get(svc, :name, Map.get(svc, "name", "unknown"))

  defp detect_cycles(services) do
    # Build adjacency list
    graph =
      services
      |> Enum.reduce(%{}, fn svc, acc ->
        name = service_name(svc)
        deps = Map.get(svc, :depends_on, Map.get(svc, "depends_on", []))
        Map.put(acc, name, deps)
      end)

    # DFS cycle detection
    graph
    |> Map.keys()
    |> Enum.reduce([], fn node, cycles ->
      case dfs_cycle(node, graph, [], MapSet.new()) do
        nil -> cycles
        cycle -> [cycle | cycles]
      end
    end)
  end

  defp dfs_cycle(node, graph, path, visited) do
    if node in path do
      # Found a cycle — return the cycle portion of the path
      cycle_start = Enum.find_index(path, &(&1 == node))
      Enum.slice(path, cycle_start..-1//1) ++ [node]
    else
      if MapSet.member?(visited, node) do
        nil
      else
        deps = Map.get(graph, node, [])

        Enum.find_value(deps, fn dep ->
          dfs_cycle(dep, graph, path ++ [node], MapSet.put(visited, node))
        end)
      end
    end
  end
end
