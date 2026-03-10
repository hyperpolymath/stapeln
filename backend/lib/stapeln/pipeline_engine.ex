# SPDX-License-Identifier: PMPL-1.0-or-later
# Stapeln.PipelineEngine - Assembly pipeline validation, optimization, and execution
# planning for visual node-graph container pipelines.

defmodule Stapeln.PipelineEngine do
  @moduledoc """
  Assembly pipeline validation, optimization, and execution planning.

  A pipeline is a directed acyclic graph (DAG) of typed nodes connected
  by edges (connections). Each node represents a container build step
  (source image, run command, copy artefact, security gate, push target, etc.)
  and connections define the data/layer flow between them.

  ## Pipeline shape

      %{
        "nodes" => [
          %{"id" => "n1", "type" => "source", "config" => %{"image" => "wolfi-base:latest"}},
          %{"id" => "n2", "type" => "run",    "config" => %{"commands" => ["apk add curl"]}},
          ...
        ],
        "connections" => [
          %{"from" => "n1", "to" => "n2", "from_port" => "out", "to_port" => "in"},
          ...
        ],
        "metadata" => %{"name" => "my-pipeline", ...}
      }

  ## Node types

  | Type           | Has inputs? | Has outputs? | Purpose                         |
  |----------------|-------------|--------------|----------------------------------|
  | `source`       | no          | yes          | FROM / base image                |
  | `run`          | yes         | yes          | RUN shell command(s)             |
  | `copy`         | yes         | yes          | COPY files into image            |
  | `env`          | yes         | yes          | ENV key=value                    |
  | `expose`       | yes         | yes          | EXPOSE port                      |
  | `workdir`      | yes         | yes          | WORKDIR path                     |
  | `label`        | yes         | yes          | LABEL key=value                  |
  | `volume`       | yes         | yes          | VOLUME mount-point               |
  | `security_gate`| yes         | yes          | Security scan checkpoint         |
  | `push`         | yes         | no           | Push to registry (terminal node) |
  """

  @type node_map :: %{String.t() => term()}
  @type connection :: %{String.t() => String.t()}
  @type pipeline :: %{String.t() => term()}

  @type validation_result :: %{
          valid: boolean(),
          errors: [String.t()],
          warnings: [String.t()],
          security_score: float(),
          estimated_size: non_neg_integer()
        }

  @source_types ~w(source)
  @terminal_types ~w(push)
  @all_node_types ~w(source run copy env expose workdir label volume security_gate push)

  # ---------------------------------------------------------------------------
  # Public API
  # ---------------------------------------------------------------------------

  @doc """
  Validate a pipeline graph.

  Checks performed:
  - Pipeline is not empty (has at least one node)
  - No cycles (DAG property via topological sort)
  - All connections reference valid node IDs and ports
  - Every node has required configuration for its type
  - Source nodes have no incoming connections
  - Push nodes have no outgoing connections
  - Security gates are positioned before push nodes
  - Estimates image size and computes a security score
  """
  @spec validate(pipeline()) :: validation_result()
  def validate(pipeline) when is_map(pipeline) do
    nodes = get_nodes(pipeline)
    connections = get_connections(pipeline)
    node_index = index_nodes(nodes)

    errors = List.flatten([
      check_empty(nodes),
      check_unknown_types(nodes),
      check_duplicate_ids(nodes),
      check_connection_refs(connections, node_index),
      check_source_no_inputs(nodes, connections),
      check_push_no_outputs(nodes, connections),
      check_required_config(nodes),
      check_cycles(nodes, connections),
      check_disconnected_nodes(nodes, connections)
    ])

    warnings = List.flatten([
      check_security_gate_before_push(nodes, connections, node_index),
      check_latest_tag(nodes),
      check_missing_workdir(nodes)
    ])

    security_score = compute_security_score(nodes, connections, node_index)
    estimated_size = estimate_image_size(nodes)

    %{
      valid: errors == [],
      errors: errors,
      warnings: warnings,
      security_score: security_score,
      estimated_size: estimated_size
    }
  end

  def validate(_), do: %{valid: false, errors: ["pipeline must be a map"], warnings: [], security_score: 0.0, estimated_size: 0}

  @doc """
  Optimize a pipeline graph.

  Transformations applied:
  - Merge consecutive RUN commands into a single RUN with `&&`
  - Reorder COPY nodes for better cache behaviour (package manifests first)
  - Eliminate dead (disconnected) nodes
  """
  @spec optimize(pipeline()) :: pipeline()
  def optimize(pipeline) when is_map(pipeline) do
    pipeline
    |> eliminate_dead_nodes()
    |> merge_consecutive_runs()
    |> reorder_copies_for_cache()
  end

  def optimize(pipeline), do: pipeline

  @doc """
  Generate an execution plan from a pipeline (topological order).

  Returns a list of stages. Nodes within the same stage have no mutual
  dependencies and may run in parallel.
  """
  @spec execution_plan(pipeline()) :: {:ok, [%{stage: pos_integer(), nodes: [String.t()]}]} | {:error, String.t()}
  def execution_plan(pipeline) when is_map(pipeline) do
    nodes = get_nodes(pipeline)
    connections = get_connections(pipeline)

    case topological_sort(nodes, connections) do
      {:ok, sorted_ids} ->
        stages = group_into_stages(sorted_ids, connections)
        {:ok, stages}

      {:error, _} = err ->
        err
    end
  end

  def execution_plan(_), do: {:error, "pipeline must be a map"}

  # ---------------------------------------------------------------------------
  # Pipeline accessors (handle atom & string keys)
  # ---------------------------------------------------------------------------

  defp get_nodes(pipeline) do
    case Map.get(pipeline, "nodes", Map.get(pipeline, :nodes, [])) do
      list when is_list(list) -> list
      _ -> []
    end
  end

  defp get_connections(pipeline) do
    case Map.get(pipeline, "connections", Map.get(pipeline, :connections, [])) do
      list when is_list(list) -> list
      _ -> []
    end
  end

  defp get_metadata(pipeline) do
    Map.get(pipeline, "metadata", Map.get(pipeline, :metadata, %{}))
  end

  defp node_id(node), do: Map.get(node, "id", Map.get(node, :id, ""))
  defp node_type(node), do: Map.get(node, "type", Map.get(node, :type, ""))
  defp node_config(node), do: Map.get(node, "config", Map.get(node, :config, %{}))

  defp conn_from(c), do: Map.get(c, "from", Map.get(c, :from, ""))
  defp conn_to(c), do: Map.get(c, "to", Map.get(c, :to, ""))

  defp index_nodes(nodes) do
    Map.new(nodes, fn n -> {node_id(n), n} end)
  end

  # ---------------------------------------------------------------------------
  # Validation checks — errors
  # ---------------------------------------------------------------------------

  defp check_empty([]), do: ["pipeline has no nodes"]
  defp check_empty(_), do: []

  defp check_unknown_types(nodes) do
    nodes
    |> Enum.flat_map(fn node ->
      t = node_type(node)
      if t in @all_node_types, do: [], else: ["node #{node_id(node)} has unknown type '#{t}'"]
    end)
  end

  defp check_duplicate_ids(nodes) do
    nodes
    |> Enum.frequencies_by(&node_id/1)
    |> Enum.flat_map(fn
      {id, count} when count > 1 -> ["duplicate node id '#{id}' appears #{count} times"]
      _ -> []
    end)
  end

  defp check_connection_refs(connections, node_index) do
    connections
    |> Enum.flat_map(fn c ->
      from_id = conn_from(c)
      to_id = conn_to(c)

      from_err = if Map.has_key?(node_index, from_id), do: [], else: ["connection references unknown source node '#{from_id}'"]
      to_err = if Map.has_key?(node_index, to_id), do: [], else: ["connection references unknown target node '#{to_id}'"]

      from_err ++ to_err
    end)
  end

  defp check_source_no_inputs(nodes, connections) do
    incoming_targets = MapSet.new(connections, &conn_to/1)

    nodes
    |> Enum.flat_map(fn node ->
      if node_type(node) in @source_types and MapSet.member?(incoming_targets, node_id(node)) do
        ["source node '#{node_id(node)}' must not have incoming connections"]
      else
        []
      end
    end)
  end

  defp check_push_no_outputs(nodes, connections) do
    outgoing_sources = MapSet.new(connections, &conn_from/1)

    nodes
    |> Enum.flat_map(fn node ->
      if node_type(node) in @terminal_types and MapSet.member?(outgoing_sources, node_id(node)) do
        ["push node '#{node_id(node)}' must not have outgoing connections"]
      else
        []
      end
    end)
  end

  defp check_required_config(nodes) do
    nodes
    |> Enum.flat_map(fn node ->
      config = node_config(node)
      id = node_id(node)

      case node_type(node) do
        "source" ->
          if config_value(config, "image") == nil,
            do: ["source node '#{id}' requires 'image' in config"],
            else: []

        "run" ->
          cmds = config_value(config, "commands") || config_value(config, "command")
          if cmds == nil or cmds == [] or cmds == "",
            do: ["run node '#{id}' requires 'commands' in config"],
            else: []

        "copy" ->
          src = config_value(config, "src") || config_value(config, "source")
          dst = config_value(config, "dst") || config_value(config, "dest") || config_value(config, "destination")
          errors = []
          errors = if src == nil, do: ["copy node '#{id}' requires 'src' in config"] ++ errors, else: errors
          errors = if dst == nil, do: ["copy node '#{id}' requires 'dst' in config"] ++ errors, else: errors
          errors

        "env" ->
          key = config_value(config, "key") || config_value(config, "name")
          if key == nil,
            do: ["env node '#{id}' requires 'key' in config"],
            else: []

        "expose" ->
          port = config_value(config, "port")
          if port == nil,
            do: ["expose node '#{id}' requires 'port' in config"],
            else: []

        "workdir" ->
          path = config_value(config, "path")
          if path == nil,
            do: ["workdir node '#{id}' requires 'path' in config"],
            else: []

        "push" ->
          registry = config_value(config, "registry") || config_value(config, "target")
          if registry == nil,
            do: ["push node '#{id}' requires 'registry' or 'target' in config"],
            else: []

        _ ->
          []
      end
    end)
  end

  defp check_cycles(nodes, connections) do
    case topological_sort(nodes, connections) do
      {:ok, _} -> []
      {:error, msg} -> [msg]
    end
  end

  defp check_disconnected_nodes(nodes, connections) do
    if length(nodes) <= 1, do: [], else: do_check_disconnected(nodes, connections)
  end

  defp do_check_disconnected(nodes, connections) do
    connected_ids =
      connections
      |> Enum.flat_map(fn c -> [conn_from(c), conn_to(c)] end)
      |> MapSet.new()

    # Source nodes are allowed to be unconnected if they're the only source
    source_ids = nodes |> Enum.filter(fn n -> node_type(n) in @source_types end) |> Enum.map(&node_id/1) |> MapSet.new()

    nodes
    |> Enum.flat_map(fn node ->
      id = node_id(node)
      if not MapSet.member?(connected_ids, id) and not MapSet.member?(source_ids, id) do
        ["node '#{id}' is disconnected from the pipeline"]
      else
        []
      end
    end)
  end

  # ---------------------------------------------------------------------------
  # Validation checks — warnings
  # ---------------------------------------------------------------------------

  defp check_security_gate_before_push(nodes, connections, node_index) do
    push_nodes = Enum.filter(nodes, fn n -> node_type(n) in @terminal_types end)
    gate_ids = nodes |> Enum.filter(fn n -> node_type(n) == "security_gate" end) |> Enum.map(&node_id/1) |> MapSet.new()

    if push_nodes == [] or MapSet.size(gate_ids) > 0 do
      # Check each push node has a security gate somewhere in its ancestor chain
      push_nodes
      |> Enum.flat_map(fn push_node ->
        ancestors = collect_ancestors(node_id(push_node), connections, node_index)
        if MapSet.disjoint?(ancestors, gate_ids) and MapSet.size(gate_ids) > 0 do
          ["push node '#{node_id(push_node)}' has no security gate in its dependency chain"]
        else
          if MapSet.size(gate_ids) == 0 and push_nodes != [] do
            ["no security gate nodes in pipeline; consider adding one before push"]
          else
            []
          end
        end
      end)
      |> Enum.uniq()
    else
      []
    end
  end

  defp check_latest_tag(nodes) do
    nodes
    |> Enum.flat_map(fn node ->
      if node_type(node) == "source" do
        image = config_value(node_config(node), "image") || ""
        if String.ends_with?(image, ":latest") do
          ["source node '#{node_id(node)}' uses :latest tag — pin to a specific version for reproducibility"]
        else
          []
        end
      else
        []
      end
    end)
  end

  defp check_missing_workdir(nodes) do
    has_workdir = Enum.any?(nodes, fn n -> node_type(n) == "workdir" end)
    has_run = Enum.any?(nodes, fn n -> node_type(n) == "run" end)

    if has_run and not has_workdir do
      ["pipeline has RUN commands but no WORKDIR — consider setting a working directory"]
    else
      []
    end
  end

  # ---------------------------------------------------------------------------
  # Security score
  # ---------------------------------------------------------------------------

  defp compute_security_score(nodes, connections, node_index) do
    base_score = 50.0

    # +15 for having security gate(s)
    gate_bonus = if Enum.any?(nodes, fn n -> node_type(n) == "security_gate" end), do: 15.0, else: 0.0

    # +10 for pinned image tags (no :latest)
    source_nodes = Enum.filter(nodes, fn n -> node_type(n) == "source" end)
    pinned_count = Enum.count(source_nodes, fn n ->
      image = config_value(node_config(n), "image") || ""
      not String.ends_with?(image, ":latest") and String.contains?(image, ":")
    end)
    total_sources = max(length(source_nodes), 1)
    pin_bonus = 10.0 * (pinned_count / total_sources)

    # +10 for using Chainguard images
    chainguard_count = Enum.count(source_nodes, fn n ->
      image = config_value(node_config(n), "image") || ""
      String.contains?(image, "cgr.dev/chainguard")
    end)
    chainguard_bonus = 10.0 * (chainguard_count / total_sources)

    # +10 for non-root WORKDIR
    workdir_bonus = if Enum.any?(nodes, fn n ->
      node_type(n) == "workdir" and config_value(node_config(n), "path") not in [nil, "/", "/root"]
    end), do: 10.0, else: 0.0

    # +5 for having at least one label (traceability)
    label_bonus = if Enum.any?(nodes, fn n -> node_type(n) == "label" end), do: 5.0, else: 0.0

    score = base_score + gate_bonus + pin_bonus + chainguard_bonus + workdir_bonus + label_bonus
    min(100.0, Float.round(score, 1))
  end

  # ---------------------------------------------------------------------------
  # Image size estimation
  # ---------------------------------------------------------------------------

  defp estimate_image_size(nodes) do
    # Rough heuristics in bytes
    base_sizes = %{
      "cgr.dev/chainguard/wolfi-base" => 12_000_000,
      "cgr.dev/chainguard/static" => 2_000_000,
      "cgr.dev/chainguard/postgres" => 80_000_000,
      "cgr.dev/chainguard/redis" => 15_000_000,
      "alpine" => 7_000_000,
      "ubuntu" => 75_000_000,
      "debian" => 120_000_000,
      "node" => 350_000_000,
      "python" => 400_000_000,
      "rust" => 800_000_000,
      "golang" => 300_000_000
    }

    source_size =
      nodes
      |> Enum.filter(fn n -> node_type(n) == "source" end)
      |> Enum.map(fn n ->
        image = config_value(node_config(n), "image") || ""
        image_base = image |> String.split(":") |> List.first() |> to_string()

        Enum.find_value(base_sizes, 50_000_000, fn {pattern, size} ->
          if String.contains?(image_base, pattern), do: size, else: nil
        end)
      end)
      |> Enum.max(fn -> 50_000_000 end)

    # Each RUN command adds estimated overhead
    run_overhead =
      nodes
      |> Enum.filter(fn n -> node_type(n) == "run" end)
      |> Enum.map(fn n ->
        cmds = config_value(node_config(n), "commands") || []
        cmds = if is_list(cmds), do: cmds, else: [cmds]

        Enum.reduce(cmds, 0, fn cmd, acc ->
          cmd_str = to_string(cmd)
          cond do
            String.contains?(cmd_str, "apk add") or String.contains?(cmd_str, "apt-get install") -> acc + 20_000_000
            String.contains?(cmd_str, "npm install") or String.contains?(cmd_str, "yarn install") -> acc + 100_000_000
            String.contains?(cmd_str, "pip install") -> acc + 50_000_000
            String.contains?(cmd_str, "cargo build") -> acc + 200_000_000
            true -> acc + 1_000_000
          end
        end)
      end)
      |> Enum.sum()

    # COPY adds a flat estimate
    copy_overhead = nodes |> Enum.count(fn n -> node_type(n) == "copy" end) |> Kernel.*(5_000_000)

    source_size + run_overhead + copy_overhead
  end

  # ---------------------------------------------------------------------------
  # Optimization: eliminate dead nodes
  # ---------------------------------------------------------------------------

  defp eliminate_dead_nodes(pipeline) do
    nodes = get_nodes(pipeline)
    connections = get_connections(pipeline)

    if length(nodes) <= 1 do
      pipeline
    else
      connected_ids =
        connections
        |> Enum.flat_map(fn c -> [conn_from(c), conn_to(c)] end)
        |> MapSet.new()

      # Keep source/push nodes even if not connected, plus all connected nodes
      live_nodes = Enum.filter(nodes, fn n ->
        node_type(n) in (@source_types ++ @terminal_types) or MapSet.member?(connected_ids, node_id(n))
      end)

      put_nodes(pipeline, live_nodes)
    end
  end

  # ---------------------------------------------------------------------------
  # Optimization: merge consecutive RUN nodes
  # ---------------------------------------------------------------------------

  defp merge_consecutive_runs(pipeline) do
    nodes = get_nodes(pipeline)
    connections = get_connections(pipeline)

    case topological_sort(nodes, connections) do
      {:ok, sorted_ids} ->
        node_index = index_nodes(nodes)
        {merged_nodes, merged_connections} = do_merge_runs(sorted_ids, node_index, connections)
        pipeline |> put_nodes(merged_nodes) |> put_connections(merged_connections)

      {:error, _} ->
        # Cannot optimise a cyclic graph
        pipeline
    end
  end

  defp do_merge_runs(sorted_ids, node_index, connections) do
    # Walk sorted order. When we find consecutive run nodes (A->B where both
    # are run nodes and B has exactly one incoming connection from A), merge.
    outgoing = build_outgoing_map(connections)
    incoming = build_incoming_map(connections)

    {merged_index, removed, rewrites} =
      Enum.reduce(sorted_ids, {node_index, MapSet.new(), %{}}, fn id, {idx, removed, rewrites} ->
        if MapSet.member?(removed, id), do: {idx, removed, rewrites}, else: do_try_merge(id, idx, removed, rewrites, outgoing, incoming)
      end)

    live_nodes =
      sorted_ids
      |> Enum.reject(&MapSet.member?(removed, &1))
      |> Enum.map(&Map.fetch!(merged_index, &1))

    # Rewrite connections: replace removed node references
    live_connections =
      connections
      |> Enum.map(fn c ->
        from = Map.get(rewrites, conn_from(c), conn_from(c))
        to = Map.get(rewrites, conn_to(c), conn_to(c))
        c |> Map.put("from", from) |> Map.put("to", to)
      end)
      |> Enum.reject(fn c -> conn_from(c) == conn_to(c) end)
      |> Enum.uniq_by(fn c -> {conn_from(c), conn_to(c)} end)

    {live_nodes, live_connections}
  end

  defp do_try_merge(id, node_index, removed, rewrites, outgoing, incoming) do
    node = Map.get(node_index, id)

    if node_type(node) != "run" do
      {node_index, removed, rewrites}
    else
      # Look for a single successor that is also a run node with exactly one input
      successors = Map.get(outgoing, id, [])

      case successors do
        [next_id] ->
          next_node = Map.get(node_index, next_id)
          next_inputs = Map.get(incoming, next_id, [])

          if next_node != nil and node_type(next_node) == "run" and length(next_inputs) == 1 do
            # Merge next_node commands into current node
            my_cmds = get_run_commands(node)
            their_cmds = get_run_commands(next_node)
            merged_cmds = my_cmds ++ their_cmds

            merged_config = node_config(node) |> Map.put("commands", merged_cmds)
            merged_node = node |> Map.put("config", merged_config) |> Map.put(:config, merged_config)
            updated_index = Map.put(node_index, id, merged_node)

            {updated_index, MapSet.put(removed, next_id), Map.put(rewrites, next_id, id)}
          else
            {node_index, removed, rewrites}
          end

        _ ->
          {node_index, removed, rewrites}
      end
    end
  end

  defp get_run_commands(node) do
    config = node_config(node)
    cmds = config_value(config, "commands") || config_value(config, "command") || []
    if is_list(cmds), do: cmds, else: [cmds]
  end

  # ---------------------------------------------------------------------------
  # Optimization: reorder COPYs for cache (package manifests first)
  # ---------------------------------------------------------------------------

  defp reorder_copies_for_cache(pipeline) do
    nodes = get_nodes(pipeline)

    # Identify COPY nodes whose source is a package manifest
    manifest_patterns = ~w(package.json package-lock.json yarn.lock Gemfile Gemfile.lock
                           requirements.txt Pipfile Pipfile.lock go.mod go.sum Cargo.toml
                           Cargo.lock mix.exs mix.lock gleam.toml rescript.json bsconfig.json)

    updated_nodes =
      Enum.map(nodes, fn node ->
        if node_type(node) == "copy" do
          src = config_value(node_config(node), "src") || ""
          is_manifest = Enum.any?(manifest_patterns, fn pat -> String.contains?(src, pat) end)
          # Tag with priority for downstream sort (lower = earlier)
          Map.put(node, "__cache_priority", if(is_manifest, do: 0, else: 1))
        else
          Map.put(node, "__cache_priority", 0)
        end
      end)
      |> Enum.sort_by(fn n -> Map.get(n, "__cache_priority", 0) end)
      |> Enum.map(&Map.delete(&1, "__cache_priority"))

    put_nodes(pipeline, updated_nodes)
  end

  # ---------------------------------------------------------------------------
  # Topological sort (Kahn's algorithm)
  # ---------------------------------------------------------------------------

  @doc false
  def topological_sort(nodes, connections) do
    node_ids = MapSet.new(nodes, &node_id/1)

    # Build in-degree map
    in_degree = Map.new(nodes, fn n -> {node_id(n), 0} end)

    in_degree =
      Enum.reduce(connections, in_degree, fn c, acc ->
        to = conn_to(c)
        if Map.has_key?(acc, to), do: Map.update!(acc, to, &(&1 + 1)), else: acc
      end)

    outgoing = build_outgoing_map(connections)

    # Start with zero in-degree nodes
    queue = in_degree |> Enum.filter(fn {_, deg} -> deg == 0 end) |> Enum.map(fn {id, _} -> id end)

    do_kahn(queue, in_degree, outgoing, [])
  end

  defp do_kahn([], in_degree, _outgoing, sorted) do
    remaining = Enum.count(in_degree, fn {_, deg} -> deg > 0 end)

    if remaining > 0 do
      {:error, "pipeline contains a cycle — #{remaining} node(s) involved"}
    else
      {:ok, Enum.reverse(sorted)}
    end
  end

  defp do_kahn([current | rest], in_degree, outgoing, sorted) do
    neighbours = Map.get(outgoing, current, [])

    {updated_degree, new_zero} =
      Enum.reduce(neighbours, {in_degree, []}, fn neighbour, {deg, zeros} ->
        new_deg = Map.update!(deg, neighbour, &(&1 - 1))

        if Map.get(new_deg, neighbour) == 0 do
          {new_deg, [neighbour | zeros]}
        else
          {new_deg, zeros}
        end
      end)

    updated_degree = Map.put(updated_degree, current, -1)
    do_kahn(rest ++ new_zero, updated_degree, outgoing, [current | sorted])
  end

  # ---------------------------------------------------------------------------
  # Stage grouping (for parallel execution)
  # ---------------------------------------------------------------------------

  defp group_into_stages(sorted_ids, connections) do
    incoming = build_incoming_map(connections)

    # Assign each node to the earliest stage possible
    {_stage_map, stages} =
      Enum.reduce(sorted_ids, {%{}, []}, fn id, {stage_map, stages} ->
        deps = Map.get(incoming, id, [])

        my_stage =
          if deps == [] do
            1
          else
            max_dep_stage = deps |> Enum.map(fn d -> Map.get(stage_map, d, 0) end) |> Enum.max()
            max_dep_stage + 1
          end

        updated_map = Map.put(stage_map, id, my_stage)
        updated_stages = ensure_stage(stages, my_stage, id)
        {updated_map, updated_stages}
      end)

    stages
    |> Enum.with_index(1)
    |> Enum.map(fn {node_ids, idx} -> %{stage: idx, nodes: node_ids} end)
  end

  defp ensure_stage(stages, stage_num, node_id) do
    # Pad the list if needed, then append node_id to the correct stage
    padded = if length(stages) < stage_num, do: stages ++ List.duplicate([], stage_num - length(stages)), else: stages
    List.update_at(padded, stage_num - 1, fn existing -> existing ++ [node_id] end)
  end

  # ---------------------------------------------------------------------------
  # Ancestor collection (for security gate check)
  # ---------------------------------------------------------------------------

  defp collect_ancestors(node_id, connections, _node_index) do
    incoming = build_incoming_map(connections)
    do_collect_ancestors([node_id], incoming, MapSet.new())
  end

  defp do_collect_ancestors([], _incoming, visited), do: visited

  defp do_collect_ancestors([current | rest], incoming, visited) do
    if MapSet.member?(visited, current) do
      do_collect_ancestors(rest, incoming, visited)
    else
      parents = Map.get(incoming, current, [])
      do_collect_ancestors(rest ++ parents, incoming, MapSet.put(visited, current))
    end
  end

  # ---------------------------------------------------------------------------
  # Graph helpers
  # ---------------------------------------------------------------------------

  defp build_outgoing_map(connections) do
    Enum.reduce(connections, %{}, fn c, acc ->
      Map.update(acc, conn_from(c), [conn_to(c)], fn existing -> existing ++ [conn_to(c)] end)
    end)
  end

  defp build_incoming_map(connections) do
    Enum.reduce(connections, %{}, fn c, acc ->
      Map.update(acc, conn_to(c), [conn_from(c)], fn existing -> existing ++ [conn_from(c)] end)
    end)
  end

  # ---------------------------------------------------------------------------
  # Config helpers
  # ---------------------------------------------------------------------------

  defp config_value(config, key) when is_map(config) do
    Map.get(config, key, Map.get(config, String.to_atom(key), nil))
  end

  defp config_value(_, _), do: nil

  # ---------------------------------------------------------------------------
  # Pipeline mutation helpers
  # ---------------------------------------------------------------------------

  defp put_nodes(pipeline, nodes) do
    cond do
      Map.has_key?(pipeline, "nodes") -> Map.put(pipeline, "nodes", nodes)
      Map.has_key?(pipeline, :nodes) -> Map.put(pipeline, :nodes, nodes)
      true -> Map.put(pipeline, "nodes", nodes)
    end
  end

  defp put_connections(pipeline, connections) do
    cond do
      Map.has_key?(pipeline, "connections") -> Map.put(pipeline, "connections", connections)
      Map.has_key?(pipeline, :connections) -> Map.put(pipeline, :connections, connections)
      true -> Map.put(pipeline, "connections", connections)
    end
  end
end
