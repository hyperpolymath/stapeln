# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Vordr.Borg do
  @moduledoc """
  Borg integration layer for container runtime operations.

  Borg provides the low-level container runtime interface, handling:
  - Image pulling and management
  - Container creation and lifecycle
  - Network namespace configuration
  - Resource limit enforcement

  This module wraps Borg's functionality to integrate with Vörðr's
  verified orchestration layer.

  ## Runtime Mode

  When the MCP server is available (`:use_mcp_runtime` config is true),
  operations are delegated to the Rust runtime via MCP HTTP calls.
  Otherwise, stub implementations are used for development/testing.
  """

  require Logger

  alias Vordr.McpClient

  @type image_ref :: String.t()
  @type container_id :: String.t()
  @type oci_config :: map()

  @doc """
  Pull a container image from a registry.

  ## Options
  - `:registry` - Override default registry
  - `:credentials` - Registry credentials
  - `:platform` - Target platform (default: current)
  """
  @spec pull_image(image_ref(), keyword()) :: {:ok, map()} | {:error, term()}
  def pull_image(image_ref, _opts \\ []) do
    Logger.info("Pulling image: #{image_ref}")

    # Stub implementation - would call borg CLI or library
    case parse_image_ref(image_ref) do
      {:ok, _parsed} ->
        # Simulate image pull
        {:ok, %{
          ref: image_ref,
          digest: generate_digest(),
          size: :rand.uniform(500_000_000),
          created: DateTime.utc_now(),
          config: %{
            "Env" => [],
            "Cmd" => ["/bin/sh"],
            "WorkingDir" => "/",
            "Labels" => %{}
          }
        }}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Create a container from an image.

  ## Options
  - `:name` - Container name
  - `:command` - Override default command
  - `:env` - Environment variables
  - `:mounts` - Volume mounts
  - `:network` - Network configuration
  - `:resources` - Resource limits
  """
  @spec create_container(image_ref(), oci_config(), keyword()) ::
          {:ok, container_id()} | {:error, term()}
  def create_container(image_ref, config, opts \\ []) do
    name = Keyword.get(opts, :name, generate_container_name())
    Logger.info("Creating container #{name} from #{image_ref}")

    # Generate container ID
    container_id = generate_container_id()

    # Create OCI bundle directory structure
    bundle_path = Path.join([System.tmp_dir!(), "vordr", "bundles", container_id])

    with :ok <- File.mkdir_p(bundle_path),
         :ok <- write_oci_config(bundle_path, config) do
      {:ok, container_id}
    else
      {:error, reason} ->
        Logger.error("Failed to create container bundle: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @doc """
  Start a created container.
  """
  @spec start_container(container_id()) :: :ok | {:error, term()}
  def start_container(container_id) do
    Logger.info("Starting container: #{container_id}")

    if use_mcp_runtime?() do
      McpClient.start(container_id)
    else
      # Stub implementation for development/testing
      :ok
    end
  end

  @doc """
  Stop a running container.

  ## Options
  - `:timeout` - Graceful shutdown timeout (default: 10 seconds)
  - `:signal` - Signal to send (default: SIGTERM, then SIGKILL)
  """
  @spec stop_container(container_id(), keyword()) :: :ok | {:error, term()}
  def stop_container(container_id, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 10_000)
    Logger.info("Stopping container #{container_id} (timeout: #{timeout}ms)")

    if use_mcp_runtime?() do
      McpClient.stop(container_id, timeout: div(timeout, 1000))
    else
      # Stub implementation for development/testing
      :ok
    end
  end

  @doc """
  Remove a container.

  ## Options
  - `:force` - Force removal even if running
  - `:volumes` - Also remove associated volumes
  """
  @spec remove_container(container_id(), keyword()) :: :ok | {:error, term()}
  def remove_container(container_id, opts \\ []) do
    force = Keyword.get(opts, :force, false)
    Logger.info("Removing container #{container_id} (force: #{force})")

    if use_mcp_runtime?() do
      McpClient.remove(container_id, force: force)
    else
      # Stub: clean up bundle directory
      bundle_path = Path.join([System.tmp_dir!(), "vordr", "bundles", container_id])

      if File.exists?(bundle_path) do
        File.rm_rf!(bundle_path)
      end

      :ok
    end
  end

  @doc """
  Get container state and information.
  """
  @spec inspect_container(container_id()) :: {:ok, map()} | {:error, :not_found}
  def inspect_container(container_id) do
    if use_mcp_runtime?() do
      McpClient.inspect_container(container_id)
    else
      # Stub implementation for development/testing
      {:ok, %{
        id: container_id,
        state: "running",
        pid: :rand.uniform(100_000),
        created: DateTime.utc_now(),
        started: DateTime.utc_now()
      }}
    end
  end

  @doc """
  Execute a command in a running container.
  """
  @spec exec(container_id(), [String.t()], keyword()) ::
          {:ok, {String.t(), integer()}} | {:error, term()}
  def exec(container_id, command, opts \\ []) do
    Logger.debug("Executing in #{container_id}: #{inspect(command)}")

    if use_mcp_runtime?() do
      case McpClient.exec(container_id, command, opts) do
        {:ok, %{"execPid" => _pid}} -> {:ok, {"", 0}}
        {:ok, result} -> {:ok, result}
        error -> error
      end
    else
      # Stub - would use runc exec
      {:ok, {"", 0}}
    end
  end

  @doc """
  Get container logs.

  ## Options
  - `:since` - Show logs since timestamp
  - `:until` - Show logs until timestamp
  - `:tail` - Number of lines from end
  - `:follow` - Stream logs
  """
  @spec logs(container_id(), keyword()) :: {:ok, String.t()} | {:error, term()}
  def logs(container_id, opts \\ []) do
    tail = Keyword.get(opts, :tail, 100)
    Logger.debug("Getting logs for #{container_id} (tail: #{tail})")

    {:ok, "Container #{container_id} started successfully.\n"}
  end

  @doc """
  List images on the host.
  """
  @spec list_images(keyword()) :: {:ok, [map()]}
  def list_images(_opts \\ []) do
    {:ok, []}
  end

  @doc """
  Remove an image.
  """
  @spec remove_image(image_ref()) :: :ok | {:error, term()}
  def remove_image(image_ref) do
    Logger.info("Removing image: #{image_ref}")
    :ok
  end

  # Private functions

  defp parse_image_ref(ref) do
    # Simple image reference parsing
    # Format: [registry/][namespace/]name[:tag|@digest]
    case String.split(ref, "/") do
      [name] ->
        {:ok, %{registry: "docker.io", namespace: "library", name: name}}

      [namespace, name] ->
        {:ok, %{registry: "docker.io", namespace: namespace, name: name}}

      [registry, namespace, name] ->
        {:ok, %{registry: registry, namespace: namespace, name: name}}

      _ ->
        {:error, :invalid_image_ref}
    end
  end

  defp generate_container_id do
    :crypto.strong_rand_bytes(16)
    |> Base.encode16(case: :lower)
  end

  defp generate_container_name do
    adjectives = ~w(happy clever swift calm bright eager fresh gentle kind lucky merry neat quick brave)
    nouns = ~w(fox wolf bear eagle hawk owl tiger lion shark whale dolphin falcon raven crane heron)

    adj = Enum.random(adjectives)
    noun = Enum.random(nouns)
    num = :rand.uniform(9999)

    "#{adj}_#{noun}_#{num}"
  end

  defp generate_digest do
    :crypto.strong_rand_bytes(32)
    |> Base.encode16(case: :lower)
    |> then(&"sha256:#{&1}")
  end

  defp write_oci_config(bundle_path, config) do
    config_path = Path.join(bundle_path, "config.json")
    json = Jason.encode!(config, pretty: true)
    File.write(config_path, json)
  end

  defp use_mcp_runtime? do
    Application.get_env(:vordr, :use_mcp_runtime, false)
  end
end
