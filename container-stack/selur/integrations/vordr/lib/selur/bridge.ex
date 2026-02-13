# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Vordr.Selur.Bridge do
  @moduledoc """
  Elixir wrapper for selur WASM bridge.

  Provides type-safe, idiomatic Elixir API for zero-copy IPC with Vörðr
  container orchestrator.

  ## Examples

      iex> {:ok, bridge} = Vordr.Selur.Bridge.new("/path/to/selur.wasm")
      iex> {:ok, size} = Vordr.Selur.Bridge.memory_size(bridge)
      iex> {:ok, container_id} = Vordr.Selur.Bridge.create_container(bridge, "nginx:latest")
      iex> :ok = Vordr.Selur.Bridge.start_container(bridge, container_id)
      iex> :ok = Vordr.Selur.Bridge.free(bridge)
  """

  use Rustler, otp_app: :selur_vordr, crate: "selur_nif"

  @type bridge :: reference()
  @type container_id :: String.t()
  @type image_ref :: String.t()
  @type error ::
          :invalid_request
          | :container_not_found
          | :container_already_exists
          | :network_error
          | :internal_error
          | :unknown_error

  @doc """
  Create new bridge from WASM module path.

  ## Parameters

    * `wasm_path` - Absolute path to selur.wasm

  ## Returns

    * `{:ok, bridge}` - Bridge reference for subsequent operations
    * `{:error, reason}` - Error atom indicating failure

  ## Examples

      {:ok, bridge} = Bridge.new("/var/lib/selur/selur.wasm")
  """
  @spec new(String.t()) :: {:ok, bridge()} | {:error, error()}
  def new(_wasm_path), do: :erlang.nif_error(:nif_not_loaded)

  @doc """
  Free bridge resources.

  Always call when done to prevent resource leaks.

  ## Examples

      :ok = Bridge.free(bridge)
  """
  @spec free(bridge()) :: :ok
  def free(_bridge), do: :erlang.nif_error(:nif_not_loaded)

  @doc """
  Get current WASM linear memory size in bytes.

  ## Returns

    * `{:ok, size}` - Memory size in bytes
    * `{:error, reason}` - Error atom

  ## Examples

      {:ok, 1048576} = Bridge.memory_size(bridge)
  """
  @spec memory_size(bridge()) :: {:ok, non_neg_integer()} | {:error, error()}
  def memory_size(_bridge), do: :erlang.nif_error(:nif_not_loaded)

  @doc """
  Create container from OCI image reference.

  ## Parameters

    * `bridge` - Bridge reference
    * `image` - OCI image reference (e.g., "nginx:latest")

  ## Returns

    * `{:ok, container_id}` - Unique container identifier
    * `{:error, :container_already_exists}` - Container with this image already exists
    * `{:error, :network_error}` - Failed to pull image
    * `{:error, reason}` - Other error

  ## Examples

      {:ok, "c1a2b3c4"} = Bridge.create_container(bridge, "nginx:latest")
      {:ok, id} = Bridge.create_container(bridge, "ghcr.io/org/app:v1.0")
  """
  @spec create_container(bridge(), image_ref()) :: {:ok, container_id()} | {:error, error()}
  def create_container(_bridge, _image), do: :erlang.nif_error(:nif_not_loaded)

  @doc """
  Start a created container.

  ## Parameters

    * `bridge` - Bridge reference
    * `container_id` - Container ID from `create_container/2`

  ## Returns

    * `:ok` - Container started successfully
    * `{:error, :container_not_found}` - No such container
    * `{:error, reason}` - Other error

  ## Examples

      :ok = Bridge.start_container(bridge, "c1a2b3c4")
  """
  @spec start_container(bridge(), container_id()) :: :ok | {:error, error()}
  def start_container(_bridge, _container_id), do: :erlang.nif_error(:nif_not_loaded)

  @doc """
  Stop a running container.

  ## Parameters

    * `bridge` - Bridge reference
    * `container_id` - Container ID

  ## Returns

    * `:ok` - Container stopped successfully
    * `{:error, :container_not_found}` - No such container
    * `{:error, reason}` - Other error

  ## Examples

      :ok = Bridge.stop_container(bridge, "c1a2b3c4")
  """
  @spec stop_container(bridge(), container_id()) :: :ok | {:error, error()}
  def stop_container(_bridge, _container_id), do: :erlang.nif_error(:nif_not_loaded)

  @doc """
  Inspect container details (returns JSON).

  ## Parameters

    * `bridge` - Bridge reference
    * `container_id` - Container ID

  ## Returns

    * `{:ok, json_string}` - Container info as JSON
    * `{:error, :container_not_found}` - No such container
    * `{:error, reason}` - Other error

  ## Examples

      {:ok, json} = Bridge.inspect_container(bridge, "c1a2b3c4")
      info = Jason.decode!(json)
  """
  @spec inspect_container(bridge(), container_id()) :: {:ok, String.t()} | {:error, error()}
  def inspect_container(_bridge, _container_id), do: :erlang.nif_error(:nif_not_loaded)

  @doc """
  Delete a stopped container.

  ## Parameters

    * `bridge` - Bridge reference
    * `container_id` - Container ID

  ## Returns

    * `:ok` - Container deleted successfully
    * `{:error, :container_not_found}` - No such container
    * `{:error, reason}` - Other error

  ## Examples

      :ok = Bridge.delete_container(bridge, "c1a2b3c4")
  """
  @spec delete_container(bridge(), container_id()) :: :ok | {:error, error()}
  def delete_container(_bridge, _container_id), do: :erlang.nif_error(:nif_not_loaded)

  @doc """
  List all container IDs.

  ## Returns

    * `{:ok, [container_ids]}` - List of container ID strings
    * `{:error, reason}` - Error atom

  ## Examples

      {:ok, ["c1a2b3c4", "d5e6f7g8"]} = Bridge.list_containers(bridge)
  """
  @spec list_containers(bridge()) :: {:ok, [container_id()]} | {:error, error()}
  def list_containers(_bridge), do: :erlang.nif_error(:nif_not_loaded)

  # Stub implementations (replaced by NIF at runtime)
  defp bridge_new(_wasm_path), do: :erlang.nif_error(:nif_not_loaded)
  defp bridge_free(_bridge), do: :erlang.nif_error(:nif_not_loaded)
  defp bridge_memory_size(_bridge), do: :erlang.nif_error(:nif_not_loaded)
end
