# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Vordr.Containers do
  @moduledoc """
  Container management API

  Provides high-level functions for managing containers through the
  supervisor and registry.
  """

  alias Vordr.Containers.Container

  @doc """
  Start a new container from an image.

  ## Options

  - `:name` - Human-readable name
  - `:config` - Container configuration
  - `:labels` - Container labels
  - `:mounts` - Volume mounts
  - `:networks` - Network connections

  ## Examples

      {:ok, id} = Vordr.Containers.start_container("nginx:1.26", name: "web")
      {:ok, id} = Vordr.Containers.start_container("redis:7", name: "cache")
  """
  @spec start_container(String.t(), keyword()) :: {:ok, String.t()} | {:error, term()}
  def start_container(image, opts \\ []) do
    spec = {Container, [image, opts]}

    case DynamicSupervisor.start_child(Vordr.ContainerSupervisor, spec) do
      {:ok, _pid} ->
        {_state, data} = Container.get_state(opts[:name] || image)
        {:ok, data.id}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  List all containers.

  ## Options

  - `:state` - Filter by state (e.g., `:running`, `:stopped`)
  - `:all` - Include all states (default: only running)
  """
  @spec list(keyword()) :: [map()]
  def list(opts \\ []) do
    filter_state = Keyword.get(opts, :state)
    all = Keyword.get(opts, :all, false)

    Registry.select(Vordr.ContainerRegistry, [{{:"$1", :"$2", :"$3"}, [], [{{:"$1", :"$2"}}]}])
    |> Enum.map(fn {_id, pid} ->
      {state, data} = Container.get_state(pid)
      Map.put(data, :state, state)
    end)
    |> Enum.filter(fn container ->
      cond do
        filter_state != nil -> container.state == filter_state
        all -> true
        true -> container.state in [:running, :paused, :created]
      end
    end)
  end

  @doc """
  Get container by ID or name.
  """
  @spec get(String.t()) :: {:ok, map()} | {:error, :not_found}
  def get(id_or_name) do
    case Registry.lookup(Vordr.ContainerRegistry, id_or_name) do
      [{pid, _}] ->
        {state, data} = Container.get_state(pid)
        {:ok, Map.put(data, :state, state)}

      [] ->
        # Try to find by name
        list(all: true)
        |> Enum.find(fn c -> c.name == id_or_name end)
        |> case do
          nil -> {:error, :not_found}
          container -> {:ok, container}
        end
    end
  end

  @doc """
  Count containers by state.
  """
  @spec count() :: map()
  def count do
    list(all: true)
    |> Enum.group_by(& &1.state)
    |> Enum.map(fn {state, containers} -> {state, length(containers)} end)
    |> Map.new()
    |> Map.put(:total, Registry.count(Vordr.ContainerRegistry))
  end

  @doc """
  Create container (image_only → created).
  """
  @spec create(String.t()) :: :ok | {:error, term()}
  def create(id), do: Container.create(id)

  @doc """
  Start container (created → running).
  """
  @spec start(String.t()) :: :ok | {:error, term()}
  def start(id), do: Container.start(id)

  @doc """
  Pause container (running → paused).
  """
  @spec pause(String.t()) :: :ok | {:error, term()}
  def pause(id), do: Container.pause(id)

  @doc """
  Resume container (paused → running).
  """
  @spec resume(String.t()) :: :ok | {:error, term()}
  def resume(id), do: Container.resume(id)

  @doc """
  Stop container (running/paused → stopped).
  """
  @spec stop(String.t(), keyword()) :: :ok | {:error, term()}
  def stop(id, opts \\ []), do: Container.stop(id, opts)

  @doc """
  Restart container (stopped → running).
  """
  @spec restart(String.t()) :: :ok | {:error, term()}
  def restart(id), do: Container.restart(id)

  @doc """
  Remove container (created/stopped → removed).
  """
  @spec remove(String.t(), keyword()) :: :ok | {:error, term()}
  def remove(id, opts \\ []), do: Container.remove(id, opts)
end
