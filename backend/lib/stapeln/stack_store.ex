defmodule Stapeln.StackStore do
  @moduledoc """
  In-memory stack persistence for API operations.

  All API entrypoints route through `Stapeln.NativeBridge`; this store is the
  fallback runtime used when a native Zig FFI binary is not configured.
  """

  use GenServer

  @type service :: %{optional(atom()) => term()}
  @type stack :: %{
          id: pos_integer(),
          name: String.t(),
          description: String.t() | nil,
          services: [service()],
          created_at: DateTime.t(),
          updated_at: DateTime.t()
        }

  @type state :: %{
          next_id: pos_integer(),
          stacks: %{optional(pos_integer()) => stack()}
        }

  @name __MODULE__

  @spec start_link(term()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: @name)
  end

  @spec create(map()) :: {:ok, stack()}
  def create(attrs) when is_map(attrs) do
    GenServer.call(@name, {:create, attrs})
  end

  @spec list() :: [stack()]
  def list do
    GenServer.call(@name, :list)
  end

  @spec get(pos_integer()) :: {:ok, stack()} | {:error, :not_found}
  def get(id) when is_integer(id) and id > 0 do
    GenServer.call(@name, {:get, id})
  end

  @spec update(pos_integer(), map()) :: {:ok, stack()} | {:error, :not_found}
  def update(id, attrs) when is_integer(id) and id > 0 and is_map(attrs) do
    GenServer.call(@name, {:update, id, attrs})
  end

  @spec reset!() :: :ok
  def reset! do
    GenServer.call(@name, :reset)
  end

  @impl true
  def init(_opts) do
    {:ok, %{next_id: 1, stacks: %{}}}
  end

  @impl true
  def handle_call({:create, attrs}, _from, state) do
    id = state.next_id
    now = DateTime.utc_now() |> DateTime.truncate(:second)

    stack = %{
      id: id,
      name: fetch(attrs, :name) || "stack-#{id}",
      description: fetch(attrs, :description),
      services: normalize_services(fetch(attrs, :services) || []),
      created_at: now,
      updated_at: now
    }

    next_state = %{
      state
      | next_id: id + 1,
        stacks: Map.put(state.stacks, id, stack)
    }

    {:reply, {:ok, stack}, next_state}
  end

  def handle_call(:list, _from, state) do
    stacks =
      state.stacks
      |> Map.values()
      |> Enum.sort_by(& &1.id)

    {:reply, stacks, state}
  end

  def handle_call({:get, id}, _from, state) do
    case Map.fetch(state.stacks, id) do
      {:ok, stack} -> {:reply, {:ok, stack}, state}
      :error -> {:reply, {:error, :not_found}, state}
    end
  end

  def handle_call({:update, id, attrs}, _from, state) do
    case Map.fetch(state.stacks, id) do
      {:ok, stack} ->
        updated =
          stack
          |> maybe_put(:name, fetch(attrs, :name))
          |> maybe_put(:description, fetch(attrs, :description))
          |> maybe_put_services(fetch(attrs, :services))
          |> Map.put(:updated_at, DateTime.utc_now() |> DateTime.truncate(:second))

        {:reply, {:ok, updated}, %{state | stacks: Map.put(state.stacks, id, updated)}}

      :error ->
        {:reply, {:error, :not_found}, state}
    end
  end

  def handle_call(:reset, _from, _state) do
    {:reply, :ok, %{next_id: 1, stacks: %{}}}
  end

  defp fetch(attrs, key) do
    Map.get(attrs, key) || Map.get(attrs, Atom.to_string(key))
  end

  defp maybe_put(stack, _field, nil), do: stack
  defp maybe_put(stack, field, value), do: Map.put(stack, field, value)

  defp maybe_put_services(stack, nil), do: stack

  defp maybe_put_services(stack, services) do
    Map.put(stack, :services, normalize_services(services))
  end

  defp normalize_services(services) when is_list(services) do
    Enum.map(services, fn service ->
      if is_map(service) do
        %{
          name: fetch(service, :name) || "unnamed-service",
          kind: fetch(service, :kind) || "unknown",
          port: fetch(service, :port)
        }
        |> Enum.reject(fn {_k, value} -> is_nil(value) end)
        |> Map.new()
      else
        %{
          name: "unnamed-service",
          kind: "unknown"
        }
      end
    end)
  end

  defp normalize_services(_), do: []
end
