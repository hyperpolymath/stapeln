# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Vordr.Reversibility.Journal do
  @moduledoc """
  Reversibility Journal

  Implements Bennett-reversible state tracking for container operations.
  Every state transition is logged with enough information to reverse it.

  ## Reversibility Principle

  For any operation `op`, there exists a reverse operation `op⁻¹` such that:

      op ∘ op⁻¹ = identity

  This is achieved by logging:
  - Previous state
  - New state
  - Transition metadata (for reconstruction)
  - Timestamp

  ## Example

      # Forward operation
      container: running → stopped (exit_code: 0)

      # Reverse operation (if container supports restart)
      container: stopped → running (restore_from: journal)
  """

  use GenServer

  require Logger

  @type entry :: %{
          id: String.t(),
          container_id: String.t(),
          from_state: atom() | nil,
          to_state: atom(),
          metadata: map(),
          timestamp: DateTime.t(),
          reversible: boolean()
        }

  ## Client API

  @doc """
  Start the journal.
  """
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Log a state transition.
  """
  @spec log_transition(String.t(), atom() | nil, atom(), map()) :: :ok
  def log_transition(container_id, from_state, to_state, metadata \\ %{}) do
    GenServer.cast(__MODULE__, {:log, container_id, from_state, to_state, metadata})
  end

  @doc """
  Get transition history for a container.
  """
  @spec get_history(String.t()) :: [entry()]
  def get_history(container_id) do
    GenServer.call(__MODULE__, {:get_history, container_id})
  end

  @doc """
  Get the last N transitions for a container.
  """
  @spec get_last(String.t(), pos_integer()) :: [entry()]
  def get_last(container_id, count) do
    GenServer.call(__MODULE__, {:get_last, container_id, count})
  end

  @doc """
  Check if a transition can be reversed.
  """
  @spec can_reverse?(String.t()) :: boolean()
  def can_reverse?(container_id) do
    GenServer.call(__MODULE__, {:can_reverse, container_id})
  end

  @doc """
  Get the reverse transition for the last operation.
  """
  @spec get_reverse(String.t()) :: {:ok, {atom(), atom()}} | {:error, :not_reversible}
  def get_reverse(container_id) do
    GenServer.call(__MODULE__, {:get_reverse, container_id})
  end

  @doc """
  Get all journal entries.
  """
  @spec all() :: [entry()]
  def all do
    GenServer.call(__MODULE__, :all)
  end

  @doc """
  Clear journal entries for a container.
  """
  @spec clear(String.t()) :: :ok
  def clear(container_id) do
    GenServer.cast(__MODULE__, {:clear, container_id})
  end

  ## Server Callbacks

  @impl true
  def init(_opts) do
    # ETS table for fast lookups
    table = :ets.new(:vordr_journal, [:ordered_set, :protected])

    {:ok, %{table: table, counter: 0}}
  end

  @impl true
  def handle_cast({:log, container_id, from_state, to_state, metadata}, state) do
    entry = %{
      id: "#{state.counter}",
      container_id: container_id,
      from_state: from_state,
      to_state: to_state,
      metadata: metadata,
      timestamp: DateTime.utc_now(),
      reversible: is_reversible?(from_state, to_state)
    }

    # Use {container_id, counter} as key for ordering
    :ets.insert(state.table, {{container_id, state.counter}, entry})

    Logger.debug("Journal: #{container_id} #{from_state} → #{to_state}")

    :telemetry.execute(
      [:vordr, :journal, :transition],
      %{},
      %{container_id: container_id, from: from_state, to: to_state}
    )

    {:noreply, %{state | counter: state.counter + 1}}
  end

  def handle_cast({:clear, container_id}, state) do
    # Delete all entries for this container
    :ets.match_delete(state.table, {{container_id, :_}, :_})
    {:noreply, state}
  end

  @impl true
  def handle_call({:get_history, container_id}, _from, state) do
    entries =
      :ets.match(state.table, {{container_id, :_}, :"$1"})
      |> List.flatten()
      |> Enum.sort_by(& &1.id)

    {:reply, entries, state}
  end

  def handle_call({:get_last, container_id, count}, _from, state) do
    entries =
      :ets.match(state.table, {{container_id, :_}, :"$1"})
      |> List.flatten()
      |> Enum.sort_by(& &1.id, :desc)
      |> Enum.take(count)

    {:reply, entries, state}
  end

  def handle_call({:can_reverse, container_id}, _from, state) do
    case get_last_entry(state.table, container_id) do
      nil -> {:reply, false, state}
      entry -> {:reply, entry.reversible, state}
    end
  end

  def handle_call({:get_reverse, container_id}, _from, state) do
    case get_last_entry(state.table, container_id) do
      nil ->
        {:reply, {:error, :not_reversible}, state}

      %{reversible: false} ->
        {:reply, {:error, :not_reversible}, state}

      %{from_state: from, to_state: to} ->
        {:reply, {:ok, {to, from}}, state}
    end
  end

  def handle_call(:all, _from, state) do
    entries =
      :ets.tab2list(state.table)
      |> Enum.map(fn {_key, entry} -> entry end)
      |> Enum.sort_by(& &1.id)

    {:reply, entries, state}
  end

  ## Private Functions

  defp get_last_entry(table, container_id) do
    :ets.match(table, {{container_id, :_}, :"$1"})
    |> List.flatten()
    |> Enum.max_by(& &1.id, fn -> nil end)
  end

  # Reversible transitions
  # Based on container lifecycle - some transitions can be undone
  defp is_reversible?(from, to) do
    case {from, to} do
      # created ↔ running is reversible (start/stop)
      {:created, :running} -> true
      {:running, :stopped} -> true  # can restart

      # paused ↔ running is reversible
      {:running, :paused} -> true
      {:paused, :running} -> true

      # stopped → running is reversible (restart)
      {:stopped, :running} -> true

      # Removal is NOT reversible
      {_, :removed} -> false

      # Initial creation is not reversible (no prior state)
      {nil, _} -> false

      # Default: not reversible
      _ -> false
    end
  end
end
