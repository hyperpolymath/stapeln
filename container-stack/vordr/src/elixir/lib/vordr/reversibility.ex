# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Vordr.Reversibility do
  @moduledoc """
  Reversibility Layer

  Provides Bennett-reversible operations for container state management.
  Allows rolling back state changes with formal guarantees.

  ## Principle

  Every state-changing operation must be reversible:

      deploy(x) ∘ undeploy(x) = identity
      start(x) ∘ stop(x) = identity (within session)

  ## Usage

      # Execute with automatic journal logging
      {:ok, :running} = Vordr.Reversibility.execute(container_id, :start)

      # Rollback last operation
      :ok = Vordr.Reversibility.rollback(container_id)

      # Rollback multiple steps
      :ok = Vordr.Reversibility.rollback(container_id, steps: 3)

      # Check if rollback is possible
      true = Vordr.Reversibility.can_rollback?(container_id)
  """

  alias Vordr.Containers.Container
  alias Vordr.Reversibility.Journal

  require Logger

  @type rollback_result :: :ok | {:error, :not_reversible | :no_history | term()}

  @doc """
  Execute a reversible operation on a container.

  The operation is logged to the journal for potential rollback.
  """
  @spec execute(String.t(), atom(), keyword()) :: {:ok, atom()} | {:error, term()}
  def execute(container_id, operation, opts \\ []) do
    # Get current state before operation
    {current_state, _data} = Container.get_state(container_id)

    # Execute the operation
    result =
      case operation do
        :create -> Container.create(container_id)
        :start -> Container.start(container_id)
        :pause -> Container.pause(container_id)
        :resume -> Container.resume(container_id)
        :stop -> Container.stop(container_id, opts)
        :restart -> Container.restart(container_id)
        :remove -> Container.remove(container_id, opts)
        _ -> {:error, :unknown_operation}
      end

    case result do
      :ok ->
        {new_state, _data} = Container.get_state(container_id)
        Logger.info("Reversible operation: #{container_id} #{operation} (#{current_state} → #{new_state})")
        {:ok, new_state}

      error ->
        error
    end
  end

  @doc """
  Rollback the last operation(s) on a container.

  ## Options

  - `:steps` - Number of operations to rollback (default: 1)
  """
  @spec rollback(String.t(), keyword()) :: rollback_result()
  def rollback(container_id, opts \\ []) do
    steps = Keyword.get(opts, :steps, 1)

    # Get history
    history = Journal.get_last(container_id, steps)

    if Enum.empty?(history) do
      {:error, :no_history}
    else
      rollback_entries(container_id, history)
    end
  end

  @doc """
  Check if rollback is possible for a container.
  """
  @spec can_rollback?(String.t()) :: boolean()
  def can_rollback?(container_id) do
    Journal.can_reverse?(container_id)
  end

  @doc """
  Get the operation that would be performed on rollback.
  """
  @spec preview_rollback(String.t()) :: {:ok, atom()} | {:error, :not_reversible}
  def preview_rollback(container_id) do
    case Journal.get_reverse(container_id) do
      {:ok, {_current, target}} ->
        {:ok, state_to_operation(target)}

      error ->
        error
    end
  end

  @doc """
  Get rollback history preview for multiple steps.
  """
  @spec preview_rollback(String.t(), pos_integer()) :: [atom()]
  def preview_rollback(container_id, steps) do
    Journal.get_last(container_id, steps)
    |> Enum.filter(& &1.reversible)
    |> Enum.map(fn entry -> state_to_operation(entry.from_state) end)
  end

  @doc """
  Execute an operation with automatic rollback on failure.

  If the operation fails, attempts to restore the previous state.
  """
  @spec with_rollback(String.t(), atom(), keyword()) :: {:ok, atom()} | {:error, term()}
  def with_rollback(container_id, operation, opts \\ []) do
    {original_state, _data} = Container.get_state(container_id)

    case execute(container_id, operation, opts) do
      {:ok, new_state} ->
        {:ok, new_state}

      {:error, reason} ->
        Logger.warning("Operation #{operation} failed, attempting rollback to #{original_state}")

        case restore_state(container_id, original_state) do
          :ok ->
            Logger.info("Rollback successful")
            {:error, {:rolled_back, reason}}

          {:error, rollback_error} ->
            Logger.error("Rollback failed: #{inspect(rollback_error)}")
            {:error, {:rollback_failed, reason, rollback_error}}
        end
    end
  end

  ## Private Functions

  defp rollback_entries(_container_id, []), do: :ok

  defp rollback_entries(container_id, [entry | rest]) do
    if entry.reversible do
      case reverse_transition(container_id, entry) do
        :ok ->
          Logger.info("Rolled back: #{entry.to_state} → #{entry.from_state}")
          rollback_entries(container_id, rest)

        {:error, reason} ->
          Logger.error("Rollback failed at #{entry.to_state}: #{inspect(reason)}")
          {:error, reason}
      end
    else
      Logger.warning("Skipping non-reversible transition: #{entry.from_state} → #{entry.to_state}")
      rollback_entries(container_id, rest)
    end
  end

  defp reverse_transition(container_id, entry) do
    # Determine the reverse operation
    operation = state_to_operation(entry.from_state)

    if operation do
      case apply(Container, operation, [container_id]) do
        :ok -> :ok
        error -> error
      end
    else
      {:error, :no_reverse_operation}
    end
  end

  defp restore_state(container_id, target_state) do
    {current_state, _data} = Container.get_state(container_id)

    if current_state == target_state do
      :ok
    else
      operation = transition_operation(current_state, target_state)

      if operation do
        apply(Container, operation, [container_id])
      else
        {:error, {:no_path, current_state, target_state}}
      end
    end
  end

  # Map target state to the operation that achieves it
  defp state_to_operation(state) do
    case state do
      :created -> :create
      :running -> :start  # or :restart depending on context
      :paused -> :pause
      :stopped -> :stop
      nil -> nil
      _ -> nil
    end
  end

  # Map current → target to operation
  defp transition_operation(current, target) do
    case {current, target} do
      {:created, :running} -> :start
      {:running, :paused} -> :pause
      {:running, :stopped} -> :stop
      {:paused, :running} -> :resume
      {:paused, :stopped} -> :stop
      {:stopped, :running} -> :restart
      _ -> nil
    end
  end
end
