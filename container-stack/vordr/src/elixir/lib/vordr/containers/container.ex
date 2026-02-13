# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Vordr.Containers.Container do
  @moduledoc """
  Container GenStateMachine

  Implements the formally verified container lifecycle state machine.
  Each state transition is logged to the reversibility journal.

  ## States

  - `:image_only` - Container image exists, no container created
  - `:created` - Container created but not started
  - `:running` - Container is executing
  - `:paused` - Container suspended
  - `:stopped` - Container exited
  - `:removed` - Container deleted (terminal state)

  ## Valid Transitions

  ```
  image_only → created     (create)
  created    → running     (start)
  created    → removed     (remove)
  running    → paused      (pause)
  running    → stopped     (stop)
  paused     → running     (resume)
  paused     → stopped     (stop)
  stopped    → running     (restart)
  stopped    → removed     (remove)
  ```
  """

  use GenStateMachine, callback_mode: [:handle_event_function, :state_enter]

  require Logger

  alias Vordr.Reversibility.Journal

  # Type definitions matching Idris2 types
  @type container_state ::
          :image_only | :created | :running | :paused | :stopped | :removed

  @type container_id :: String.t()

  @type container_data :: %{
          id: container_id(),
          name: String.t(),
          image: String.t(),
          created_at: DateTime.t(),
          started_at: DateTime.t() | nil,
          stopped_at: DateTime.t() | nil,
          exit_code: integer() | nil,
          pid: integer() | nil,
          config: map(),
          labels: map(),
          mounts: list(),
          networks: list()
        }

  # Valid state transitions (matching Idris2 ValidTransition type)
  # This data structure documents the valid transitions but is not used at runtime
  # since transition validation is enforced by the handle_event pattern matching.
  #
  # valid_transitions = %{
  #   image_only: [:created],
  #   created: [:running, :removed],
  #   running: [:paused, :stopped],
  #   paused: [:running, :stopped],
  #   stopped: [:running, :removed],
  #   removed: []  # Terminal state - no transitions out
  # }

  ## Client API

  @doc """
  Start a new container process.

  ## Options

  - `:name` - Human-readable name for the container
  - `:config` - Container configuration
  - `:labels` - Container labels
  """
  @spec start_link(String.t(), keyword()) :: GenStateMachine.on_start()
  def start_link(image, opts \\ []) do
    name = Keyword.get(opts, :name, generate_name())
    id = generate_id()

    data = %{
      id: id,
      name: name,
      image: image,
      created_at: DateTime.utc_now(),
      started_at: nil,
      stopped_at: nil,
      exit_code: nil,
      pid: nil,
      config: Keyword.get(opts, :config, %{}),
      labels: Keyword.get(opts, :labels, %{}),
      mounts: Keyword.get(opts, :mounts, []),
      networks: Keyword.get(opts, :networks, [])
    }

    GenStateMachine.start_link(__MODULE__, data,
      name: via_tuple(id)
    )
  end

  @doc """
  Get container state and data.
  """
  @spec get_state(container_id() | pid()) :: {container_state(), container_data()}
  def get_state(container) do
    GenStateMachine.call(resolve(container), :get_state)
  end

  @doc """
  Create container from image (image_only → created).
  """
  @spec create(container_id() | pid()) :: :ok | {:error, term()}
  def create(container) do
    GenStateMachine.call(resolve(container), :create)
  end

  @doc """
  Start container (created → running).
  """
  @spec start(container_id() | pid()) :: :ok | {:error, term()}
  def start(container) do
    GenStateMachine.call(resolve(container), :start)
  end

  @doc """
  Pause container (running → paused).
  """
  @spec pause(container_id() | pid()) :: :ok | {:error, term()}
  def pause(container) do
    GenStateMachine.call(resolve(container), :pause)
  end

  @doc """
  Resume container (paused → running).
  """
  @spec resume(container_id() | pid()) :: :ok | {:error, term()}
  def resume(container) do
    GenStateMachine.call(resolve(container), :resume)
  end

  @doc """
  Stop container (running/paused → stopped).
  """
  @spec stop(container_id() | pid(), keyword()) :: :ok | {:error, term()}
  def stop(container, opts \\ []) do
    timeout = Keyword.get(opts, :timeout, 10_000)
    GenStateMachine.call(resolve(container), {:stop, timeout})
  end

  @doc """
  Restart container (stopped → running).
  """
  @spec restart(container_id() | pid()) :: :ok | {:error, term()}
  def restart(container) do
    GenStateMachine.call(resolve(container), :restart)
  end

  @doc """
  Remove container (created/stopped → removed).
  """
  @spec remove(container_id() | pid(), keyword()) :: :ok | {:error, term()}
  def remove(container, opts \\ []) do
    force = Keyword.get(opts, :force, false)
    GenStateMachine.call(resolve(container), {:remove, force})
  end

  ## GenStateMachine Callbacks

  @impl true
  def init(data) do
    Logger.info("Container #{data.id} initialized (image: #{data.image})")

    # Log initial state to journal
    Journal.log_transition(data.id, nil, :image_only, %{image: data.image})

    {:ok, :image_only, data}
  end

  # State enter callbacks (for telemetry/logging)
  @impl true
  def handle_event(:enter, old_state, new_state, data) do
    if old_state != new_state do
      Logger.debug("Container #{data.id}: #{old_state} → #{new_state}")

      :telemetry.execute(
        [:vordr, :container, :state_change],
        %{},
        %{container_id: data.id, from: old_state, to: new_state}
      )
    end

    :keep_state_and_data
  end

  # Get state
  def handle_event({:call, from}, :get_state, state, data) do
    {:keep_state_and_data, [{:reply, from, {state, data}}]}
  end

  # Create: image_only → created
  def handle_event({:call, from}, :create, :image_only, data) do
    case do_create(data) do
      {:ok, new_data} ->
        Journal.log_transition(data.id, :image_only, :created, %{})
        {:next_state, :created, new_data, [{:reply, from, :ok}]}

      {:error, reason} ->
        {:keep_state_and_data, [{:reply, from, {:error, reason}}]}
    end
  end

  # Start: created → running
  def handle_event({:call, from}, :start, :created, data) do
    case do_start(data) do
      {:ok, new_data} ->
        Journal.log_transition(data.id, :created, :running, %{pid: new_data.pid})
        {:next_state, :running, new_data, [{:reply, from, :ok}]}

      {:error, reason} ->
        {:keep_state_and_data, [{:reply, from, {:error, reason}}]}
    end
  end

  # Pause: running → paused
  def handle_event({:call, from}, :pause, :running, data) do
    case do_pause(data) do
      :ok ->
        Journal.log_transition(data.id, :running, :paused, %{})
        {:next_state, :paused, data, [{:reply, from, :ok}]}

      {:error, reason} ->
        {:keep_state_and_data, [{:reply, from, {:error, reason}}]}
    end
  end

  # Resume: paused → running
  def handle_event({:call, from}, :resume, :paused, data) do
    case do_resume(data) do
      :ok ->
        Journal.log_transition(data.id, :paused, :running, %{})
        {:next_state, :running, data, [{:reply, from, :ok}]}

      {:error, reason} ->
        {:keep_state_and_data, [{:reply, from, {:error, reason}}]}
    end
  end

  # Stop: running → stopped
  def handle_event({:call, from}, {:stop, timeout}, :running, data) do
    case do_stop(data, timeout) do
      {:ok, exit_code} ->
        new_data = %{data | stopped_at: DateTime.utc_now(), exit_code: exit_code, pid: nil}
        Journal.log_transition(data.id, :running, :stopped, %{exit_code: exit_code})
        {:next_state, :stopped, new_data, [{:reply, from, :ok}]}

      {:error, reason} ->
        {:keep_state_and_data, [{:reply, from, {:error, reason}}]}
    end
  end

  # Stop: paused → stopped
  def handle_event({:call, from}, {:stop, timeout}, :paused, data) do
    case do_stop(data, timeout) do
      {:ok, exit_code} ->
        new_data = %{data | stopped_at: DateTime.utc_now(), exit_code: exit_code, pid: nil}
        Journal.log_transition(data.id, :paused, :stopped, %{exit_code: exit_code})
        {:next_state, :stopped, new_data, [{:reply, from, :ok}]}

      {:error, reason} ->
        {:keep_state_and_data, [{:reply, from, {:error, reason}}]}
    end
  end

  # Restart: stopped → running
  def handle_event({:call, from}, :restart, :stopped, data) do
    case do_start(data) do
      {:ok, new_data} ->
        Journal.log_transition(data.id, :stopped, :running, %{pid: new_data.pid})
        {:next_state, :running, new_data, [{:reply, from, :ok}]}

      {:error, reason} ->
        {:keep_state_and_data, [{:reply, from, {:error, reason}}]}
    end
  end

  # Remove: created → removed
  def handle_event({:call, from}, {:remove, _force}, :created, data) do
    Journal.log_transition(data.id, :created, :removed, %{})
    {:next_state, :removed, data, [{:reply, from, :ok}]}
  end

  # Remove: stopped → removed
  def handle_event({:call, from}, {:remove, _force}, :stopped, data) do
    Journal.log_transition(data.id, :stopped, :removed, %{})
    {:next_state, :removed, data, [{:reply, from, :ok}]}
  end

  # Force remove from running (if force=true)
  def handle_event({:call, from}, {:remove, true}, :running, data) do
    _ = do_stop(data, 0)
    Journal.log_transition(data.id, :running, :removed, %{forced: true})
    {:next_state, :removed, data, [{:reply, from, :ok}]}
  end

  # Invalid transitions
  def handle_event({:call, from}, action, state, data) do
    Logger.warning("Invalid transition: #{inspect(action)} in state #{state} for #{data.id}")
    {:keep_state_and_data, [{:reply, from, {:error, {:invalid_transition, state, action}}}]}
  end

  ## Private Functions

  defp via_tuple(id) do
    {:via, Registry, {Vordr.ContainerRegistry, id}}
  end

  defp resolve(pid) when is_pid(pid), do: pid
  defp resolve(id) when is_binary(id), do: via_tuple(id)

  defp generate_id do
    :crypto.strong_rand_bytes(16) |> Base.encode16(case: :lower)
  end

  defp generate_name do
    adjectives = ~w(happy swift brave calm clever eager gentle jolly kind lucky)
    nouns = ~w(falcon eagle hawk raven condor owl sparrow robin finch crane)

    adj = Enum.random(adjectives)
    noun = Enum.random(nouns)
    num = :rand.uniform(999)

    "#{adj}_#{noun}_#{num}"
  end

  # Stub implementations - would call actual runtime in production
  @spec do_create(map()) :: {:ok, map()} | {:error, term()}
  defp do_create(data) do
    Logger.info("Creating container #{data.id} from image #{data.image}")
    try do
      {:ok, data}
    rescue
      e -> {:error, Exception.message(e)}
    end
  end

  @spec do_start(map()) :: {:ok, map()} | {:error, term()}
  defp do_start(data) do
    Logger.info("Starting container #{data.id}")
    try do
      # Simulate getting a PID (in production, would call runtime)
      pid = :rand.uniform(65535)
      {:ok, %{data | started_at: DateTime.utc_now(), pid: pid}}
    rescue
      e -> {:error, Exception.message(e)}
    end
  end

  @spec do_pause(map()) :: :ok | {:error, term()}
  defp do_pause(data) do
    Logger.info("Pausing container #{data.id}")
    try do
      :ok
    rescue
      e -> {:error, Exception.message(e)}
    end
  end

  @spec do_resume(map()) :: :ok | {:error, term()}
  defp do_resume(data) do
    Logger.info("Resuming container #{data.id}")
    try do
      :ok
    rescue
      e -> {:error, Exception.message(e)}
    end
  end

  @spec do_stop(map(), non_neg_integer()) :: {:ok, integer()} | {:error, term()}
  defp do_stop(data, timeout) do
    Logger.info("Stopping container #{data.id} (timeout: #{timeout}ms)")
    try do
      # In production, would send SIGTERM to container process
      {:ok, 0}
    rescue
      e -> {:error, Exception.message(e)}
    end
  end
end
