defmodule Stapeln.Security.PanicAttacker do
  @moduledoc """
  Supervises panic-attack invocations and exposes trace/timeline state to the security UI.
  """

  use GenServer
  require Logger

  alias Stapeln.TaskSupervisor

  @max_timeline 40

  defmodule State do
    defstruct status: :idle,
              timeline: [],
              command: nil,
              target: nil,
              schedule: nil,
              task: nil
  end

  ## Client API

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %State{}, name: __MODULE__)
  end

  def start_trace(command, target, schedule) do
    GenServer.call(__MODULE__, {:start, command, target, schedule})
  end

  def stop_trace do
    GenServer.call(__MODULE__, :stop)
  end

  def status do
    GenServer.call(__MODULE__, :status)
  end

  ## Server callbacks

  @impl true
  def init(state) do
    {:ok, state}
  end

  defguardp matches_task?(state, ref) when not is_nil(state.task) and state.task.ref == ref

  @impl true
  def handle_call({:start, _command, _target, _schedule}, _from, state)
      when state.status == :running do
    {state, _} = append_event(state, :warn, "Panic Attacker already running")
    {:reply, {:error, :already_running, response(state)}, state}
  end

  def handle_call({:start, command, target, schedule}, _from, state) do
    %State{} = state = set_command(state, command, target, schedule)

    {%State{} = state, _} =
      append_event(state, :info, "Scheduling panic-attack #{command} #{target}")

    task = Task.Supervisor.async_nolink(TaskSupervisor, fn -> execute(command, target) end)
    new_state = %State{state | status: :running, task: task}
    Logger.info("Panic Attacker scheduled command #{command} #{target}")
    {:reply, {:ok, response(new_state)}, new_state}
  end

  def handle_call(:stop, _from, state) do
    {%State{} = state, entry} =
      if state.status == :running and state.task do
        Task.shutdown(state.task, :brutal_kill)
        append_event(state, :warn, "Panic Attacker monitoring stopped")
      else
        append_event(state, :info, "Panic Attacker idle; nothing to stop")
      end

    new_state = %State{state | status: :stopped, task: nil}
    Logger.info("Panic Attacker stopped: #{entry.message}")
    {:reply, {:ok, response(new_state)}, new_state}
  end

  def handle_call(:status, _from, state) do
    {:reply, response(state), state}
  end

  @impl true
  def handle_info({ref, {:ok, output}}, state) when matches_task?(state, ref) do
    {%State{} = state, _} =
      append_event(state, :success, "panic-attack completed: #{shorten(output)}")

    new_state = %State{state | status: :completed, task: nil}
    {:noreply, new_state}
  end

  def handle_info({ref, {:error, reason}}, state) when matches_task?(state, ref) do
    {%State{} = state, _} = append_event(state, :error, "panic-attack failed: #{inspect(reason)}")
    new_state = %State{state | status: :failed, task: nil}
    {:noreply, new_state}
  end

  def handle_info({:DOWN, ref, :process, _pid, reason}, state) when matches_task?(state, ref) do
    status = if reason == :normal, do: :completed, else: :failed
    level = if reason == :normal, do: :success, else: :error

    {%State{} = state, _} =
      append_event(state, level, "panic-attack process down: #{inspect(reason)}")

    new_state = %State{state | status: status, task: nil}
    {:noreply, new_state}
  end

  def handle_info(_msg, state), do: {:noreply, state}

  ## Helpers

  defp set_command(state, command, target, schedule) do
    %State{state | command: command, target: target, schedule: schedule}
  end

  defp response(state) do
    %{
      status: Atom.to_string(state.status),
      command: state.command,
      target: state.target,
      schedule: state.schedule,
      timeline: state.timeline
    }
  end

  defp append_event(state, level, message) do
    entry = timeline_entry(level, message)
    timeline = limit_timeline(state.timeline ++ [entry])
    {%State{state | timeline: timeline}, entry}
  end

  defp timeline_entry(level, message) do
    timestamp = DateTime.utc_now() |> DateTime.to_iso8601()
    id = Integer.to_string(System.unique_integer([:positive]))
    %{id: id, timestamp: timestamp, level: to_string(level), message: message}
  end

  defp limit_timeline(timeline) do
    len = length(timeline)

    if len <= @max_timeline do
      timeline
    else
      start = len - @max_timeline
      Enum.slice(timeline, start, @max_timeline)
    end
  end

  defp execute(command, target) do
    args = build_args(command, target)

    case System.find_executable("panic-attack") do
      nil ->
        {:error, :not_found, "panic-attack binary not found in PATH"}

      exe ->
        Logger.debug("Executing #{exe} #{Enum.join(args, " ")}")

        case System.cmd(exe, args, stderr_to_stdout: true) do
          {output, 0} -> {:ok, output}
          {output, code} -> {:error, {:exit, code, output}}
        end
    end
  end

  defp build_args("ambush", target) do
    ["ambush", target]
  end

  defp build_args(_, target) do
    ["ambush", target]
  end

  defp shorten(output) do
    output
    |> String.trim()
    |> String.slice(0, 120)
  end
end
