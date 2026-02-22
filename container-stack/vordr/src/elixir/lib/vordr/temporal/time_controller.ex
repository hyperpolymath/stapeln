# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Vordr.Temporal.TimeController do
  @moduledoc """
  Time Manipulation Orchestrator

  Manages fake NTP and PTP servers to control the target process's
  perception of time. Coordinates with libfaketime injection and
  the Rust-side TimeController for consistent time across all layers.

  ## Time layers

  | Layer          | Mechanism                 | Fakes                          |
  |----------------|---------------------------|--------------------------------|
  | NTP            | Fake NTP server on port   | ntpdate, chronyd queries       |
  | PTP            | IEEE 1588 fake grandmaster| Hardware clock sync, /dev/ptp* |
  | libfaketime    | LD_PRELOAD (Rust side)    | gettimeofday(), clock_gettime()|
  | CLOCK_MONOTONIC| Via libfaketime           | Uptime-relative timing         |
  | RDTSC          | Firecracker (Rust side)   | CPU cycle counter              |

  ## PTP as processor speed simulation

  PTP normally synchronises clocks to sub-microsecond accuracy.
  By controlling the PTP grandmaster, we make the target's clock
  sync report that the processor is running slower than it actually
  is. This creates a perception that time passes slowly, giving
  our BEAM interceptors more relative time to react.

  ## Detection resistance

  Programs routinely deal with NTP jitter, port conflicts, and
  variable syscall latency. Our manipulation looks like normal
  operational noise.
  """

  use GenServer

  require Logger

  @ntp_port 123
  @ptp_event_port 319
  @ptp_general_port 320

  # ── Types ──────────────────────────────────────────────

  @type time_offset :: integer()
  @type dilation_ratio :: float()

  @type sweep_strategy :: :linear | :calendar_aware | :binary_search | :oscillating

  @type sweep_config :: %{
          start_offset: time_offset(),
          end_offset: time_offset(),
          step_seconds: pos_integer(),
          strategy: sweep_strategy()
        }

  # ── Public API ─────────────────────────────────────────

  @doc """
  Start the time controller.

  ## Options

  - `:dilation_ratio` — Speed ratio (default: 10_000.0, meaning 10,000:1)
  - `:initial_offset` — Starting time offset in seconds (default: 0)
  - `:enable_ntp` — Run fake NTP server (default: true for temporal tier)
  - `:enable_ptp` — Run fake PTP grandmaster (default: true for temporal tier)
  - `:ntp_port` — Port for fake NTP server (default: 123)
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Set the simulated time offset (seconds from real time).

  Positive values = future, negative = past.
  All active time layers are updated atomically.
  """
  @spec set_offset(time_offset()) :: :ok
  def set_offset(seconds) do
    GenServer.call(__MODULE__, {:set_offset, seconds})
  end

  @doc """
  Advance simulated time by a step.
  """
  @spec advance(pos_integer()) :: :ok
  def advance(seconds) do
    GenServer.call(__MODULE__, {:advance, seconds})
  end

  @doc """
  Reverse simulated time by a step.
  """
  @spec reverse(pos_integer()) :: :ok
  def reverse(seconds) do
    GenServer.call(__MODULE__, {:reverse, seconds})
  end

  @doc """
  Get the current simulated time offset.
  """
  @spec current_offset() :: time_offset()
  def current_offset do
    GenServer.call(__MODULE__, :current_offset)
  end

  @doc """
  Run a time sweep across a range.

  Calls the provided callback at each time point. The callback
  receives the current offset and should return `:continue` or
  `{:triggered, details}`.

  Returns `:ok` if no trigger found, or `{:triggered, offset, details}`.
  """
  @spec sweep(sweep_config(), (time_offset() -> :continue | {:triggered, term()})) ::
          :ok | {:triggered, time_offset(), term()}
  def sweep(config, callback) do
    GenServer.call(__MODULE__, {:sweep, config, callback}, :infinity)
  end

  @doc """
  Get calendar-aware trigger dates.

  Returns a list of dates with higher probability of being
  trigger dates for time-bombs: epoch boundaries, New Year,
  quarter starts, known CVE dates.
  """
  @spec calendar_trigger_dates() :: [DateTime.t()]
  def calendar_trigger_dates do
    base_year = DateTime.utc_now().year

    [
      # Epoch overflow (2038 problem)
      ~U[2038-01-19 03:14:07Z],
      ~U[2038-01-19 03:14:08Z],
      # New Year boundaries
      DateTime.new!(Date.new!(base_year, 1, 1), ~T[00:00:00], "Etc/UTC"),
      DateTime.new!(Date.new!(base_year + 1, 1, 1), ~T[00:00:00], "Etc/UTC"),
      # Quarter boundaries
      DateTime.new!(Date.new!(base_year, 4, 1), ~T[00:00:00], "Etc/UTC"),
      DateTime.new!(Date.new!(base_year, 7, 1), ~T[00:00:00], "Etc/UTC"),
      DateTime.new!(Date.new!(base_year, 10, 1), ~T[00:00:00], "Etc/UTC"),
      # Specific notable dates
      DateTime.new!(Date.new!(base_year, 2, 29), ~T[00:00:00], "Etc/UTC") |> maybe_date(),
      # Y2K38 midpoint
      ~U[2030-01-01 00:00:00Z],
      ~U[2035-01-01 00:00:00Z]
    ]
    |> Enum.reject(&is_nil/1)
  end

  # ── GenServer callbacks ────────────────────────────────

  @impl true
  def init(opts) do
    dilation = Keyword.get(opts, :dilation_ratio, 10_000.0)
    offset = Keyword.get(opts, :initial_offset, 0)
    enable_ntp = Keyword.get(opts, :enable_ntp, false)
    enable_ptp = Keyword.get(opts, :enable_ptp, false)

    state = %{
      dilation_ratio: dilation,
      current_offset: offset,
      base_real_time: System.system_time(:second),
      ntp_server: nil,
      ptp_server: nil,
      sweep_history: []
    }

    # Start fake NTP server if enabled
    state =
      if enable_ntp do
        case start_fake_ntp(offset) do
          {:ok, pid} ->
            Logger.info("Fake NTP server started on port #{@ntp_port}")
            %{state | ntp_server: pid}

          {:error, reason} ->
            Logger.warning("Could not start fake NTP: #{inspect(reason)}")
            state
        end
      else
        state
      end

    # Start fake PTP grandmaster if enabled
    state =
      if enable_ptp do
        case start_fake_ptp(offset, dilation) do
          {:ok, pid} ->
            Logger.info("Fake PTP grandmaster started (dilation: #{dilation}:1)")
            %{state | ptp_server: pid}

          {:error, reason} ->
            Logger.warning("Could not start fake PTP: #{inspect(reason)}")
            state
        end
      else
        state
      end

    {:ok, state}
  end

  @impl true
  def handle_call({:set_offset, seconds}, _from, state) do
    new_state = update_offset(state, seconds)
    {:reply, :ok, new_state}
  end

  @impl true
  def handle_call({:advance, seconds}, _from, state) do
    new_state = update_offset(state, state.current_offset + seconds)
    {:reply, :ok, new_state}
  end

  @impl true
  def handle_call({:reverse, seconds}, _from, state) do
    new_state = update_offset(state, state.current_offset - seconds)
    {:reply, :ok, new_state}
  end

  @impl true
  def handle_call(:current_offset, _from, state) do
    {:reply, state.current_offset, state}
  end

  @impl true
  def handle_call({:sweep, config, callback}, _from, state) do
    result = execute_sweep(state, config, callback)
    {:reply, result, state}
  end

  # ── Private ────────────────────────────────────────────

  defp update_offset(state, new_offset) do
    Logger.debug("Time offset: #{state.current_offset}s → #{new_offset}s")

    # Update NTP server offset
    if state.ntp_server do
      send(state.ntp_server, {:set_offset, new_offset})
    end

    # Update PTP grandmaster offset
    if state.ptp_server do
      send(state.ptp_server, {:set_offset, new_offset})
    end

    %{state | current_offset: new_offset}
  end

  defp execute_sweep(state, config, callback) do
    offsets = generate_sweep_offsets(config)

    Logger.info("Sweep: #{length(offsets)} time points, strategy: #{config.strategy}")

    Enum.reduce_while(offsets, :ok, fn offset, _acc ->
      # Update time layers
      update_offset(state, offset)

      # Call the observation callback
      case callback.(offset) do
        :continue ->
          {:cont, :ok}

        {:triggered, details} ->
          Logger.warning("Trigger detected at offset #{offset}s")
          {:halt, {:triggered, offset, details}}
      end
    end)
  end

  defp generate_sweep_offsets(%{strategy: :linear} = config) do
    config.start_offset
    |> Stream.iterate(&(&1 + config.step_seconds))
    |> Enum.take_while(&(&1 <= config.end_offset))
  end

  defp generate_sweep_offsets(%{strategy: :calendar_aware} = config) do
    # Linear sweep plus extra density around calendar trigger dates
    linear = generate_sweep_offsets(%{config | strategy: :linear})

    # Add fine-grained points around each calendar trigger date
    now = System.system_time(:second)

    calendar_offsets =
      calendar_trigger_dates()
      |> Enum.flat_map(fn dt ->
        target = DateTime.to_unix(dt)
        offset = target - now

        if offset >= config.start_offset and offset <= config.end_offset do
          # 1-hour granularity around each trigger date (24 hours before and after)
          -24..24
          |> Enum.map(&(offset + &1 * 3600))
        else
          []
        end
      end)

    (linear ++ calendar_offsets)
    |> Enum.sort()
    |> Enum.dedup()
  end

  defp generate_sweep_offsets(%{strategy: :oscillating} = config) do
    # Forward-back-forward to catch hysteresis
    linear = generate_sweep_offsets(%{config | strategy: :linear})
    reversed = Enum.reverse(linear)

    linear ++ reversed ++ linear
  end

  defp generate_sweep_offsets(%{strategy: :binary_search} = config) do
    # Binary search starts with midpoint, then quarters, then eighths, etc.
    mid = div(config.start_offset + config.end_offset, 2)
    q1 = div(config.start_offset + mid, 2)
    q3 = div(mid + config.end_offset, 2)

    [config.start_offset, mid, config.end_offset, q1, q3]
  end

  defp start_fake_ntp(_offset) do
    # Stub: In full implementation, starts a UDP server on port 123
    # that responds to NTP queries with the manipulated time.
    # Uses Erlang's :gen_udp for low-overhead packet handling.
    {:error, :not_implemented}
  end

  defp start_fake_ptp(_offset, _dilation) do
    # Stub: In full implementation, starts an IEEE 1588 PTP
    # grandmaster that announces itself as the best clock source.
    # The target's PTP client syncs to this clock, perceiving
    # a slower processor speed.
    {:error, :not_implemented}
  end

  defp maybe_date(datetime) do
    datetime
  rescue
    _ -> nil
  end
end
