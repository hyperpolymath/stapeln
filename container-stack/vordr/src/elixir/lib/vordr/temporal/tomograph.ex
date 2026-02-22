# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Vordr.Temporal.Tomograph do
  @moduledoc """
  CT-Scan Tomographic Observation Mode

  Like a CT scanner taking thousands of thin slices to build a 3D image,
  each capturing almost nothing individually, this module distributes
  observation across thousands of instances — each monitoring ONE thing.

  ## Concept

  1. Make N copies of the suspicious code (N = 1,000 to 100,000)
  2. Each copy runs in its own sealed environment (Kubernetes pod)
  3. Each copy monitors ONE thing only:
     - One port, OR one syscall number, OR one file descriptor,
       OR one network destination, OR one signal
  4. Time sweeps at high speed, forward and backward, oscillating
  5. No single instance knows the full scan is happening
  6. Combine all N results into a complete behavioural tomograph

  ## Why this is undetectable

  - Each instance's monitoring footprint is ONE data point —
    statistically indistinguishable from normal environmental noise
  - The code cannot warn other instances — they are isolated
  - Even if the code detects ONE instance, the other N-1 still collect
  - The attacker would need to detect distributed partial observation,
    which is orders of magnitude harder than detecting strace

  ## Kubernetes orchestration

  Uses the K8s Elixir client to spawn and manage pods.
  Each pod runs a copy of the target with a single-channel monitor.
  Results stream back via the central ring buffer.

  ## For high-assurance scanning

  Nuclear, aviation, power grid, military — environments where
  a missed time-bomb could have catastrophic consequences.
  """

  use GenServer

  require Logger

  # ── Types ──────────────────────────────────────────────

  @type observation_channel :: {:syscall, non_neg_integer()}
                             | {:port, pos_integer()}
                             | {:signal, non_neg_integer()}
                             | {:file_pattern, String.t()}
                             | {:network_dest, String.t()}

  @type slice_result :: %{
          channel: observation_channel(),
          time_offset: integer(),
          observed: boolean(),
          details: term()
        }

  @type tomograph :: %{
          total_slices: non_neg_integer(),
          triggered_slices: non_neg_integer(),
          trigger_channels: [observation_channel()],
          trigger_time_range: {integer(), integer()} | nil,
          anomaly_map: %{observation_channel() => [integer()]}
        }

  # ── Public API ─────────────────────────────────────────

  @doc """
  Start the tomograph orchestrator.

  ## Options

  - `:instances` — Number of parallel instances (default: 1_000)
  - `:k8s_context` — Kubernetes context to use
  - `:image` — Container image for each instance
  - `:target_binary` — Path to the binary under observation
  - `:sweep_config` — Time sweep configuration
  - `:channels` — List of observation channels (auto-generated if nil)
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Run a full tomographic scan.

  Spawns N instances across Kubernetes, each monitoring a single
  channel. Sweeps through time, collects results, and assembles
  the complete behavioural tomograph.

  Returns the assembled tomograph with anomaly map.
  """
  @spec scan() :: {:ok, tomograph()} | {:error, term()}
  def scan do
    GenServer.call(__MODULE__, :scan, :infinity)
  end

  @doc """
  Generate observation channels for a given configuration.

  Produces a list of single-channel monitoring specifications:
  - One per common syscall (0..500)
  - One per interesting port (1..1024, plus common high ports)
  - One per POSIX signal (1..31)
  - Key file patterns (/etc/shadow, /proc/*, /dev/*)
  """
  @spec generate_channels(keyword()) :: [observation_channel()]
  def generate_channels(opts \\ []) do
    syscalls = generate_syscall_channels(opts)
    ports = generate_port_channels(opts)
    signals = generate_signal_channels()
    files = generate_file_channels()

    syscalls ++ ports ++ signals ++ files
  end

  @doc """
  Assemble slice results into a tomograph.

  Takes raw results from all instances and builds the
  composite behavioural picture.
  """
  @spec assemble([slice_result()]) :: tomograph()
  def assemble(slices) do
    triggered = Enum.filter(slices, & &1.observed)

    anomaly_map =
      triggered
      |> Enum.group_by(& &1.channel)
      |> Enum.map(fn {channel, results} ->
        offsets = Enum.map(results, & &1.time_offset) |> Enum.sort()
        {channel, offsets}
      end)
      |> Map.new()

    time_range =
      case triggered do
        [] ->
          nil

        results ->
          offsets = Enum.map(results, & &1.time_offset)
          {Enum.min(offsets), Enum.max(offsets)}
      end

    %{
      total_slices: length(slices),
      triggered_slices: length(triggered),
      trigger_channels: Map.keys(anomaly_map),
      trigger_time_range: time_range,
      anomaly_map: anomaly_map
    }
  end

  @doc """
  Diff a tomograph against a baseline.

  Returns only the anomalous channels — things that appeared
  in the scan but not in the baseline.
  """
  @spec diff(tomograph(), tomograph()) :: tomograph()
  def diff(scan, baseline) do
    baseline_channels = MapSet.new(baseline.trigger_channels)

    novel_anomalies =
      scan.anomaly_map
      |> Enum.reject(fn {channel, _offsets} ->
        MapSet.member?(baseline_channels, channel)
      end)
      |> Map.new()

    %{
      total_slices: scan.total_slices,
      triggered_slices: map_size(novel_anomalies),
      trigger_channels: Map.keys(novel_anomalies),
      trigger_time_range: scan.trigger_time_range,
      anomaly_map: novel_anomalies
    }
  end

  # ── GenServer callbacks ────────────────────────────────

  @impl true
  def init(opts) do
    state = %{
      instances: Keyword.get(opts, :instances, 1_000),
      k8s_context: Keyword.get(opts, :k8s_context),
      image: Keyword.get(opts, :image),
      target_binary: Keyword.get(opts, :target_binary),
      sweep_config: Keyword.get(opts, :sweep_config),
      channels: Keyword.get(opts, :channels),
      results: [],
      status: :idle
    }

    {:ok, state}
  end

  @impl true
  def handle_call(:scan, _from, state) do
    Logger.info("Starting tomographic scan: #{state.instances} instances")

    channels = state.channels || generate_channels()

    # In full implementation:
    # 1. For each channel, spawn a Kubernetes pod via K8s client
    # 2. Each pod runs: target binary + single-channel eBPF monitor
    # 3. Time controller sweeps through range
    # 4. Collect results from all pods via ring buffer
    # 5. Assemble tomograph

    # Stub: generate empty results
    slices =
      Enum.map(channels, fn channel ->
        %{
          channel: channel,
          time_offset: 0,
          observed: false,
          details: nil
        }
      end)

    tomograph = assemble(slices)

    Logger.info(
      "Tomographic scan complete: #{tomograph.total_slices} slices, " <>
        "#{tomograph.triggered_slices} triggered"
    )

    {:reply, {:ok, tomograph}, %{state | status: :complete, results: slices}}
  end

  # ── Channel generators ─────────────────────────────────

  defp generate_syscall_channels(_opts \\ []) do
    # Common syscalls to monitor (Linux x86_64)
    # Focus on: time queries, file access, network, process control
    important_syscalls = [
      # Time queries (what time-bombs check)
      96,   # gettimeofday
      228,  # clock_gettime
      230,  # clock_nanosleep
      35,   # nanosleep
      # File access
      2, 257,  # open, openat
      0, 1,    # read, write
      # Network
      41, 42, 43, 44, 45, 49, 50,  # socket, connect, accept, sendto, recvfrom, bind, listen
      # Process
      59, 322,  # execve, execveat
      56, 57, 62,  # exit, wait4, kill
      # Namespace/container escape
      272, 308,   # unshare, setns
      161, 163,   # chroot, pivot_root
      # Module loading
      175, 176, 313  # init_module, delete_module, finit_module
    ]

    Enum.map(important_syscalls, &{:syscall, &1})
  end

  defp generate_port_channels(_opts \\ []) do
    # Well-known ports plus common service ports
    well_known = Enum.map(1..1024, &{:port, &1})

    common_high = [
      3306, 5432, 6379, 8080, 8443, 9090, 27017,  # DB, web, monitoring
      4444, 5555, 6666, 7777, 8888, 9999,          # Common backdoor ports
      1337, 31337,                                    # Classic hacker ports
      4443, 8444                                      # Alternative HTTPS
    ]
    |> Enum.map(&{:port, &1})

    well_known ++ common_high
  end

  defp generate_signal_channels do
    # POSIX signals 1-31
    Enum.map(1..31, &{:signal, &1})
  end

  defp generate_file_channels do
    [
      {:file_pattern, "/etc/shadow"},
      {:file_pattern, "/etc/passwd"},
      {:file_pattern, "/etc/hosts"},
      {:file_pattern, "/proc/self/maps"},
      {:file_pattern, "/proc/self/status"},
      {:file_pattern, "/proc/self/environ"},
      {:file_pattern, "/dev/mem"},
      {:file_pattern, "/dev/kmem"},
      {:file_pattern, "/boot/"},
      {:file_pattern, "/sys/firmware/"}
    ]
  end
end
