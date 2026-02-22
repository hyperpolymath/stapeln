# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Vordr.Temporal.PortSwarm do
  @moduledoc """
  BEAM Port Interceptor Swarm

  Spawns one lightweight BEAM process per port (up to 65,535).
  Each process listens on a single port and logs all connection attempts.
  Pre-spawned before the target process starts, so the target sees
  services already listening — indistinguishable from normal deployment.

  ## Why BEAM?

  BEAM processes are ~300 bytes each. 65,535 interceptors consume
  approximately 20MB of memory — negligible for a monitoring system.
  BEAM spawns processes in microseconds, so the entire swarm is ready
  before the target code executes its first instruction.

  ## Detection resistance

  In any real deployment, most ports have something listening
  (web servers, databases, message brokers, health checks).
  Port interceptors look like normal services — they are
  environmental noise, not monitoring artefacts.

  ## Integration

  Port swarm events feed into the eBPF event consumer via
  the selur IPC bridge. The Rust eBPF monitor receives
  `NetworkEvent` structs for correlation with syscall events.
  """

  use GenServer

  require Logger

  @default_port_range 1..65_535
  @listen_backlog 1

  # ── Types ──────────────────────────────────────────────

  @type interceptor_id :: pos_integer()
  @type connection_log :: %{
          port: pos_integer(),
          source_ip: String.t(),
          source_port: pos_integer(),
          timestamp: DateTime.t(),
          data_preview: binary()
        }

  # ── Public API ─────────────────────────────────────────

  @doc """
  Start the port swarm supervisor.

  ## Options

  - `:port_range` — Range of ports to intercept (default: 1..65_535)
  - `:exclude_ports` — Ports to skip (e.g. SSH on 22, or the BEAM node port)
  - `:collector_pid` — PID to receive connection events
  - `:log_data` — Whether to capture initial bytes of data (default: false)
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Spawn interceptors across the port range.

  Returns the count of successfully spawned interceptors.
  Ports that are already in use are silently skipped (they
  already have a legitimate service — no need to intercept).
  """
  @spec spawn_swarm() :: {:ok, non_neg_integer()}
  def spawn_swarm do
    GenServer.call(__MODULE__, :spawn_swarm, :infinity)
  end

  @doc """
  Stop all interceptors and release ports.
  """
  @spec stop_swarm() :: :ok
  def stop_swarm do
    GenServer.call(__MODULE__, :stop_swarm)
  end

  @doc """
  Get all connection events logged so far.
  """
  @spec get_connections() :: [connection_log()]
  def get_connections do
    GenServer.call(__MODULE__, :get_connections)
  end

  @doc """
  Get count of active interceptors.
  """
  @spec active_count() :: non_neg_integer()
  def active_count do
    GenServer.call(__MODULE__, :active_count)
  end

  @doc """
  Get connection count per port (only ports that received connections).
  """
  @spec connection_summary() :: %{pos_integer() => non_neg_integer()}
  def connection_summary do
    GenServer.call(__MODULE__, :connection_summary)
  end

  # ── GenServer callbacks ────────────────────────────────

  @impl true
  def init(opts) do
    port_range = Keyword.get(opts, :port_range, @default_port_range)
    exclude_ports = Keyword.get(opts, :exclude_ports, [])
    collector_pid = Keyword.get(opts, :collector_pid, self())
    log_data = Keyword.get(opts, :log_data, false)

    state = %{
      port_range: port_range,
      exclude_ports: MapSet.new(exclude_ports),
      collector_pid: collector_pid,
      log_data: log_data,
      interceptors: %{},
      connections: [],
      spawned_count: 0
    }

    {:ok, state}
  end

  @impl true
  def handle_call(:spawn_swarm, _from, state) do
    Logger.info("Spawning port interceptor swarm across #{inspect(state.port_range)}")

    {interceptors, count} = spawn_interceptors(state)

    Logger.info("Swarm ready: #{count} interceptors active (#{map_size(state.exclude_ports)} ports excluded)")

    new_state = %{state | interceptors: interceptors, spawned_count: count}
    {:reply, {:ok, count}, new_state}
  end

  @impl true
  def handle_call(:stop_swarm, _from, state) do
    Logger.info("Stopping port interceptor swarm (#{map_size(state.interceptors)} active)")

    Enum.each(state.interceptors, fn {_port, {socket, _pid}} ->
      :gen_tcp.close(socket)
    end)

    {:reply, :ok, %{state | interceptors: %{}, spawned_count: 0}}
  end

  @impl true
  def handle_call(:get_connections, _from, state) do
    {:reply, Enum.reverse(state.connections), state}
  end

  @impl true
  def handle_call(:active_count, _from, state) do
    {:reply, map_size(state.interceptors), state}
  end

  @impl true
  def handle_call(:connection_summary, _from, state) do
    summary =
      state.connections
      |> Enum.group_by(& &1.port)
      |> Enum.map(fn {port, conns} -> {port, length(conns)} end)
      |> Map.new()

    {:reply, summary, state}
  end

  @impl true
  def handle_info({:connection, port, source_ip, source_port, data_preview}, state) do
    log_entry = %{
      port: port,
      source_ip: source_ip,
      source_port: source_port,
      timestamp: DateTime.utc_now(),
      data_preview: data_preview
    }

    Logger.debug("Connection intercepted on port #{port} from #{source_ip}:#{source_port}")

    # Forward to collector (e.g. eBPF event consumer)
    if state.collector_pid != self() do
      send(state.collector_pid, {:intercepted_connection, log_entry})
    end

    {:noreply, %{state | connections: [log_entry | state.connections]}}
  end

  @impl true
  def handle_info(_msg, state) do
    {:noreply, state}
  end

  # ── Private ────────────────────────────────────────────

  defp spawn_interceptors(state) do
    state.port_range
    |> Enum.reject(&MapSet.member?(state.exclude_ports, &1))
    |> Enum.reduce({%{}, 0}, fn port, {interceptors, count} ->
      case listen_on_port(port, state) do
        {:ok, socket, acceptor_pid} ->
          {Map.put(interceptors, port, {socket, acceptor_pid}), count + 1}

        {:error, :eaddrinuse} ->
          # Port already in use — legitimate service, skip silently
          {interceptors, count}

        {:error, :eacces} ->
          # Permission denied (ports < 1024 need root), skip
          {interceptors, count}

        {:error, reason} ->
          Logger.debug("Could not bind port #{port}: #{inspect(reason)}")
          {interceptors, count}
      end
    end)
  end

  defp listen_on_port(port, state) do
    opts = [
      :binary,
      active: false,
      reuseaddr: true,
      backlog: @listen_backlog
    ]

    case :gen_tcp.listen(port, opts) do
      {:ok, socket} ->
        # Spawn acceptor process for this port
        parent = self()
        log_data = state.log_data

        acceptor_pid =
          spawn_link(fn ->
            accept_loop(socket, port, parent, log_data)
          end)

        {:ok, socket, acceptor_pid}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp accept_loop(listen_socket, port, parent, log_data) do
    case :gen_tcp.accept(listen_socket, 5000) do
      {:ok, client_socket} ->
        # Get peer info
        {source_ip, source_port} =
          case :inet.peername(client_socket) do
            {:ok, {ip, p}} -> {format_ip(ip), p}
            _ -> {"unknown", 0}
          end

        # Optionally capture initial data
        data_preview =
          if log_data do
            case :gen_tcp.recv(client_socket, 0, 1000) do
              {:ok, data} -> data
              _ -> <<>>
            end
          else
            <<>>
          end

        # Close the connection (we only needed to observe it)
        :gen_tcp.close(client_socket)

        # Report to parent
        send(parent, {:connection, port, source_ip, source_port, data_preview})

        # Continue accepting
        accept_loop(listen_socket, port, parent, log_data)

      {:error, :timeout} ->
        # No connection within timeout, loop back
        accept_loop(listen_socket, port, parent, log_data)

      {:error, :closed} ->
        # Socket closed, swarm is stopping
        :ok

      {:error, _reason} ->
        # Other error, stop this acceptor
        :ok
    end
  end

  defp format_ip({a, b, c, d}), do: "#{a}.#{b}.#{c}.#{d}"

  defp format_ip({a, b, c, d, e, f, g, h}),
    do: "#{Integer.to_string(a, 16)}:#{Integer.to_string(b, 16)}:#{Integer.to_string(c, 16)}:#{Integer.to_string(d, 16)}:#{Integer.to_string(e, 16)}:#{Integer.to_string(f, 16)}:#{Integer.to_string(g, 16)}:#{Integer.to_string(h, 16)}"
end
