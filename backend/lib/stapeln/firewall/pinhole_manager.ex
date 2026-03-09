# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Stapeln.Firewall.PinholeManager do
  @moduledoc """
  GenServer managing active ephemeral pinhole firewall rules.

  Pinholes are temporary, scoped network access rules that auto-expire
  after their TTL. All operations are logged for audit purposes.
  """

  use GenServer

  alias Stapeln.Firewall.Pinhole

  require Logger

  # Maximum TTL: 24 hours
  @max_ttl_seconds 86_400

  # Periodic sweep interval: 10 seconds
  @sweep_interval_ms 10_000

  # ---------------------------------------------------------------------------
  # Public API
  # ---------------------------------------------------------------------------

  @doc """
  Starts the PinholeManager GenServer.
  """
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Creates a new ephemeral pinhole.

  Returns `{:ok, pinhole}` on success or `{:error, reason}` on failure.
  TTL is clamped to a maximum of 24 hours.
  """
  @spec create(String.t(), String.t(), non_neg_integer(), non_neg_integer(), String.t(), String.t()) ::
          {:ok, Pinhole.t()} | {:error, String.t()}
  def create(source, destination, port, ttl_seconds, reason, protocol \\ "tcp") do
    GenServer.call(__MODULE__, {:create, source, destination, port, ttl_seconds, reason, protocol})
  end

  @doc """
  Lists all currently active (non-expired, non-revoked) pinholes.
  """
  @spec list_active() :: [Pinhole.t()]
  def list_active do
    GenServer.call(__MODULE__, :list_active)
  end

  @doc """
  Lists all pinholes including expired and revoked ones.
  """
  @spec list_all() :: [Pinhole.t()]
  def list_all do
    GenServer.call(__MODULE__, :list_all)
  end

  @doc """
  Revokes a pinhole early by its ID.

  Returns `{:ok, pinhole}` if found and revoked, `{:error, reason}` otherwise.
  """
  @spec revoke(String.t()) :: {:ok, Pinhole.t()} | {:error, String.t()}
  def revoke(pinhole_id) do
    GenServer.call(__MODULE__, {:revoke, pinhole_id})
  end

  @doc """
  Checks whether traffic from `source` to `destination` on `port` is
  allowed by any active pinhole.
  """
  @spec allowed?(String.t(), String.t(), non_neg_integer()) :: boolean()
  def allowed?(source, destination, port) do
    GenServer.call(__MODULE__, {:allowed?, source, destination, port})
  end

  # ---------------------------------------------------------------------------
  # GenServer callbacks
  # ---------------------------------------------------------------------------

  @impl true
  def init(_opts) do
    schedule_sweep()
    {:ok, %{pinholes: %{}}}
  end

  @impl true
  def handle_call({:create, source, destination, port, ttl_seconds, reason, protocol}, _from, state) do
    clamped_ttl = min(ttl_seconds, @max_ttl_seconds)

    pinhole = Pinhole.new(source, destination, port, protocol, clamped_ttl, reason)

    Logger.info(
      "[PinholeManager] Created pinhole #{pinhole.id}: " <>
        "#{source} -> #{destination}:#{port}/#{protocol} TTL=#{clamped_ttl}s reason=#{reason}"
    )

    schedule_expiry(pinhole)

    new_state = put_in(state, [:pinholes, pinhole.id], pinhole)
    {:reply, {:ok, pinhole}, new_state}
  end

  def handle_call(:list_active, _from, state) do
    active =
      state.pinholes
      |> Map.values()
      |> Enum.filter(&(&1.status == :active and not Pinhole.expired?(&1)))

    {:reply, active, state}
  end

  def handle_call(:list_all, _from, state) do
    all = Map.values(state.pinholes)
    {:reply, all, state}
  end

  def handle_call({:revoke, pinhole_id}, _from, state) do
    case Map.get(state.pinholes, pinhole_id) do
      nil ->
        {:reply, {:error, "Pinhole not found"}, state}

      %Pinhole{status: status} when status in [:expired, :revoked] ->
        {:reply, {:error, "Pinhole already #{status}"}, state}

      pinhole ->
        revoked = Pinhole.revoke(pinhole)

        Logger.info("[PinholeManager] Revoked pinhole #{pinhole_id}")

        new_state = put_in(state, [:pinholes, pinhole_id], revoked)
        {:reply, {:ok, revoked}, new_state}
    end
  end

  def handle_call({:allowed?, source, destination, port}, _from, state) do
    allowed =
      state.pinholes
      |> Map.values()
      |> Enum.any?(fn p ->
        p.status == :active and
          not Pinhole.expired?(p) and
          p.source == source and
          p.destination == destination and
          p.port == port
      end)

    {:reply, allowed, state}
  end

  @impl true
  def handle_info({:expire_pinhole, pinhole_id}, state) do
    case Map.get(state.pinholes, pinhole_id) do
      %Pinhole{status: :active} = pinhole ->
        expired = Pinhole.expire(pinhole)

        Logger.info("[PinholeManager] Expired pinhole #{pinhole_id}")

        new_state = put_in(state, [:pinholes, pinhole_id], expired)
        {:noreply, new_state}

      _ ->
        {:noreply, state}
    end
  end

  def handle_info(:sweep, state) do
    now = DateTime.utc_now()

    updated_pinholes =
      Map.new(state.pinholes, fn {id, pinhole} ->
        if pinhole.status == :active and DateTime.compare(now, pinhole.expires_at) != :lt do
          Logger.info("[PinholeManager] Sweep expired pinhole #{id}")
          {id, Pinhole.expire(pinhole)}
        else
          {id, pinhole}
        end
      end)

    schedule_sweep()
    {:noreply, %{state | pinholes: updated_pinholes}}
  end

  # ---------------------------------------------------------------------------
  # Internal helpers
  # ---------------------------------------------------------------------------

  # Schedule a message to expire a specific pinhole at its TTL.
  defp schedule_expiry(%Pinhole{id: id, ttl_seconds: ttl}) do
    Process.send_after(self(), {:expire_pinhole, id}, ttl * 1_000)
  end

  # Schedule the periodic sweep to catch any pinholes that slipped through.
  defp schedule_sweep do
    Process.send_after(self(), :sweep, @sweep_interval_ms)
  end
end
