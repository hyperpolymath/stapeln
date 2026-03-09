# SPDX-License-Identifier: PMPL-1.0-or-later
# settings_store.ex - In-memory settings storage for stapeln

defmodule Stapeln.SettingsStore do
  @moduledoc """
  In-memory settings store backed by a GenServer.

  Tries Ecto-backed `Stapeln.DbStore` when PostgreSQL is available, and
  falls back to the GenServer state otherwise.

  Settings persist to /tmp/stapeln-settings.json for dev convenience.
  """

  use GenServer

  alias Stapeln.DbStore

  @name __MODULE__
  @persist_path "/tmp/stapeln-settings.json"

  @default_settings %{
    "theme" => "dark",
    "defaultRuntime" => "podman",
    "autoSave" => false,
    "backendUrl" => "/api"
  }

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: @name)
  end

  @spec get() :: map()
  def get do
    if DbStore.available?() do
      DbStore.get_settings(nil)
    else
      GenServer.call(@name, :get)
    end
  end

  @spec update(map()) :: {:ok, map()} | {:error, term()}
  def update(attrs) when is_map(attrs) do
    if DbStore.available?() do
      DbStore.update_settings(nil, attrs)
    else
      GenServer.call(@name, {:update, attrs})
    end
  end

  @impl true
  def init(_opts) do
    {:ok, load_state()}
  end

  @impl true
  def handle_call(:get, _from, state) do
    {:reply, state, state}
  end

  def handle_call({:update, attrs}, _from, state) do
    new_state = Map.merge(state, attrs)
    persist(new_state)
    {:reply, {:ok, new_state}, new_state}
  end

  defp load_state do
    case File.read(@persist_path) do
      {:ok, body} ->
        case Jason.decode(body) do
          {:ok, settings} when is_map(settings) ->
            Map.merge(@default_settings, settings)

          _ ->
            @default_settings
        end

      _ ->
        @default_settings
    end
  end

  defp persist(state) do
    with encoded <- Jason.encode!(state) do
      File.write(@persist_path, encoded)
    end
  end
end
