# SPDX-License-Identifier: PMPL-1.0-or-later
# settings_controller.ex - User settings endpoints for stapeln

defmodule StapelnWeb.SettingsController do
  use StapelnWeb, :controller

  alias Stapeln.SettingsStore

  @doc "GET /api/settings"
  def show(conn, _params) do
    settings = SettingsStore.get()
    json(conn, %{data: settings})
  end

  @doc "PUT /api/settings"
  def update(conn, params) do
    # Accept settings from the top-level params (Phoenix strips the wrapper)
    settings =
      params
      |> Map.take(["theme", "defaultRuntime", "autoSave", "backendUrl"])
      |> Enum.reject(fn {_k, v} -> is_nil(v) end)
      |> Map.new()

    case SettingsStore.update(settings) do
      {:ok, updated} ->
        json(conn, %{data: updated})

      {:error, reason} ->
        conn
        |> put_status(:bad_request)
        |> json(%{error: to_string(reason)})
    end
  end
end
