# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule SvalinnWeb.HealthController do
  use SvalinnWeb, :controller

  alias Svalinn.VordrAdapter

  @doc """
  Health check endpoint.

  Returns overall health status including Vörðr connection.
  """
  def health(conn, _params) do
    vordr_connected = VordrAdapter.connected?()

    status = if vordr_connected, do: "healthy", else: "degraded"

    json(conn, %{
      status: status,
      version: Application.spec(:svalinn, :vsn) |> to_string(),
      vordr_connected: vordr_connected,
      vordr_mode: VordrAdapter.mode(),
      vordr_mode_description: VordrAdapter.mode_description(),
      timestamp: DateTime.utc_now()
    })
  end

  @doc """
  Readiness check endpoint.

  Returns ready only if Vörðr is accessible.
  """
  def ready(conn, _params) do
    if VordrAdapter.connected?() do
      json(conn, %{
        ready: true,
        mode: VordrAdapter.mode()
      })
    else
      conn
      |> put_status(503)
      |> json(%{
        ready: false,
        reason: "Vörðr unavailable"
      })
    end
  end

  @doc """
  Adapter information endpoint.

  Returns details about the current VordrAdapter configuration.
  """
  def adapter_info(conn, _params) do
    json(conn, VordrAdapter.health())
  end
end
