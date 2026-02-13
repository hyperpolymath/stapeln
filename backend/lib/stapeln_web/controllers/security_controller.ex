defmodule StapelnWeb.SecurityController do
  use StapelnWeb, :controller

  alias Stapeln.Security

  action_fallback StapelnWeb.ErrorJson

  def start(conn, params) do
    command = Map.get(params, "command", "ambush")
    target = Map.get(params, "target", "/bin/true")
    schedule = Map.get(params, "schedule", "one-off")

    with {:ok, state} <- Security.delegate_trace(command, target, schedule) do
      json(conn, state)
    else
      {:error, reason, fallback} ->
        conn
        |> put_status(:conflict)
        |> json(%{error: reason, info: fallback})
    end
  end

  def stop(conn, _params) do
    with {:ok, state} <- Security.stop_trace() do
      json(conn, state)
    end
  end

  def status(conn, _params) do
    json(conn, Security.trace_status())
  end
end
