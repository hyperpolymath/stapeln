defmodule StapelnWeb.HealthController do
  use StapelnWeb, :controller

  alias Stapeln.NativeBridge

  def show(conn, _params) do
    json(conn, %{
      status: "ok",
      backend: Atom.to_string(NativeBridge.backend()),
      timestamp: DateTime.utc_now() |> DateTime.truncate(:second) |> DateTime.to_iso8601()
    })
  end
end
