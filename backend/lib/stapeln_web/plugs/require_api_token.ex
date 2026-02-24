defmodule StapelnWeb.Plugs.RequireApiToken do
  @moduledoc false

  import Plug.Conn

  alias Stapeln.Auth.ApiToken

  def init(opts), do: opts

  def call(conn, _opts) do
    case ApiToken.authorize_headers(conn.req_headers) do
      :ok ->
        conn

      {:error, :token_not_configured} ->
        conn
        |> send_json_error(500, "API authentication is not configured")
        |> halt()

      {:error, _reason} ->
        conn
        |> put_resp_header("www-authenticate", "Bearer")
        |> send_json_error(401, "missing or invalid API token")
        |> halt()
    end
  end

  defp send_json_error(conn, status, message) do
    body = Jason.encode!(%{error: message})

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(status, body)
  end
end
