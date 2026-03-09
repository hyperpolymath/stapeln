defmodule StapelnWeb.Plugs.RequireApiToken do
  @moduledoc false

  import Plug.Conn

  alias Stapeln.Auth.ApiToken

  def init(opts), do: opts

  def call(conn, _opts) do
    # Try static API token first, then fall back to JWT
    case ApiToken.authorize_headers(conn.req_headers) do
      :ok ->
        conn

      {:error, :token_not_configured} ->
        # No static token configured — try JWT
        try_jwt(conn)

      {:error, _reason} ->
        # Static token check failed — try JWT before rejecting
        try_jwt(conn)
    end
  end

  defp try_jwt(conn) do
    case Plug.Conn.get_req_header(conn, "authorization") do
      ["Bearer " <> token | _] ->
        case Stapeln.Auth.Token.verify(String.trim(token)) do
          {:ok, user_id} ->
            Plug.Conn.assign(conn, :current_user_id, user_id)

          {:error, _} ->
            reject(conn)
        end

      _ ->
        reject(conn)
    end
  end

  defp reject(conn) do
    conn
    |> put_resp_header("www-authenticate", "Bearer")
    |> send_json_error(401, "missing or invalid API token")
    |> halt()
  end

  defp send_json_error(conn, status, message) do
    body = Jason.encode!(%{error: message})

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(status, body)
  end
end
