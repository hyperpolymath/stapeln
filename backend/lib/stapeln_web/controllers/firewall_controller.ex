# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule StapelnWeb.FirewallController do
  @moduledoc """
  Controller for ephemeral pinhole firewall management.

  Exposes REST endpoints for creating, listing, revoking, and
  checking ephemeral pinhole rules.
  """

  use StapelnWeb, :controller

  alias Stapeln.Firewall.{Pinhole, PinholeManager}

  @doc """
  POST /api/firewall/pinholes

  Create a new ephemeral pinhole. Expects JSON body:

      {
        "source": "web-app",
        "destination": "postgres-primary",
        "port": 5432,
        "ttl_seconds": 300,
        "reason": "Database migration",
        "protocol": "tcp"
      }
  """
  def create(conn, params) do
    with {:ok, source} <- require_param(params, "source"),
         {:ok, destination} <- require_param(params, "destination"),
         {:ok, port} <- require_int_param(params, "port"),
         {:ok, ttl} <- require_int_param(params, "ttl_seconds"),
         {:ok, reason} <- require_param(params, "reason") do
      protocol = Map.get(params, "protocol", "tcp")

      case PinholeManager.create(source, destination, port, ttl, reason, protocol) do
        {:ok, pinhole} ->
          conn
          |> put_status(:created)
          |> json(%{pinhole: Pinhole.to_map(pinhole)})

        {:error, reason} ->
          conn
          |> put_status(:unprocessable_entity)
          |> json(%{error: reason})
      end
    else
      {:error, msg} ->
        conn
        |> put_status(:bad_request)
        |> json(%{error: msg})
    end
  end

  @doc """
  GET /api/firewall/pinholes

  List all active pinholes.
  """
  def index(conn, _params) do
    pinholes = PinholeManager.list_active()
    json(conn, %{pinholes: Enum.map(pinholes, &Pinhole.to_map/1)})
  end

  @doc """
  DELETE /api/firewall/pinholes/:id

  Revoke a pinhole early.
  """
  def delete(conn, %{"id" => id}) do
    case PinholeManager.revoke(id) do
      {:ok, pinhole} ->
        json(conn, %{pinhole: Pinhole.to_map(pinhole)})

      {:error, reason} ->
        conn
        |> put_status(:not_found)
        |> json(%{error: reason})
    end
  end

  @doc """
  POST /api/firewall/check

  Check if traffic is allowed. Expects JSON body:

      {
        "source": "web-app",
        "destination": "postgres-primary",
        "port": 5432
      }
  """
  def check(conn, params) do
    with {:ok, source} <- require_param(params, "source"),
         {:ok, destination} <- require_param(params, "destination"),
         {:ok, port} <- require_int_param(params, "port") do
      allowed = PinholeManager.allowed?(source, destination, port)
      json(conn, %{allowed: allowed, source: source, destination: destination, port: port})
    else
      {:error, msg} ->
        conn
        |> put_status(:bad_request)
        |> json(%{error: msg})
    end
  end

  # ---------------------------------------------------------------------------
  # Parameter helpers
  # ---------------------------------------------------------------------------

  defp require_param(params, key) do
    case Map.get(params, key) do
      nil -> {:error, "Missing required parameter: #{key}"}
      "" -> {:error, "Parameter #{key} cannot be empty"}
      value when is_binary(value) -> {:ok, value}
      _other -> {:error, "Parameter #{key} must be a string"}
    end
  end

  defp require_int_param(params, key) do
    case Map.get(params, key) do
      nil -> {:error, "Missing required parameter: #{key}"}
      value when is_integer(value) and value > 0 -> {:ok, value}
      value when is_binary(value) ->
        case Integer.parse(value) do
          {n, ""} when n > 0 -> {:ok, n}
          _ -> {:error, "Parameter #{key} must be a positive integer"}
        end
      _ -> {:error, "Parameter #{key} must be a positive integer"}
    end
  end
end
