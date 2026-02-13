# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule SvalinnWeb.ContainerController do
  use SvalinnWeb, :controller

  alias Svalinn.VordrAdapter

  require Logger

  @doc """
  List all containers.

  GET /api/v1/containers
  """
  def index(conn, _params) do
    case VordrAdapter.list_containers() do
      {:ok, containers} ->
        json(conn, %{containers: containers})

      {:error, reason} ->
        Logger.error("Failed to list containers: #{inspect(reason)}")

        conn
        |> put_status(500)
        |> json(%{error: "Failed to list containers", details: inspect(reason)})
    end
  end

  @doc """
  Get a specific container by ID.

  GET /api/v1/containers/:id
  """
  def show(conn, %{"id" => id}) do
    case VordrAdapter.get_container(id) do
      {:ok, container} ->
        json(conn, container)

      {:error, :not_found} ->
        conn
        |> put_status(404)
        |> json(%{error: "Container not found", id: id})

      {:error, reason} ->
        Logger.error("Failed to get container #{id}: #{inspect(reason)}")

        conn
        |> put_status(500)
        |> json(%{error: "Failed to get container", details: inspect(reason)})
    end
  end

  @doc """
  Create and start a new container.

  POST /api/v1/containers
  Body: {
    "imageName": "alpine:latest",
    "imageDigest": "sha256:...",
    "name": "my-container",  // optional
    "containerConfig": {...} // optional
  }
  """
  def create(conn, params) do
    with {:ok, validated} <- validate_create_request(params),
         {:ok, container} <- VordrAdapter.create_container(
           validated.image_name,
           name: validated.name,
           config: validated.config
         ) do
      conn
      |> put_status(201)
      |> json(container)
    else
      {:error, :validation, errors} ->
        conn
        |> put_status(400)
        |> json(%{error: "Validation failed", details: errors})

      {:error, reason} ->
        Logger.error("Failed to create container: #{inspect(reason)}")

        conn
        |> put_status(500)
        |> json(%{error: "Failed to create container", details: inspect(reason)})
    end
  end

  @doc """
  Stop a running container.

  DELETE /api/v1/containers/:id
  Query params:
    - timeout: graceful shutdown timeout in seconds (default: 10)
  """
  def delete(conn, %{"id" => id} = params) do
    timeout = Map.get(params, "timeout", 10)

    with :ok <- VordrAdapter.stop_container(id, timeout: timeout),
         :ok <- VordrAdapter.remove_container(id) do
      send_resp(conn, 204, "")
    else
      {:error, :not_found} ->
        conn
        |> put_status(404)
        |> json(%{error: "Container not found", id: id})

      {:error, reason} ->
        Logger.error("Failed to delete container #{id}: #{inspect(reason)}")

        conn
        |> put_status(500)
        |> json(%{error: "Failed to delete container", details: inspect(reason)})
    end
  end

  # ─────────────────────────────────────────────────────────────────────────
  # Request Validation
  # ─────────────────────────────────────────────────────────────────────────

  defp validate_create_request(params) do
    with {:ok, image_name} <- fetch_required(params, "imageName"),
         {:ok, image_digest} <- fetch_required(params, "imageDigest") do
      {:ok, %{
        image_name: image_name,
        image_digest: image_digest,
        name: Map.get(params, "name"),
        config: Map.get(params, "containerConfig")
      }}
    else
      {:error, field} ->
        {:error, :validation, "Missing required field: #{field}"}
    end
  end

  defp fetch_required(params, key) do
    case Map.fetch(params, key) do
      {:ok, value} when is_binary(value) and value != "" ->
        {:ok, value}

      _ ->
        {:error, key}
    end
  end
end
