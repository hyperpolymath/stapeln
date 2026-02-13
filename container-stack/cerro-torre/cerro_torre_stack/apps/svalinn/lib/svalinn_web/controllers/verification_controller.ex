# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule SvalinnWeb.VerificationController do
  use SvalinnWeb, :controller

  alias Svalinn.VordrAdapter

  require Logger

  @doc """
  Verify an image with signature and SBOM checks.

  POST /api/v1/verify
  Body: {
    "imageName": "alpine:latest",
    "imageDigest": "sha256:..."
  }
  """
  def verify(conn, params) do
    with {:ok, image_name} <- fetch_required(params, "imageName"),
         {:ok, image_digest} <- fetch_required(params, "imageDigest"),
         {:ok, result} <- VordrAdapter.verify_image(image_name, image_digest) do
      json(conn, result)
    else
      {:error, :validation, field} ->
        conn
        |> put_status(400)
        |> json(%{error: "Missing required field", field: field})

      {:error, reason} ->
        Logger.error("Image verification failed: #{inspect(reason)}")

        conn
        |> put_status(500)
        |> json(%{error: "Verification failed", details: inspect(reason)})
    end
  end

  # ─────────────────────────────────────────────────────────────────────────
  # Helpers
  # ─────────────────────────────────────────────────────────────────────────

  defp fetch_required(params, key) do
    case Map.fetch(params, key) do
      {:ok, value} when is_binary(value) and value != "" ->
        {:ok, value}

      _ ->
        {:error, :validation, key}
    end
  end
end
