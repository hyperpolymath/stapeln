# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)

defmodule Stapeln.VeriSimDB.Client do
  @moduledoc """
  HTTP client for communicating with a remote VeriSimDB instance.

  Connects when the `VERISIMDB_URL` environment variable is set (e.g.
  `http://localhost:9420`).  All writes and queries go through the VeriSimDB
  REST API.  When the URL is not configured or the instance is unreachable,
  every function returns `{:error, reason}` so the caller can fall back to
  local storage.
  """

  @write_path "/api/v1/audit"
  @query_path "/api/v1/audit"
  @connect_timeout 5_000
  @receive_timeout 10_000

  @doc """
  Write an audit entry to the remote VeriSimDB instance.

  Returns `:ok` on success or `{:error, reason}` if the instance is
  unconfigured or unreachable.
  """
  @spec write(map()) :: :ok | {:error, term()}
  def write(entry) when is_map(entry) do
    with {:ok, base_url} <- verisimdb_url(),
         url <- base_url <> @write_path,
         {:ok, body} <- encode(entry) do
      case post(url, body) do
        {:ok, %{status: status}} when status in 200..299 ->
          :ok

        {:ok, %{status: status}} ->
          {:error, {:verisimdb_status, status}}

        {:error, reason} ->
          {:error, {:verisimdb_request, reason}}
      end
    end
  end

  @doc """
  Query audit entries from the remote VeriSimDB instance.

  Supported `opts`:
  - `:event_type` — atom, filter by event type
  - `:since`      — ISO 8601 string
  - `:until`      — ISO 8601 string
  - `:limit`      — integer, max results

  Returns `{:ok, [map()]}` on success or `{:error, reason}`.
  """
  @spec query(keyword()) :: {:ok, [map()]} | {:error, term()}
  def query(opts \\ []) do
    with {:ok, base_url} <- verisimdb_url(),
         url <- base_url <> @query_path <> query_string(opts) do
      case get(url) do
        {:ok, %{status: status, body: body}} when status in 200..299 ->
          decode_entries(body)

        {:ok, %{status: status}} ->
          {:error, {:verisimdb_status, status}}

        {:error, reason} ->
          {:error, {:verisimdb_request, reason}}
      end
    end
  end

  # ---------------------------------------------------------------------------
  # HTTP Helpers (uses Req, already a project dependency)
  # ---------------------------------------------------------------------------

  defp post(url, body) do
    Req.post(url,
      body: body,
      headers: [{"content-type", "application/json"}, {"accept", "application/json"}],
      connect_options: [timeout: @connect_timeout],
      receive_timeout: @receive_timeout
    )
  rescue
    error -> {:error, {:http_error, error}}
  end

  defp get(url) do
    Req.get(url,
      headers: [{"accept", "application/json"}],
      connect_options: [timeout: @connect_timeout],
      receive_timeout: @receive_timeout
    )
  rescue
    error -> {:error, {:http_error, error}}
  end

  # ---------------------------------------------------------------------------
  # Encoding / Decoding
  # ---------------------------------------------------------------------------

  defp encode(entry) do
    case Jason.encode(entry) do
      {:ok, _json} = ok -> ok
      {:error, reason} -> {:error, {:encode_error, reason}}
    end
  end

  defp decode_entries(body) when is_binary(body) do
    case Jason.decode(body) do
      {:ok, %{"data" => entries}} when is_list(entries) -> {:ok, entries}
      {:ok, entries} when is_list(entries) -> {:ok, entries}
      {:ok, _other} -> {:ok, []}
      {:error, reason} -> {:error, {:decode_error, reason}}
    end
  end

  defp decode_entries(body) when is_list(body), do: {:ok, body}
  defp decode_entries(body) when is_map(body), do: {:ok, Map.get(body, "data", [])}
  defp decode_entries(_), do: {:ok, []}

  # ---------------------------------------------------------------------------
  # Configuration
  # ---------------------------------------------------------------------------

  defp verisimdb_url do
    case System.get_env("VERISIMDB_URL") do
      nil -> {:error, :verisimdb_not_configured}
      "" -> {:error, :verisimdb_not_configured}
      url -> {:ok, String.trim_trailing(url, "/")}
    end
  end

  defp query_string(opts) do
    params =
      []
      |> maybe_add("event_type", Keyword.get(opts, :event_type), &Atom.to_string/1)
      |> maybe_add("since", Keyword.get(opts, :since), & &1)
      |> maybe_add("until", Keyword.get(opts, :until), & &1)
      |> maybe_add("limit", Keyword.get(opts, :limit), &Integer.to_string/1)

    case params do
      [] -> ""
      pairs -> "?" <> Enum.join(pairs, "&")
    end
  end

  defp maybe_add(acc, _key, nil, _transform), do: acc

  defp maybe_add(acc, key, value, transform) do
    acc ++ ["#{key}=#{URI.encode_www_form(transform.(value))}"]
  end
end
