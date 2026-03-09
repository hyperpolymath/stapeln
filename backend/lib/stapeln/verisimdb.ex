# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)

defmodule Stapeln.VeriSimDB do
  @moduledoc """
  VeriSimDB audit trail integration for stapeln.

  Records all significant events (stack changes, security scans, gap analyses)
  as immutable audit entries. When a VeriSimDB instance is not available,
  falls back to a local JSON append-log at /tmp/stapeln-audit.jsonl.
  """

  alias Stapeln.VeriSimDB.Client

  @audit_log_path "/tmp/stapeln-audit.jsonl"

  @type event_type ::
          :stack_created
          | :stack_updated
          | :stack_deleted
          | :security_scan
          | :gap_analysis
          | :validation

  @doc """
  Record an audit event.

  Attempts to write to a remote VeriSimDB instance first (if `VERISIMDB_URL`
  is set).  Falls back to a local JSONL append-log when the remote is
  unreachable or unconfigured.
  """
  @spec record(event_type(), map()) :: :ok
  def record(event_type, payload) when is_atom(event_type) and is_map(payload) do
    entry = build_entry(event_type, payload)

    case Client.write(entry) do
      :ok -> :ok
      {:error, _reason} -> write_to_local_log(entry)
    end
  end

  @doc """
  Query audit entries with optional filters.

  Supported options:
  - `:event_type`  — filter by event type atom
  - `:since`       — ISO 8601 lower bound (inclusive)
  - `:until`       — ISO 8601 upper bound (inclusive)
  - `:limit`       — max entries to return (default 100)
  """
  @spec query(keyword()) :: {:ok, [map()]}
  def query(opts \\ []) do
    case Client.query(opts) do
      {:ok, _entries} = ok ->
        ok

      {:error, _reason} ->
        query_local_log(opts)
    end
  end

  # ---------------------------------------------------------------------------
  # Entry Construction
  # ---------------------------------------------------------------------------

  defp build_entry(event_type, payload) do
    %{
      id: generate_id(),
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
      event_type: event_type,
      payload: payload,
      source: "stapeln",
      version: "0.4.0"
    }
  end

  defp generate_id do
    # 128-bit hex string — collision-free for audit purposes.
    :crypto.strong_rand_bytes(16) |> Base.hex_encode32(case: :lower, padding: false)
  end

  # ---------------------------------------------------------------------------
  # Local JSONL Fallback — Write
  # ---------------------------------------------------------------------------

  defp write_to_local_log(entry) do
    log_path = audit_log_path()

    with :ok <- ensure_parent_dir(log_path),
         line <- Jason.encode!(serialise_entry(entry)),
         :ok <- File.write(log_path, line <> "\n", [:append, :utf8]) do
      :ok
    else
      _ -> :ok
    end
  end

  # ---------------------------------------------------------------------------
  # Local JSONL Fallback — Query
  # ---------------------------------------------------------------------------

  defp query_local_log(opts) do
    log_path = audit_log_path()

    entries =
      case File.read(log_path) do
        {:ok, body} ->
          body
          |> String.split("\n", trim: true)
          |> Enum.flat_map(fn line ->
            case Jason.decode(line) do
              {:ok, decoded} -> [decoded]
              _ -> []
            end
          end)

        {:error, _} ->
          []
      end

    filtered = apply_filters(entries, opts)
    {:ok, filtered}
  end

  defp apply_filters(entries, opts) do
    entries
    |> filter_event_type(Keyword.get(opts, :event_type))
    |> filter_since(Keyword.get(opts, :since))
    |> filter_until(Keyword.get(opts, :until))
    |> Enum.take(Keyword.get(opts, :limit, 100))
  end

  defp filter_event_type(entries, nil), do: entries

  defp filter_event_type(entries, event_type) do
    type_str = Atom.to_string(event_type)
    Enum.filter(entries, fn e -> Map.get(e, "event_type") == type_str end)
  end

  defp filter_since(entries, nil), do: entries

  defp filter_since(entries, since) when is_binary(since) do
    Enum.filter(entries, fn e ->
      ts = Map.get(e, "timestamp", "")
      ts >= since
    end)
  end

  defp filter_until(entries, nil), do: entries

  defp filter_until(entries, until_ts) when is_binary(until_ts) do
    Enum.filter(entries, fn e ->
      ts = Map.get(e, "timestamp", "")
      ts <= until_ts
    end)
  end

  # ---------------------------------------------------------------------------
  # Helpers
  # ---------------------------------------------------------------------------

  defp serialise_entry(entry) do
    %{
      id: entry.id,
      timestamp: entry.timestamp,
      event_type: Atom.to_string(entry.event_type),
      payload: entry.payload,
      source: entry.source,
      version: entry.version
    }
  end

  defp audit_log_path do
    Application.get_env(:stapeln, :verisimdb_audit_log, @audit_log_path)
  end

  defp ensure_parent_dir(path) do
    path |> Path.dirname() |> File.mkdir_p()
  end
end
