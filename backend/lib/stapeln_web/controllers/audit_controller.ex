# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)

defmodule StapelnWeb.AuditController do
  @moduledoc """
  REST endpoint for querying the VeriSimDB audit trail.

  GET /api/audit — returns recent audit entries with optional filters.

  Query parameters:
  - `event_type` — filter by event type (e.g. `stack_created`, `security_scan`)
  - `since`      — ISO 8601 lower bound
  - `until`      — ISO 8601 upper bound
  - `limit`      — max entries (default 100)
  """

  use StapelnWeb, :controller

  alias Stapeln.VeriSimDB

  def index(conn, params) do
    opts =
      []
      |> maybe_put(:event_type, params["event_type"])
      |> maybe_put(:since, params["since"])
      |> maybe_put(:until, params["until"])
      |> maybe_put_int(:limit, params["limit"])

    {:ok, entries} = VeriSimDB.query(opts)

    json(conn, %{data: entries})
  end

  defp maybe_put(opts, _key, nil), do: opts
  defp maybe_put(opts, _key, ""), do: opts

  defp maybe_put(opts, :event_type, value) when is_binary(value) do
    Keyword.put(opts, :event_type, String.to_existing_atom(value))
  rescue
    ArgumentError -> opts
  end

  defp maybe_put(opts, key, value), do: Keyword.put(opts, key, value)

  defp maybe_put_int(opts, _key, nil), do: opts
  defp maybe_put_int(opts, _key, ""), do: opts

  defp maybe_put_int(opts, key, value) when is_binary(value) do
    case Integer.parse(value) do
      {n, ""} when n > 0 -> Keyword.put(opts, key, n)
      _ -> opts
    end
  end

  defp maybe_put_int(opts, key, value) when is_integer(value) and value > 0 do
    Keyword.put(opts, key, value)
  end

  defp maybe_put_int(opts, _key, _value), do: opts
end
