# SPDX-License-Identifier: PMPL-1.0-or-later
# repo.ex - Ecto repository for stapeln PostgreSQL persistence

defmodule Stapeln.Repo do
  @moduledoc """
  Ecto repository for PostgreSQL persistence.

  This is an optional persistence layer — the GenServer-based stores
  (StackStore, UserStore, SettingsStore) remain as fallbacks when
  PostgreSQL is unavailable.
  """

  use Ecto.Repo, otp_app: :stapeln, adapter: Ecto.Adapters.Postgres
end
