# SPDX-License-Identifier: PMPL-1.0-or-later
# 20260309000003_create_user_settings.exs - Create user_settings table

defmodule Stapeln.Repo.Migrations.CreateUserSettings do
  use Ecto.Migration

  def change do
    create table(:user_settings) do
      add :user_id, references(:users, on_delete: :delete_all), null: false
      add :settings, :map, default: %{}, null: false

      timestamps(type: :utc_datetime)
    end

    create unique_index(:user_settings, [:user_id])
  end
end
