# SPDX-License-Identifier: PMPL-1.0-or-later
# 20260309000001_create_stacks.exs - Create stacks table

defmodule Stapeln.Repo.Migrations.CreateStacks do
  use Ecto.Migration

  def change do
    create table(:stacks) do
      add :name, :string, null: false
      add :description, :text
      add :services, {:array, :map}, default: [], null: false

      timestamps(type: :utc_datetime)
    end

    create index(:stacks, [:name])
  end
end
