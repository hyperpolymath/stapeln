# SPDX-License-Identifier: PMPL-1.0-or-later
# user_settings.ex - Ecto schema for per-user settings

defmodule Stapeln.Schemas.UserSettings do
  @moduledoc """
  Ecto schema for per-user settings.

  Settings are stored as a JSON map, mirroring the shape used by
  `Stapeln.SettingsStore`.
  """

  use Ecto.Schema
  import Ecto.Changeset

  schema "user_settings" do
    field :settings, :map, default: %{}

    belongs_to :user, Stapeln.Schemas.User

    timestamps(type: :utc_datetime)
  end

  @required_fields ~w(user_id)a
  @optional_fields ~w(settings)a

  @doc "Build a changeset for creating or updating user settings."
  @spec changeset(%__MODULE__{} | Ecto.Changeset.t(), map()) :: Ecto.Changeset.t()
  def changeset(user_settings, attrs) do
    user_settings
    |> cast(attrs, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
    |> foreign_key_constraint(:user_id)
  end
end
