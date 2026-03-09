# SPDX-License-Identifier: PMPL-1.0-or-later
# user.ex - Ecto schema for authenticated users

defmodule Stapeln.Schemas.User do
  @moduledoc """
  Ecto schema for user accounts.

  Mirrors the shape used by `Stapeln.Auth.UserStore` so that data can
  move between the in-memory GenServer fallback and PostgreSQL.
  """

  use Ecto.Schema
  import Ecto.Changeset

  schema "users" do
    field :email, :string
    field :password_hash, :string

    has_many :user_settings, Stapeln.Schemas.UserSettings

    timestamps(type: :utc_datetime)
  end

  @required_fields ~w(email password_hash)a

  @doc "Build a changeset for creating or updating a user."
  @spec changeset(%__MODULE__{} | Ecto.Changeset.t(), map()) :: Ecto.Changeset.t()
  def changeset(user, attrs) do
    user
    |> cast(attrs, @required_fields)
    |> validate_required(@required_fields)
    |> validate_format(:email, ~r/@/)
    |> unique_constraint(:email)
  end
end
