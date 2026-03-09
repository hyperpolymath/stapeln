# SPDX-License-Identifier: PMPL-1.0-or-later
# stack.ex - Ecto schema for container stacks

defmodule Stapeln.Schemas.Stack do
  @moduledoc """
  Ecto schema for container stack definitions.

  Mirrors the shape used by `Stapeln.StackStore` so that data can move
  between the in-memory GenServer fallback and PostgreSQL transparently.
  """

  use Ecto.Schema
  import Ecto.Changeset

  schema "stacks" do
    field :name, :string
    field :description, :string
    field :services, {:array, :map}, default: []

    timestamps(type: :utc_datetime)
  end

  @required_fields ~w(name)a
  @optional_fields ~w(description services)a

  @doc "Build a changeset for creating or updating a stack."
  @spec changeset(%__MODULE__{} | Ecto.Changeset.t(), map()) :: Ecto.Changeset.t()
  def changeset(stack, attrs) do
    stack
    |> cast(attrs, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
    |> validate_length(:name, min: 1, max: 255)
  end
end
