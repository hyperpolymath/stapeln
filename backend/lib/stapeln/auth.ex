# SPDX-License-Identifier: PMPL-1.0-or-later
# auth.ex - User authentication context for stapeln

defmodule Stapeln.Auth do
  @moduledoc """
  Authentication context.

  Tries Ecto-backed `Stapeln.DbStore` when PostgreSQL is available, and
  falls back to in-memory `UserStore` otherwise.

  Passwords are hashed with :crypto.hash/2 (SHA-256). For production,
  swap to bcrypt/argon2.
  """

  alias Stapeln.Auth.{Token, UserStore}
  alias Stapeln.DbStore

  @doc "Register a new user. Returns {:ok, token} or {:error, reason}."
  @spec register(String.t(), String.t()) :: {:ok, String.t()} | {:error, atom()}
  def register(email, password) when is_binary(email) and is_binary(password) do
    email = String.downcase(String.trim(email))

    cond do
      String.length(email) < 3 -> {:error, :invalid_email}
      String.length(password) < 6 -> {:error, :password_too_short}
      true ->
        hashed = hash_password(password)

        result =
          if DbStore.available?() do
            DbStore.create_user(email, hashed)
          else
            UserStore.create(email, hashed)
          end

        case result do
          {:ok, user_id} -> {:ok, Token.generate(user_id)}
          {:error, _} = err -> err
        end
    end
  end

  @doc "Login with email and password. Returns {:ok, token} or {:error, reason}."
  @spec login(String.t(), String.t()) :: {:ok, String.t()} | {:error, atom()}
  def login(email, password) when is_binary(email) and is_binary(password) do
    email = String.downcase(String.trim(email))

    lookup =
      if DbStore.available?() do
        DbStore.get_user_by_email(email)
      else
        UserStore.get_by_email(email)
      end

    case lookup do
      {:ok, %{id: user_id, password_hash: stored_hash}} ->
        if Plug.Crypto.secure_compare(hash_password(password), stored_hash) do
          {:ok, Token.generate(user_id)}
        else
          {:error, :invalid_credentials}
        end

      {:error, :not_found} ->
        # Constant-time comparison to prevent timing attacks
        _dummy = hash_password(password)
        {:error, :invalid_credentials}
    end
  end

  @doc "Get user info by ID."
  @spec get_user(String.t()) :: {:ok, map()} | {:error, :not_found}
  def get_user(user_id) do
    if DbStore.available?() do
      DbStore.get_user(user_id)
    else
      UserStore.get(user_id)
    end
  end

  defp hash_password(password) do
    :crypto.hash(:sha256, password) |> Base.encode16(case: :lower)
  end
end
