# SPDX-License-Identifier: PMPL-1.0-or-later
# user_store.ex - In-memory user storage for stapeln auth

defmodule Stapeln.Auth.UserStore do
  @moduledoc """
  In-memory user store backed by a GenServer.

  Stores users as maps with id, email, and password_hash fields.
  Data persists to /tmp/stapeln-user-store.json for dev convenience.
  """

  use GenServer

  @name __MODULE__
  @persist_path "/tmp/stapeln-user-store.json"

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: @name)
  end

  @spec create(String.t(), String.t()) :: {:ok, String.t()} | {:error, :email_taken}
  def create(email, password_hash) do
    GenServer.call(@name, {:create, email, password_hash})
  end

  @spec get(String.t()) :: {:ok, map()} | {:error, :not_found}
  def get(user_id) do
    GenServer.call(@name, {:get, user_id})
  end

  @spec get_by_email(String.t()) :: {:ok, map()} | {:error, :not_found}
  def get_by_email(email) do
    GenServer.call(@name, {:get_by_email, email})
  end

  @impl true
  def init(_opts) do
    {:ok, load_state()}
  end

  @impl true
  def handle_call({:create, email, password_hash}, _from, state) do
    if Map.has_key?(state.by_email, email) do
      {:reply, {:error, :email_taken}, state}
    else
      user_id = "user_#{state.next_id}"

      user = %{
        id: user_id,
        email: email,
        password_hash: password_hash,
        created_at: DateTime.utc_now() |> DateTime.to_iso8601()
      }

      new_state = %{
        state
        | next_id: state.next_id + 1,
          users: Map.put(state.users, user_id, user),
          by_email: Map.put(state.by_email, email, user_id)
      }

      persist(new_state)
      {:reply, {:ok, user_id}, new_state}
    end
  end

  def handle_call({:get, user_id}, _from, state) do
    case Map.fetch(state.users, user_id) do
      {:ok, user} -> {:reply, {:ok, user}, state}
      :error -> {:reply, {:error, :not_found}, state}
    end
  end

  def handle_call({:get_by_email, email}, _from, state) do
    case Map.get(state.by_email, email) do
      nil -> {:reply, {:error, :not_found}, state}
      user_id -> {:reply, Map.fetch(state.users, user_id) |> ok_or_not_found(), state}
    end
  end

  defp ok_or_not_found({:ok, _} = ok), do: ok
  defp ok_or_not_found(:error), do: {:error, :not_found}

  defp load_state do
    case File.read(@persist_path) do
      {:ok, body} ->
        case Jason.decode(body) do
          {:ok, %{"next_id" => next_id, "users" => users}} ->
            user_map =
              Map.new(users, fn {id, u} ->
                {id, %{
                  id: u["id"],
                  email: u["email"],
                  password_hash: u["password_hash"],
                  created_at: u["created_at"]
                }}
              end)

            email_map =
              Map.new(user_map, fn {_id, u} -> {u.email, u.id} end)

            %{next_id: next_id, users: user_map, by_email: email_map}

          _ ->
            empty_state()
        end

      _ ->
        empty_state()
    end
  end

  defp empty_state, do: %{next_id: 1, users: %{}, by_email: %{}}

  defp persist(state) do
    payload = %{
      next_id: state.next_id,
      users:
        Map.new(state.users, fn {id, u} ->
          {id, %{
            id: u.id,
            email: u.email,
            password_hash: u.password_hash,
            created_at: u.created_at
          }}
        end)
    }

    with encoded <- Jason.encode!(payload) do
      File.write(@persist_path, encoded)
    end
  end
end
