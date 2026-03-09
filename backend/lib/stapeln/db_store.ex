# SPDX-License-Identifier: PMPL-1.0-or-later
# db_store.ex - Ecto-backed persistence layer for stapeln

defmodule Stapeln.DbStore do
  @moduledoc """
  Ecto-backed persistence layer.

  Provides the same API surface as the GenServer stores (StackStore,
  UserStore, SettingsStore) but persists to PostgreSQL via `Stapeln.Repo`.

  Falls back gracefully when the Repo process is not running (e.g. when
  PostgreSQL is unavailable or the application is started without the
  database supervisor tree).
  """

  alias Stapeln.Repo
  alias Stapeln.Schemas.{Stack, User, UserSettings}

  import Ecto.Query, only: [from: 2]

  require Logger

  # ---------------------------------------------------------------------------
  # Availability check
  # ---------------------------------------------------------------------------

  @doc """
  Returns true when `Stapeln.Repo` is compiled and its process is alive.

  This is the guard used by NativeBridge, Auth, and SettingsStore to decide
  whether to use Ecto or fall back to GenServer stores.
  """
  @spec available?() :: boolean()
  def available? do
    Code.ensure_loaded?(Repo) and GenServer.whereis(Repo) != nil
  end

  # ---------------------------------------------------------------------------
  # Stack operations
  # ---------------------------------------------------------------------------

  @doc "List all stacks ordered by ID."
  @spec list_stacks() :: {:ok, [map()]}
  def list_stacks do
    stacks =
      from(s in Stack, order_by: [asc: s.id])
      |> Repo.all()
      |> Enum.map(&stack_to_map/1)

    {:ok, stacks}
  rescue
    error ->
      Logger.warning("DbStore.list_stacks failed: #{inspect(error)}")
      {:error, :db_error}
  end

  @doc "Create a new stack from the given attributes map."
  @spec create_stack(map()) :: {:ok, map()} | {:error, term()}
  def create_stack(attrs) when is_map(attrs) do
    %Stack{}
    |> Stack.changeset(normalize_attrs(attrs))
    |> Repo.insert()
    |> case do
      {:ok, stack} -> {:ok, stack_to_map(stack)}
      {:error, changeset} -> {:error, changeset_errors(changeset)}
    end
  rescue
    error ->
      Logger.warning("DbStore.create_stack failed: #{inspect(error)}")
      {:error, :db_error}
  end

  @doc "Fetch a single stack by integer ID."
  @spec get_stack(pos_integer()) :: {:ok, map()} | {:error, :not_found}
  def get_stack(id) when is_integer(id) and id > 0 do
    case Repo.get(Stack, id) do
      nil -> {:error, :not_found}
      stack -> {:ok, stack_to_map(stack)}
    end
  rescue
    error ->
      Logger.warning("DbStore.get_stack failed: #{inspect(error)}")
      {:error, :db_error}
  end

  @doc "Update an existing stack."
  @spec update_stack(pos_integer(), map()) :: {:ok, map()} | {:error, :not_found | term()}
  def update_stack(id, attrs) when is_integer(id) and id > 0 and is_map(attrs) do
    case Repo.get(Stack, id) do
      nil ->
        {:error, :not_found}

      stack ->
        stack
        |> Stack.changeset(normalize_attrs(attrs))
        |> Repo.update()
        |> case do
          {:ok, updated} -> {:ok, stack_to_map(updated)}
          {:error, changeset} -> {:error, changeset_errors(changeset)}
        end
    end
  rescue
    error ->
      Logger.warning("DbStore.update_stack failed: #{inspect(error)}")
      {:error, :db_error}
  end

  # ---------------------------------------------------------------------------
  # User operations
  # ---------------------------------------------------------------------------

  @doc "Create a user with the given email and password hash."
  @spec create_user(String.t(), String.t()) :: {:ok, String.t()} | {:error, term()}
  def create_user(email, password_hash)
      when is_binary(email) and is_binary(password_hash) do
    %User{}
    |> User.changeset(%{email: email, password_hash: password_hash})
    |> Repo.insert()
    |> case do
      {:ok, user} -> {:ok, "user_#{user.id}"}
      {:error, changeset} -> {:error, changeset_errors(changeset)}
    end
  rescue
    error ->
      Logger.warning("DbStore.create_user failed: #{inspect(error)}")
      {:error, :db_error}
  end

  @doc "Get a user by their string ID (e.g. `\"user_42\"`)."
  @spec get_user(String.t()) :: {:ok, map()} | {:error, :not_found}
  def get_user(user_id) when is_binary(user_id) do
    case parse_user_id(user_id) do
      {:ok, int_id} ->
        case Repo.get(User, int_id) do
          nil -> {:error, :not_found}
          user -> {:ok, user_to_map(user)}
        end

      :error ->
        {:error, :not_found}
    end
  rescue
    error ->
      Logger.warning("DbStore.get_user failed: #{inspect(error)}")
      {:error, :db_error}
  end

  @doc "Get a user by email address."
  @spec get_user_by_email(String.t()) :: {:ok, map()} | {:error, :not_found}
  def get_user_by_email(email) when is_binary(email) do
    case Repo.get_by(User, email: email) do
      nil -> {:error, :not_found}
      user -> {:ok, user_to_map(user)}
    end
  rescue
    error ->
      Logger.warning("DbStore.get_user_by_email failed: #{inspect(error)}")
      {:error, :db_error}
  end

  # ---------------------------------------------------------------------------
  # Settings operations
  # ---------------------------------------------------------------------------

  @doc """
  Get settings for a user. Returns the settings map (not wrapped in {:ok, ...}).

  When no user_id is given (global settings), returns the first UserSettings
  row or the default settings map.
  """
  @spec get_settings(pos_integer() | nil) :: map()
  def get_settings(nil) do
    case Repo.one(from(s in UserSettings, limit: 1)) do
      nil -> default_settings()
      record -> Map.merge(default_settings(), record.settings || %{})
    end
  rescue
    _error -> default_settings()
  end

  def get_settings(user_id) when is_integer(user_id) do
    case Repo.get_by(UserSettings, user_id: user_id) do
      nil -> default_settings()
      record -> Map.merge(default_settings(), record.settings || %{})
    end
  rescue
    _error -> default_settings()
  end

  @doc """
  Update settings for a user. Upserts the UserSettings row.

  When user_id is nil, operates on the first row (global settings).
  """
  @spec update_settings(pos_integer() | nil, map()) :: {:ok, map()}
  def update_settings(nil, attrs) when is_map(attrs) do
    case Repo.one(from(s in UserSettings, limit: 1)) do
      nil ->
        # No existing row — cannot insert without a user_id for FK constraint.
        # Return merged defaults as a best-effort response.
        {:ok, Map.merge(default_settings(), attrs)}

      record ->
        merged = Map.merge(record.settings || %{}, attrs)

        record
        |> UserSettings.changeset(%{settings: merged})
        |> Repo.update()
        |> case do
          {:ok, updated} -> {:ok, Map.merge(default_settings(), updated.settings)}
          {:error, _} -> {:ok, Map.merge(default_settings(), attrs)}
        end
    end
  rescue
    _error -> {:ok, Map.merge(default_settings(), attrs)}
  end

  def update_settings(user_id, attrs) when is_integer(user_id) and is_map(attrs) do
    case Repo.get_by(UserSettings, user_id: user_id) do
      nil ->
        %UserSettings{}
        |> UserSettings.changeset(%{user_id: user_id, settings: attrs})
        |> Repo.insert()
        |> case do
          {:ok, record} -> {:ok, Map.merge(default_settings(), record.settings)}
          {:error, _} -> {:ok, Map.merge(default_settings(), attrs)}
        end

      record ->
        merged = Map.merge(record.settings || %{}, attrs)

        record
        |> UserSettings.changeset(%{settings: merged})
        |> Repo.update()
        |> case do
          {:ok, updated} -> {:ok, Map.merge(default_settings(), updated.settings)}
          {:error, _} -> {:ok, Map.merge(default_settings(), attrs)}
        end
    end
  rescue
    _error -> {:ok, Map.merge(default_settings(), attrs)}
  end

  # ---------------------------------------------------------------------------
  # Private helpers
  # ---------------------------------------------------------------------------

  defp stack_to_map(%Stack{} = s) do
    %{
      id: s.id,
      name: s.name,
      description: s.description,
      services: s.services || [],
      created_at: s.inserted_at,
      updated_at: s.updated_at
    }
  end

  defp user_to_map(%User{} = u) do
    %{
      id: "user_#{u.id}",
      email: u.email,
      password_hash: u.password_hash,
      created_at: u.inserted_at && DateTime.to_iso8601(u.inserted_at)
    }
  end

  defp normalize_attrs(attrs) do
    attrs
    |> Map.new(fn
      {k, v} when is_atom(k) -> {k, v}
      {k, v} when is_binary(k) -> {String.to_existing_atom(k), v}
    end)
  rescue
    # If string keys don't match existing atoms, keep as-is for changeset cast
    _error ->
      Map.new(attrs, fn
        {k, v} when is_atom(k) -> {k, v}
        {k, v} when is_binary(k) -> {safe_to_atom(k), v}
      end)
  end

  defp safe_to_atom(key) when is_binary(key) do
    String.to_existing_atom(key)
  rescue
    ArgumentError -> String.to_atom(key)
  end

  defp parse_user_id("user_" <> rest) do
    case Integer.parse(rest) do
      {int_id, ""} when int_id > 0 -> {:ok, int_id}
      _ -> :error
    end
  end

  defp parse_user_id(_), do: :error

  defp changeset_errors(%Ecto.Changeset{} = changeset) do
    errors =
      Ecto.Changeset.traverse_errors(changeset, fn {msg, opts} ->
        Regex.replace(~r"%{(\w+)}", msg, fn _, key ->
          opts |> Keyword.get(String.to_existing_atom(key), key) |> to_string()
        end)
      end)

    case Map.to_list(errors) do
      [{_field, [message | _]} | _] when is_binary(message) ->
        String.to_atom(message)

      _ ->
        :validation_error
    end
  end

  defp default_settings do
    %{
      "theme" => "dark",
      "defaultRuntime" => "podman",
      "autoSave" => false,
      "backendUrl" => "/api"
    }
  end
end
