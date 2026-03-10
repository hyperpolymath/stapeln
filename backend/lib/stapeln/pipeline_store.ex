# SPDX-License-Identifier: PMPL-1.0-or-later
# Stapeln.PipelineStore - In-memory persistence for assembly pipelines.
#
# Mirrors the GenServer pattern from StackStore. Persists to a JSONL file
# so pipeline data survives restarts.

defmodule Stapeln.PipelineStore do
  @moduledoc """
  In-memory pipeline persistence with file-backed durability.

  Provides CRUD operations for saved assembly pipelines. Data is held in a
  GenServer and persisted to a JSON file at `/tmp/stapeln-pipeline-store.json`
  (configurable via `:stapeln, :pipeline_store_file`).
  """

  use GenServer

  @type pipeline_record :: %{
          id: pos_integer(),
          name: String.t(),
          description: String.t() | nil,
          pipeline: map(),
          validation: map() | nil,
          created_at: String.t(),
          updated_at: String.t()
        }

  @name __MODULE__
  @default_persist_path "/tmp/stapeln-pipeline-store.json"

  # ---------------------------------------------------------------------------
  # Client API
  # ---------------------------------------------------------------------------

  @spec start_link(term()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: @name)
  end

  @spec create(map()) :: {:ok, pipeline_record()}
  def create(attrs) when is_map(attrs) do
    GenServer.call(@name, {:create, attrs})
  end

  @spec list() :: [pipeline_record()]
  def list do
    GenServer.call(@name, :list)
  end

  @spec get(pos_integer()) :: {:ok, pipeline_record()} | {:error, :not_found}
  def get(id) when is_integer(id) and id > 0 do
    GenServer.call(@name, {:get, id})
  end

  @spec update(pos_integer(), map()) :: {:ok, pipeline_record()} | {:error, :not_found}
  def update(id, attrs) when is_integer(id) and id > 0 and is_map(attrs) do
    GenServer.call(@name, {:update, id, attrs})
  end

  @spec delete(pos_integer()) :: {:ok, pipeline_record()} | {:error, :not_found}
  def delete(id) when is_integer(id) and id > 0 do
    GenServer.call(@name, {:delete, id})
  end

  @spec reset!() :: :ok
  def reset! do
    GenServer.call(@name, :reset)
  end

  # ---------------------------------------------------------------------------
  # GenServer callbacks
  # ---------------------------------------------------------------------------

  @impl true
  def init(_opts) do
    persist_path = Application.get_env(:stapeln, :pipeline_store_file, @default_persist_path)
    {:ok, load_state(persist_path)}
  end

  @impl true
  def handle_call({:create, attrs}, _from, %{persist_path: persist_path} = state) do
    id = state.next_id
    now = DateTime.utc_now() |> DateTime.truncate(:second) |> DateTime.to_iso8601()

    record = %{
      id: id,
      name: fetch(attrs, :name) || "pipeline-#{id}",
      description: fetch(attrs, :description),
      pipeline: fetch(attrs, :pipeline) || %{},
      validation: fetch(attrs, :validation),
      created_at: fetch(attrs, :created_at) || now,
      updated_at: fetch(attrs, :updated_at) || now
    }

    next_state = %{
      state
      | next_id: id + 1,
        pipelines: Map.put(state.pipelines, id, record)
    }

    persist(next_state, persist_path)
    {:reply, {:ok, record}, next_state}
  end

  def handle_call(:list, _from, state) do
    pipelines =
      state.pipelines
      |> Map.values()
      |> Enum.sort_by(& &1.id)

    {:reply, pipelines, state}
  end

  def handle_call({:get, id}, _from, state) do
    case Map.fetch(state.pipelines, id) do
      {:ok, record} -> {:reply, {:ok, record}, state}
      :error -> {:reply, {:error, :not_found}, state}
    end
  end

  def handle_call({:update, id, attrs}, _from, %{persist_path: persist_path} = state) do
    case Map.fetch(state.pipelines, id) do
      {:ok, record} ->
        now = DateTime.utc_now() |> DateTime.truncate(:second) |> DateTime.to_iso8601()

        updated =
          record
          |> maybe_put(:name, fetch(attrs, :name))
          |> maybe_put(:description, fetch(attrs, :description))
          |> maybe_put(:pipeline, fetch(attrs, :pipeline))
          |> maybe_put(:validation, fetch(attrs, :validation))
          |> Map.put(:updated_at, fetch(attrs, :updated_at) || now)

        next_state = %{state | pipelines: Map.put(state.pipelines, id, updated)}
        persist(next_state, persist_path)
        {:reply, {:ok, updated}, next_state}

      :error ->
        {:reply, {:error, :not_found}, state}
    end
  end

  def handle_call({:delete, id}, _from, %{persist_path: persist_path} = state) do
    case Map.fetch(state.pipelines, id) do
      {:ok, record} ->
        next_state = %{state | pipelines: Map.delete(state.pipelines, id)}
        persist(next_state, persist_path)
        {:reply, {:ok, record}, next_state}

      :error ->
        {:reply, {:error, :not_found}, state}
    end
  end

  def handle_call(:reset, _from, %{persist_path: persist_path}) do
    next_state = %{next_id: 1, pipelines: %{}, persist_path: persist_path}
    persist(next_state, persist_path)
    {:reply, :ok, next_state}
  end

  # ---------------------------------------------------------------------------
  # Private helpers
  # ---------------------------------------------------------------------------

  defp fetch(attrs, key) do
    Map.get(attrs, key) || Map.get(attrs, Atom.to_string(key))
  end

  defp maybe_put(record, _field, nil), do: record
  defp maybe_put(record, field, value), do: Map.put(record, field, value)

  defp load_state(nil) do
    %{next_id: 1, pipelines: %{}, persist_path: nil}
  end

  defp load_state(path) do
    case File.read(path) do
      {:ok, body} ->
        case Jason.decode(body) do
          {:ok, decoded} -> decode_state(decoded, path)
          _ -> %{next_id: 1, pipelines: %{}, persist_path: path}
        end

      _ ->
        %{next_id: 1, pipelines: %{}, persist_path: path}
    end
  end

  defp decode_state(%{"next_id" => next_id, "pipelines" => encoded_pipelines}, persist_path)
       when is_integer(next_id) and is_map(encoded_pipelines) do
    pipelines =
      encoded_pipelines
      |> Enum.map(fn {k, record} ->
        id =
          case Integer.parse(k) do
            {value, ""} -> value
            _ -> 0
          end

        {id, decode_record(record, id)}
      end)
      |> Enum.reject(fn {id, _} -> id == 0 end)
      |> Map.new()

    %{next_id: next_id, pipelines: pipelines, persist_path: persist_path}
  end

  defp decode_state(_, persist_path) do
    %{next_id: 1, pipelines: %{}, persist_path: persist_path}
  end

  defp decode_record(record, id) do
    %{
      id: id,
      name: fetch(record, :name) || "pipeline-#{id}",
      description: fetch(record, :description),
      pipeline: fetch(record, :pipeline) || %{},
      validation: fetch(record, :validation),
      created_at: fetch(record, :created_at) || "",
      updated_at: fetch(record, :updated_at) || ""
    }
  end

  defp persist(_state, nil), do: :ok

  defp persist(state, path) do
    payload = %{
      next_id: state.next_id,
      pipelines:
        Map.new(state.pipelines, fn {id, record} ->
          {id, %{
            id: record.id,
            name: record.name,
            description: record.description,
            pipeline: record.pipeline,
            validation: record.validation,
            created_at: record.created_at,
            updated_at: record.updated_at
          }}
        end)
    }

    with :ok <- ensure_parent_dir(path),
         encoded <- Jason.encode!(payload),
         :ok <- File.write(path, encoded) do
      :ok
    else
      _ -> :ok
    end
  end

  defp ensure_parent_dir(path) do
    path
    |> Path.dirname()
    |> File.mkdir_p()
  end
end
