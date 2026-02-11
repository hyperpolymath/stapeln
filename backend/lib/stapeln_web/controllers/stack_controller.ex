defmodule StapelnWeb.StackController do
  use StapelnWeb, :controller

  alias Stapeln.Stacks

  def index(conn, _params) do
    with {:ok, stacks} <- Stacks.list() do
      json(conn, %{data: Enum.map(stacks, &serialize_stack/1)})
    end
  end

  def create(conn, params) do
    with {:ok, stack} <- Stacks.create(params) do
      conn
      |> put_status(:created)
      |> json(%{data: serialize_stack(stack)})
    end
  end

  def show(conn, %{"id" => raw_id}) do
    with {:ok, id} <- parse_id(raw_id),
         {:ok, stack} <- Stacks.fetch(id) do
      json(conn, %{data: serialize_stack(stack)})
    else
      {:error, :invalid_id} -> bad_request(conn, "invalid stack id")
      {:error, :not_found} -> not_found(conn)
    end
  end

  def update(conn, %{"id" => raw_id} = params) do
    attrs = Map.delete(params, "id")

    with {:ok, id} <- parse_id(raw_id),
         {:ok, stack} <- Stacks.update(id, attrs) do
      json(conn, %{data: serialize_stack(stack)})
    else
      {:error, :invalid_id} -> bad_request(conn, "invalid stack id")
      {:error, :not_found} -> not_found(conn)
    end
  end

  def validate(conn, %{"id" => raw_id}) do
    with {:ok, id} <- parse_id(raw_id),
         {:ok, report} <- Stacks.validate(id) do
      json(conn, %{data: serialize_report(report)})
    else
      {:error, :invalid_id} -> bad_request(conn, "invalid stack id")
      {:error, :not_found} -> not_found(conn)
    end
  end

  defp serialize_report(report) do
    %{
      score: report.score,
      findings: Enum.map(report.findings, &serialize_finding/1),
      stack: serialize_stack(report.stack)
    }
  end

  defp serialize_finding(finding) do
    %{
      id: finding.id,
      severity: Atom.to_string(finding.severity),
      message: finding.message,
      hint: finding.hint
    }
  end

  defp serialize_stack(stack) do
    %{
      id: stack.id,
      name: stack.name,
      description: stack.description,
      services: stack.services,
      created_at: DateTime.to_iso8601(stack.created_at),
      updated_at: DateTime.to_iso8601(stack.updated_at)
    }
  end

  defp parse_id(raw_id) do
    case Integer.parse(to_string(raw_id)) do
      {id, ""} when id > 0 -> {:ok, id}
      _ -> {:error, :invalid_id}
    end
  end

  defp bad_request(conn, message) do
    conn
    |> put_status(:bad_request)
    |> json(%{error: message})
  end

  defp not_found(conn) do
    conn
    |> put_status(:not_found)
    |> json(%{error: "stack not found"})
  end
end
