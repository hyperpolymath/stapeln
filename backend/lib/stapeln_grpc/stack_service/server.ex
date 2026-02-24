defmodule StapelnGrpc.StackService.Server do
  @moduledoc false
  use GRPC.Server, service: StapelnGrpc.StackService.Service

  alias Stapeln.Auth.ApiToken
  alias Stapeln.Stacks

  def list_stacks(_request, stream) do
    authorize_stream!(stream)

    with {:ok, stacks} <- Stacks.list() do
      %StapelnGrpc.ListStacksResponse{
        stacks: Enum.map(stacks, &to_stack_payload/1)
      }
    end
  end

  def create_stack(request, stream) do
    authorize_stream!(stream)

    attrs = %{
      "name" => normalize_blank(request.name),
      "description" => normalize_blank(request.description),
      "services" => Enum.map(request.services, &from_service_payload/1)
    }

    with {:ok, stack} <- Stacks.create(attrs) do
      %StapelnGrpc.StackResponse{stack: to_stack_payload(stack)}
    end
  end

  def get_stack(request, stream) do
    authorize_stream!(stream)

    with {:ok, id} <- parse_id(request.id),
         {:ok, stack} <- Stacks.fetch(id) do
      %StapelnGrpc.StackResponse{stack: to_stack_payload(stack)}
    else
      {:error, :invalid_id} -> raise_rpc_error(:invalid_argument, "invalid stack id")
      {:error, :not_found} -> raise_rpc_error(:not_found, "stack not found")
    end
  end

  def update_stack(request, stream) do
    authorize_stream!(stream)

    with {:ok, id} <- parse_id(request.id),
         {:ok, stack} <- Stacks.update(id, update_attrs(request)) do
      %StapelnGrpc.StackResponse{stack: to_stack_payload(stack)}
    else
      {:error, :invalid_id} -> raise_rpc_error(:invalid_argument, "invalid stack id")
      {:error, :not_found} -> raise_rpc_error(:not_found, "stack not found")
    end
  end

  def validate_stack(request, stream) do
    authorize_stream!(stream)

    with {:ok, id} <- parse_id(request.id),
         {:ok, report} <- Stacks.validate(id) do
      %StapelnGrpc.ValidateStackResponse{
        score: Map.get(report, :score, 0),
        findings: Enum.map(Map.get(report, :findings, []), &to_finding_payload/1),
        stack: report |> Map.get(:stack, %{}) |> to_stack_payload()
      }
    else
      {:error, :invalid_id} -> raise_rpc_error(:invalid_argument, "invalid stack id")
      {:error, :not_found} -> raise_rpc_error(:not_found, "stack not found")
    end
  end

  defp parse_id(raw_id) do
    case Integer.parse(to_string(raw_id)) do
      {id, ""} when id > 0 -> {:ok, id}
      _ -> {:error, :invalid_id}
    end
  end

  defp update_attrs(request) do
    %{}
    |> maybe_put("name", normalize_blank(request.name))
    |> maybe_put("description", normalize_blank(request.description))
    |> maybe_put("services", normalize_services(request.services))
  end

  defp maybe_put(attrs, _key, nil), do: attrs
  defp maybe_put(attrs, key, value), do: Map.put(attrs, key, value)

  defp normalize_services([]), do: nil
  defp normalize_services(services), do: Enum.map(services, &from_service_payload/1)

  defp from_service_payload(payload) do
    %{
      "name" => normalize_blank(payload.name) || "unnamed-service",
      "kind" => normalize_blank(payload.kind),
      "port" => if(payload.port > 0, do: payload.port, else: nil)
    }
    |> Enum.reject(fn {_key, value} -> is_nil(value) end)
    |> Map.new()
  end

  defp to_stack_payload(stack) when is_map(stack) do
    %StapelnGrpc.StackPayload{
      id: stack |> get_value(:id) |> to_string(),
      name: to_string(get_value(stack, :name) || "stack"),
      description: to_string(get_value(stack, :description) || ""),
      created_at: to_string(get_value(stack, :created_at) || ""),
      updated_at: to_string(get_value(stack, :updated_at) || ""),
      services: stack |> get_value(:services, []) |> Enum.map(&to_service_payload/1)
    }
  end

  defp to_stack_payload(_), do: %StapelnGrpc.StackPayload{}

  defp to_service_payload(service) do
    %StapelnGrpc.ServicePayload{
      name: to_string(get_value(service, :name) || "unnamed-service"),
      kind: to_string(get_value(service, :kind) || ""),
      port: int_value(get_value(service, :port))
    }
  end

  defp to_finding_payload(finding) do
    %StapelnGrpc.FindingPayload{
      id: to_string(get_value(finding, :id) || ""),
      severity: to_string(get_value(finding, :severity) || ""),
      message: to_string(get_value(finding, :message) || ""),
      hint: to_string(get_value(finding, :hint) || "")
    }
  end

  defp int_value(nil), do: 0
  defp int_value(value) when is_integer(value), do: value

  defp int_value(value) when is_binary(value) do
    case Integer.parse(value) do
      {parsed, ""} -> parsed
      _ -> 0
    end
  end

  defp int_value(_), do: 0

  defp get_value(map, key, fallback \\ nil) do
    Map.get(map, key) || Map.get(map, Atom.to_string(key)) || fallback
  end

  defp normalize_blank(nil), do: nil

  defp normalize_blank(value) when is_binary(value) do
    case String.trim(value) do
      "" -> nil
      trimmed -> trimmed
    end
  end

  defp normalize_blank(value), do: value |> to_string() |> normalize_blank()

  defp authorize_stream!(stream) do
    case ApiToken.authorize_grpc_stream(stream) do
      :ok ->
        :ok

      {:error, :token_not_configured} ->
        raise_rpc_error(:internal, "api authentication is not configured")

      {:error, _reason} ->
        raise_rpc_error(:unauthenticated, "missing or invalid API token")
    end
  end

  defp raise_rpc_error(status, message) do
    raise GRPC.RPCError, status: status, message: message
  end
end
