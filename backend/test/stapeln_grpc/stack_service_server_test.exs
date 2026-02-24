defmodule StapelnGrpc.StackService.ServerTest do
  use ExUnit.Case, async: false

  alias StapelnGrpc.CreateStackRequest
  alias StapelnGrpc.GetStackRequest
  alias StapelnGrpc.ListStacksRequest
  alias StapelnGrpc.ServicePayload
  alias StapelnGrpc.StackService.Server
  alias StapelnGrpc.UpdateStackRequest
  alias StapelnGrpc.ValidateStackRequest

  setup do
    Stapeln.StackStore.reset!()
    :ok
  end

  test "create and retrieve stack via grpc server callbacks" do
    create_request = %CreateStackRequest{
      name: "grpc-stack",
      description: "via grpc",
      services: [%ServicePayload{name: "api", kind: "web", port: 8080}]
    }

    created = Server.create_stack(create_request, authorized_stream())
    assert created.stack.name == "grpc-stack"
    assert created.stack.id != ""

    fetched = Server.get_stack(%GetStackRequest{id: created.stack.id}, authorized_stream())
    assert fetched.stack.id == created.stack.id
    assert fetched.stack.services |> Enum.at(0) |> Map.get(:name) == "api"
  end

  test "list and validate stack via grpc server callbacks" do
    _ =
      Server.create_stack(
        %CreateStackRequest{
          name: "validate-me",
          services: [%ServicePayload{name: "db", kind: "db", port: 5432}]
        },
        authorized_stream()
      )

    list_response = Server.list_stacks(%ListStacksRequest{}, authorized_stream())
    assert Enum.count(list_response.stacks) == 1

    id = list_response.stacks |> Enum.at(0) |> Map.get(:id)
    validation = Server.validate_stack(%ValidateStackRequest{id: id}, authorized_stream())
    assert is_integer(validation.score)
    assert is_list(validation.findings)
    assert validation.stack.id == id
  end

  test "invalid id raises grpc rpc error" do
    assert_raise GRPC.RPCError, fn ->
      Server.get_stack(%GetStackRequest{id: "invalid"}, authorized_stream())
    end
  end

  test "update modifies stack fields" do
    created =
      Server.create_stack(
        %CreateStackRequest{
          name: "before",
          services: [%ServicePayload{name: "api", kind: "web", port: 8080}]
        },
        authorized_stream()
      )

    update_request = %UpdateStackRequest{
      id: created.stack.id,
      name: "after",
      description: "updated"
    }

    updated = Server.update_stack(update_request, authorized_stream())
    assert updated.stack.name == "after"
    assert updated.stack.description == "updated"
  end

  test "missing auth metadata raises unauthenticated grpc error" do
    error =
      assert_raise GRPC.RPCError, fn ->
        Server.list_stacks(%ListStacksRequest{}, nil)
      end

    assert error.status == :unauthenticated
  end

  defp authorized_stream do
    %{http_request_headers: %{"authorization" => "Bearer #{api_token()}"}}
  end

  defp api_token do
    :stapeln
    |> Application.fetch_env!(:api_auth)
    |> Keyword.fetch!(:token)
  end
end
