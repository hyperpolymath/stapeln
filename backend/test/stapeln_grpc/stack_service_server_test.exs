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

    created = Server.create_stack(create_request, nil)
    assert created.stack.name == "grpc-stack"
    assert created.stack.id != ""

    fetched = Server.get_stack(%GetStackRequest{id: created.stack.id}, nil)
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
        nil
      )

    list_response = Server.list_stacks(%ListStacksRequest{}, nil)
    assert Enum.count(list_response.stacks) == 1

    id = list_response.stacks |> Enum.at(0) |> Map.get(:id)
    validation = Server.validate_stack(%ValidateStackRequest{id: id}, nil)
    assert is_integer(validation.score)
    assert is_list(validation.findings)
    assert validation.stack.id == id
  end

  test "invalid id raises grpc rpc error" do
    assert_raise GRPC.RPCError, fn ->
      Server.get_stack(%GetStackRequest{id: "invalid"}, nil)
    end
  end

  test "update modifies stack fields" do
    created =
      Server.create_stack(
        %CreateStackRequest{
          name: "before",
          services: [%ServicePayload{name: "api", kind: "web", port: 8080}]
        },
        nil
      )

    update_request = %UpdateStackRequest{
      id: created.stack.id,
      name: "after",
      description: "updated"
    }

    updated = Server.update_stack(update_request, nil)
    assert updated.stack.name == "after"
    assert updated.stack.description == "updated"
  end
end

