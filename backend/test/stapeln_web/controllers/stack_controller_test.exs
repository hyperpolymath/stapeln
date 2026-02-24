defmodule StapelnWeb.StackControllerTest do
  use StapelnWeb.ConnCase, async: false

  setup do
    Stapeln.StackStore.reset!()
    :ok
  end

  test "creates, fetches, updates, and validates a stack", %{conn: conn} do
    create_params = %{
      "name" => "demo-stack",
      "description" => "testing",
      "services" => [%{"name" => "api", "kind" => "web"}]
    }

    conn = post(conn, ~p"/api/stacks", create_params)
    assert %{"data" => created} = json_response(conn, 201)
    assert created["name"] == "demo-stack"
    assert is_integer(created["id"])
    id = created["id"]

    conn = get(recycle(conn), ~p"/api/stacks/#{id}")
    assert %{"data" => fetched} = json_response(conn, 200)
    assert fetched["id"] == id

    conn = put(recycle(conn), ~p"/api/stacks/#{id}", %{"name" => "demo-stack-v2"})
    assert %{"data" => updated} = json_response(conn, 200)
    assert updated["name"] == "demo-stack-v2"

    conn = post(recycle(conn), ~p"/api/stacks/#{id}/validate", %{})
    assert %{"data" => report} = json_response(conn, 200)
    assert is_integer(report["score"])
    assert is_list(report["findings"])
    assert report["stack"]["id"] == id
  end

  test "returns not_found for unknown stack", %{conn: conn} do
    conn = get(conn, ~p"/api/stacks/999")
    assert %{"error" => "stack not found"} = json_response(conn, 404)
  end

  test "lists stacks", %{conn: conn} do
    _ = post(conn, ~p"/api/stacks", %{"name" => "a"})
    _ = post(recycle(conn), ~p"/api/stacks", %{"name" => "b"})

    conn = get(recycle(conn), ~p"/api/stacks")
    assert %{"data" => stacks} = json_response(conn, 200)
    assert Enum.count(stacks) == 2
  end

  test "rejects stack requests without API token" do
    conn = get(Phoenix.ConnTest.build_conn(), ~p"/api/stacks")
    assert %{"error" => "missing or invalid API token"} = json_response(conn, 401)
  end
end
