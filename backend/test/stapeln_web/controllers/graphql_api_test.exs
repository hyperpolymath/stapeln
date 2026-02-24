defmodule StapelnWeb.GraphqlApiTest do
  use StapelnWeb.ConnCase, async: false

  setup do
    Stapeln.StackStore.reset!()
    :ok
  end

  test "create, query, and validate stack via graphql", %{conn: conn} do
    create = """
    mutation Create($input: StackInput!) {
      createStack(input: $input) {
        id
        name
        services {
          name
          kind
        }
      }
    }
    """

    variables = %{
      "input" => %{
        "name" => "graphql-stack",
        "services" => [%{"name" => "api", "kind" => "web"}]
      }
    }

    conn =
      post(conn, ~p"/api/graphql", %{
        query: create,
        variables: variables
      })

    assert %{"data" => %{"createStack" => created}} = json_response(conn, 200)
    id = created["id"]
    assert created["name"] == "graphql-stack"

    query = """
    query Get($id: ID!) {
      stack(id: $id) {
        id
        name
      }
    }
    """

    conn =
      post(recycle(conn), ~p"/api/graphql", %{
        query: query,
        variables: %{"id" => id}
      })

    assert %{"data" => %{"stack" => fetched}} = json_response(conn, 200)
    assert fetched["id"] == id

    validate = """
    mutation Validate($id: ID!) {
      validateStack(id: $id) {
        score
        findings {
          id
          severity
        }
      }
    }
    """

    conn =
      post(recycle(conn), ~p"/api/graphql", %{
        query: validate,
        variables: %{"id" => id}
      })

    assert %{"data" => %{"validateStack" => report}} = json_response(conn, 200)
    assert is_integer(report["score"])
    assert is_list(report["findings"])
  end

  test "rejects graphql requests without API token" do
    conn =
      post(Phoenix.ConnTest.build_conn(), ~p"/api/graphql", %{
        query: "{ stacks { id name } }"
      })

    assert %{"error" => "missing or invalid API token"} = json_response(conn, 401)
  end
end
