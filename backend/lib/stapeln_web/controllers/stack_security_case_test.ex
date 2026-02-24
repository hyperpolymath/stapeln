defmodule StapelnWeb.SecurityControllerTest do
  use StapelnWeb.ConnCase, async: true

  alias Stapeln.Security.PanicAttacker

  describe "POST /api/security/panic-attacker" do
    test "starts monitoring and returns status", %{conn: conn} do
      {:ok, _pid} = start_supervised(PanicAttacker)

      conn =
        post(conn, "/api/security/panic-attacker", %{
          "command" => "ambush",
          "target" => "/bin/true"
        })

      assert json_response(conn, 200)["status"] == "running"
    end
  end

  describe "POST /api/security/panic-attacker/stop" do
    test "stops monitoring", %{conn: conn} do
      {:ok, _pid} = start_supervised(PanicAttacker)

      post(conn, "/api/security/panic-attacker", %{"command" => "ambush", "target" => "/bin/true"})

      conn = post(conn, "/api/security/panic-attacker/stop", %{})
      assert json_response(conn, 200)["status"] in ["stopped", "idle"]
    end
  end

  describe "GET /api/security/panic-attacker/status" do
    test "returns current state", %{conn: conn} do
      {:ok, _pid} = start_supervised(PanicAttacker)
      conn = get(conn, "/api/security/panic-attacker/status")
      assert json_response(conn, 200)["status"] == "idle"
    end
  end
end
