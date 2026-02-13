
# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Vordr.McpClientIntegrationTest do
  use ExUnit.Case, async: true

  alias Vordr.McpClient

  # Mocking Req library for HTTP requests
  describe "Vordr.McpClient" do
    setup do
      # Mock the Req.post/Req.get functions to return predefined responses
      # This setup will be specific to each test depending on the expected outcome
      :ok
    end

    @tag :mcp_client
    test "call/2 sends a correct JSON-RPC request and handles success response" do
      # Mock Req.post to return a successful response for a known tool call
      mock_post = fn "http://127.0.0.1:8080", [json: request, receive_timeout: 30000] ->
        assert request[:jsonrpc] == "2.0"
        assert request.method == "tools/call"
        assert request.params.name == "vordr_ps"
        assert request.params.arguments == %{all: true}
        assert is_binary(request.id)

        # Simulate a successful response
        {:ok, %{status: 200, body: %{"jsonrpc" => "2.0", "result" => %{"containers" => []}, "id" => request.id}}}
      end

      # Patch Req.post temporarily for this test
      :meck.expect(Req, :post, mock_post)

      # Call the client function
      assert Vordr.McpClient.call("vordr_ps", %{all: true}) == {:ok, %{"containers" => []}}

      # Clean up the mock
      :meck.unload()
    end

    @tag :mcp_client
    test "call/2 handles MCP errors correctly" do
      mock_post = fn "http://127.0.0.1:8080", [json: request, receive_timeout: 30000] ->
        # Simulate an MCP error response
        {:ok, %{status: 200, body: %{"jsonrpc" => "2.0", "error" => %{"code" => -32000, "message" => "Internal server error"}, "id" => request.id}}}
      end

      :meck.expect(Req, :post, mock_post)

      assert Vordr.McpClient.call("vordr_run", %{image: "test"}) == {:error, {:mcp_error, -32000, "Internal server error"}}
      :meck.unload()
    end

    @tag :mcp_client
    test "call/2 handles HTTP errors correctly" do
      mock_post = fn "http://127.0.0.1:8080", [json: _request, receive_timeout: 30000] ->
        # Simulate an HTTP error response
        {:ok, %{status: 500, body: "Internal Server Error"}}
      end

      :meck.expect(Req, :post, mock_post)

      assert Vordr.McpClient.call("vordr_ps") == {:error, {:http_error, 500, "Internal Server Error"}}
      :meck.unload()
    end

    @tag :mcp_client
    test "call/2 handles connection errors correctly" do
      mock_post = fn "http://127.0.0.1:8080", [_opts] ->
        # Simulate a connection error
        {:error, :nxdomain}
      end

      :meck.expect(Req, :post, mock_post)

      assert Vordr.McpClient.call("vordr_ps") == {:error, {:connection_error, :nxdomain}}
      :meck.unload()
    end

    @tag :mcp_client
    test "list_tools/0 fetches and returns tools" do
      mock_post = fn "http://127.0.0.1:8080", [json: request, receive_timeout: 30000] ->
        assert request.method == "tools/list"
        {:ok, %{status: 200, body: %{"jsonrpc" => "2.0", "result" => %{"tools" => [%{"name" => "vordr_run"}]}, "id" => request.id}}}
      end

      :meck.expect(Req, :post, mock_post)

      assert Vordr.McpClient.list_tools() == {:ok, [%{"name" => "vordr_run"}]}
      :meck.unload()
    end

    @tag :mcp_client
    test "health_check/0 returns :ok on success" do
      mock_get = fn "http://127.0.0.1:8080/health", [receive_timeout: 5000] ->
        {:ok, %{status: 200, body: %{"status" => "ok"}}}
      end

      :meck.expect(Req, :get, mock_get)

      assert Vordr.McpClient.health_check() == :ok
      :meck.unload()
    end

    @tag :mcp_client
    test "health_check/0 returns error on unhealthy status" do
      mock_get = fn "http://127.0.0.1:8080/health", [receive_timeout: 5000] ->
        {:ok, %{status: 503, body: %{"status" => "unhealthy"}}}
      end

      :meck.expect(Req, :get, mock_get)

      assert Vordr.McpClient.health_check() == {:error, {:unhealthy, 503}}
      :meck.unload()
    end

    @tag :mcp_client
    test "health_check/0 handles connection errors" do
      mock_get = fn "http://127.0.0.1:8080/health", [_opts] ->
        {:error, :timeout}
      end

      :meck.expect(Req, :get, mock_get)

      assert Vordr.McpClient.health_check() == {:error, {:connection_error, :timeout}}
      :meck.unload()
    end

    # Specific tool function tests
    # These tests combine tool-specific arguments with general call logic
    # and rely on the success of the underlying `call/3` function.

    @tag :mcp_client
    test "run/2 with basic arguments" do
      mock_call = fn "http://127.0.0.1:8080", [json: request, receive_timeout: 30000] ->
        assert request.params.name == "vordr_run"
        assert request.params.arguments.image == "alpine:latest"
        assert request.params.arguments.detach == true
        {:ok, %{status: 200, body: %{"result" => %{"containerId" => "abcdef123"}, "id" => request.id}}}
      end

      :meck.expect(Req, :post, mock_call)

      assert Vordr.McpClient.run("alpine:latest") == {:ok, "abcdef123"}
      :meck.unload()
    end

    @tag :mcp_client
    test "run/2 with advanced arguments" do
      mock_call = fn "http://127.0.0.1:8080", [json: request, receive_timeout: 30000] ->
        assert request.params.name == "vordr_run"
        assert request.params.arguments.image == "nginx:1.26"
        assert request.params.arguments.name == "webserver"
        assert request.params.arguments.ports == ["80:80"]
        assert request.params.arguments.detach == true
        {:ok, %{status: 200, body: %{"result" => %{"containerId" => "nginx-cont"}, "id" => request.id}}}
      end

      :meck.expect(Req, :post, mock_call)

      assert Vordr.McpClient.run("nginx:1.26", name: "webserver", ports: ["80:80"]) == {:ok, "nginx-cont"}
      :meck.unload()
    end

    @tag :mcp_client
    test "start/1 calls vordr_container_start" do
      mock_call = fn "http://127.0.0.1:8080", [json: request, receive_timeout: 30000] ->
        assert request.params.name == "vordr_container_start"
        assert request.params.arguments.containerId == "cont123"
        {:ok, %{status: 200, body: %{"result" => %{}, "id" => request.id}}}
      end

      :meck.expect(Req, :post, mock_call)
      assert Vordr.McpClient.start("cont123") == :ok
      :meck.unload()
    end

    @tag :mcp_client
    test "stop/2 calls vordr_stop with timeout" do
      mock_call = fn "http://127.0.0.1:8080", [json: request, receive_timeout: 30000] ->
        assert request.params.name == "vordr_stop"
        assert request.params.arguments.containerId == "cont123"
        assert request.params.arguments.timeout == 20
        {:ok, %{status: 200, body: %{"result" => %{}, "id" => request.id}}}
      end

      :meck.expect(Req, :post, mock_call)
      assert Vordr.McpClient.stop("cont123", timeout: 20) == :ok
      :meck.unload()
    end

    @tag :mcp_client
    test "remove/2 calls vordr_rm with force option" do
      mock_call = fn "http://127.0.0.1:8080", [json: request, receive_timeout: 30000] ->
        assert request.params.name == "vordr_rm"
        assert request.params.arguments.containerId == "cont123"
        assert request.params.arguments.force == true
        {:ok, %{status: 200, body: %{"result" => %{}, "id" => request.id}}}
      end

      :meck.expect(Req, :post, mock_call)
      assert Vordr.McpClient.remove("cont123", force: true) == :ok
      :meck.unload()
    end

    @tag :mcp_client
    test "list/1 calls vordr_ps with all option" do
      mock_call = fn "http://127.0.0.1:8080", [json: request, receive_timeout: 30000] ->
        assert request.params.name == "vordr_ps"
        assert request.params.arguments.all == true
        {:ok, %{status: 200, body: %{"result" => %{"containers" => [%{"id" => "cont1"}]}, "id" => request.id}}}
      end

      :meck.expect(Req, :post, mock_call)
      assert Vordr.McpClient.list(all: true) == {:ok, [%{"id" => "cont1"}]}
      :meck.unload()
    end

    @tag :mcp_client
    test "inspect_container/1 calls vordr_inspect" do
      mock_call = fn "http://127.0.0.1:8080", [json: request, receive_timeout: 30000] ->
        assert request.params.name == "vordr_inspect"
        assert request.params.arguments.containerId == "cont123"
        {:ok, %{status: 200, body: %{"result" => %{"containerId" => "cont123", "state" => "running"}, "id" => request.id}}}
      end

      :meck.expect(Req, :post, mock_call)
      assert Vordr.McpClient.inspect_container("cont123") == {:ok, %{"containerId" => "cont123", "state" => "running"}}
      :meck.unload()
    end

    @tag :mcp_client
    test "exec/3 calls vordr_exec" do
      mock_call = fn "http://127.0.0.1:8080", [json: request, receive_timeout: 30000] ->
        assert request.params.name == "vordr_exec"
        assert request.params.arguments.containerId == "cont123"
        assert request.params.arguments.command == ["ls", "-l"]
        {:ok, %{status: 200, body: %{"result" => %{"stdout" => "output"}, "id" => request.id}}}
      end

      :meck.expect(Req, :post, mock_call)
      assert Vordr.McpClient.exec("cont123", ["ls", "-l"]) == {:ok, %{"stdout" => "output"}}
      :meck.unload()
    end

    @tag :mcp_client
    test "images/1 calls vordr_images" do
      mock_call = fn "http://127.0.0.1:8080", [json: request, receive_timeout: 30000] ->
        assert request.params.name == "vordr_images"
        {:ok, %{status: 200, body: %{"result" => %{"images" => []}, "id" => request.id}}}
      end

      :meck.expect(Req, :post, mock_call)
      assert Vordr.McpClient.images() == {:ok, %{"images" => []}}
      :meck.unload()
    end
  end
end
