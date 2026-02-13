# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Vordr.McpClient do
  @moduledoc """
  MCP (Model Context Protocol) client for Vörðr Rust runtime.

  Communicates with the Vörðr MCP HTTP server via JSON-RPC 2.0.
  This provides the bridge between Elixir orchestration and the
  Rust container runtime.
  """

  require Logger

  @default_endpoint "http://127.0.0.1:8080"
  @timeout 30_000

  @type tool_name :: String.t()
  @type tool_args :: map()
  @type result :: {:ok, map()} | {:error, term()}

  @doc """
  Call an MCP tool on the Vörðr server.

  ## Examples

      {:ok, result} = Vordr.McpClient.call("vordr_ps", %{all: true})
      {:ok, result} = Vordr.McpClient.call("vordr_run", %{image: "alpine:latest"})
  """
  @spec call(tool_name(), tool_args()) :: result()
  def call(tool_name, args \\ %{}) do
    call(tool_name, args, endpoint())
  end

  @spec call(tool_name(), tool_args(), String.t()) :: result()
  def call(tool_name, args, endpoint) do
    request = %{
      jsonrpc: "2.0",
      method: "tools/call",
      params: %{
        name: tool_name,
        arguments: args
      },
      id: generate_request_id()
    }

    case Req.post(endpoint, json: request, receive_timeout: @timeout) do
      {:ok, %{status: 200, body: body}} ->
        handle_response(body)

      {:ok, %{status: status, body: body}} ->
        Logger.error("MCP request failed with status #{status}: #{inspect(body)}")
        {:error, {:http_error, status, body}}

      {:error, reason} ->
        Logger.error("MCP request failed: #{inspect(reason)}")
        {:error, {:connection_error, reason}}
    end
  end

  @doc """
  List available MCP tools.
  """
  @spec list_tools() :: {:ok, [map()]} | {:error, term()}
  def list_tools do
    request = %{
      jsonrpc: "2.0",
      method: "tools/list",
      id: generate_request_id()
    }

    case Req.post(endpoint(), json: request, receive_timeout: @timeout) do
      {:ok, %{status: 200, body: body}} ->
        case body do
          %{"result" => %{"tools" => tools}} -> {:ok, tools}
          _ -> {:error, :invalid_response}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Check if the MCP server is healthy.
  """
  @spec health_check() :: :ok | {:error, term()}
  def health_check do
    case Req.get("#{endpoint()}/health", receive_timeout: 5_000) do
      {:ok, %{status: 200, body: %{"status" => "ok"}}} ->
        :ok

      {:ok, %{status: status}} ->
        {:error, {:unhealthy, status}}

      {:error, reason} ->
        {:error, {:connection_error, reason}}
    end
  end

  # Container operations

  @doc """
  Create and run a container.
  """
  @spec run(String.t(), keyword()) :: {:ok, String.t()} | {:error, term()}
  def run(image, opts \\ []) do
    args = %{
      image: image,
      name: Keyword.get(opts, :name),
      command: Keyword.get(opts, :command),
      env: Keyword.get(opts, :env),
      volumes: Keyword.get(opts, :volumes),
      ports: Keyword.get(opts, :ports),
      detach: Keyword.get(opts, :detach, true)
    }
    |> Enum.reject(fn {_k, v} -> is_nil(v) end)
    |> Map.new()

    case call("vordr_run", args) do
      {:ok, %{"containerId" => id}} -> {:ok, id}
      {:ok, result} -> {:ok, result}
      error -> error
    end
  end

  @doc """
  Start a created container.
  """
  @spec start(String.t()) :: :ok | {:error, term()}
  def start(container_id) do
    case call("vordr_container_start", %{containerId: container_id}) do
      {:ok, _} -> :ok
      error -> error
    end
  end

  @doc """
  Stop a running container.
  """
  @spec stop(String.t(), keyword()) :: :ok | {:error, term()}
  def stop(container_id, opts \\ []) do
    args = %{
      containerId: container_id,
      timeout: Keyword.get(opts, :timeout, 10)
    }

    case call("vordr_stop", args) do
      {:ok, _} -> :ok
      error -> error
    end
  end

  @doc """
  Remove a container.
  """
  @spec remove(String.t(), keyword()) :: :ok | {:error, term()}
  def remove(container_id, opts \\ []) do
    args = %{
      containerId: container_id,
      force: Keyword.get(opts, :force, false)
    }

    case call("vordr_rm", args) do
      {:ok, _} -> :ok
      error -> error
    end
  end

  @doc """
  List containers.
  """
  @spec list(keyword()) :: {:ok, [map()]} | {:error, term()}
  def list(opts \\ []) do
    args = %{
      all: Keyword.get(opts, :all, false)
    }

    case call("vordr_ps", args) do
      {:ok, %{"containers" => containers}} -> {:ok, containers}
      {:ok, result} -> {:ok, result}
      error -> error
    end
  end

  @doc """
  Inspect a container.
  """
  @spec inspect_container(String.t()) :: {:ok, map()} | {:error, term()}
  def inspect_container(container_id) do
    call("vordr_inspect", %{containerId: container_id})
  end

  @doc """
  Execute a command in a running container.
  """
  @spec exec(String.t(), [String.t()], keyword()) :: {:ok, map()} | {:error, term()}
  def exec(container_id, command, _opts \\ []) do
    call("vordr_exec", %{
      containerId: container_id,
      command: command
    })
  end

  @doc """
  List images.
  """
  @spec images(keyword()) :: {:ok, [map()]} | {:error, term()}
  def images(_opts \\ []) do
    case call("vordr_images", %{}) do
      {:ok, %{"images" => images}} -> {:ok, images}
      {:ok, result} -> {:ok, result}
      error -> error
    end
  end

  # Private functions

  defp endpoint do
    Application.get_env(:vordr, :mcp_endpoint, @default_endpoint)
  end

  defp generate_request_id do
    :erlang.unique_integer([:positive, :monotonic])
  end

  defp handle_response(%{"result" => result}) when not is_nil(result) do
    {:ok, result}
  end

  defp handle_response(%{"error" => %{"message" => message, "code" => code}}) do
    {:error, {:mcp_error, code, message}}
  end

  defp handle_response(body) do
    Logger.warning("Unexpected MCP response: #{inspect(body)}")
    {:error, {:unexpected_response, body}}
  end
end
