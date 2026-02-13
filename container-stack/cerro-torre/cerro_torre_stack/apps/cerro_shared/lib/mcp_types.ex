# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule CerroShared.MCPTypes do
  @moduledoc """
  Model Context Protocol (MCP) types for JSON-RPC 2.0 communication.

  Used when Svalinn and Vörðr run as separate services.
  """

  @type json_rpc_request :: %{
    jsonrpc: String.t(),
    method: String.t(),
    params: map() | nil,
    id: integer()
  }

  @type json_rpc_response :: %{
    jsonrpc: String.t(),
    result: any() | nil,
    error: json_rpc_error() | nil,
    id: integer()
  }

  @type json_rpc_error :: %{
    code: integer(),
    message: String.t(),
    data: any() | nil
  }

  @type mcp_tool_call :: %{
    name: String.t(),
    arguments: map()
  }

  @doc """
  Build a JSON-RPC 2.0 request
  """
  def build_request(method, params, id \\ nil) do
    %{
      jsonrpc: "2.0",
      method: method,
      params: params,
      id: id || System.unique_integer([:positive])
    }
  end

  @doc """
  Build a success response
  """
  def build_response(result, id) do
    %{
      jsonrpc: "2.0",
      result: result,
      error: nil,
      id: id
    }
  end

  @doc """
  Build an error response
  """
  def build_error(code, message, id, data \\ nil) do
    %{
      jsonrpc: "2.0",
      result: nil,
      error: %{
        code: code,
        message: message,
        data: data
      },
      id: id
    }
  end
end
