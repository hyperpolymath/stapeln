# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Svalinn.VordrAdapter do
  @moduledoc """
  Automatic adapter for Vörðr communication with compile-time mode detection.

  ## Snap-In Magic

  This module automatically detects at **compile time** whether Vörðr is
  available as a local dependency:

  - **SNAPPED MODE** (Vörðr available): Direct function calls with zero overhead
  - **SEPARATE MODE** (Vörðr remote): HTTP/MCP communication

  ### Zero Configuration Required!

  The mode is detected automatically:
  ```elixir
  # This SAME code works in both modes:
  VordrAdapter.create_container("alpine:latest", name: "test")
  ```

  ### Performance

  - Snapped: ~1-10μs (microseconds)
  - Separate: ~1-5ms (milliseconds)
  - **1000x faster when snapped!**

  ### Deployment Modes

  1. **Separate** (default): Build svalinn alone, communicates via HTTP
  2. **Snapped**: Build cerro_stack, includes both apps in one process
  3. **Distributed**: Erlang distribution across nodes

  ## Usage

  ```elixir
  alias Svalinn.VordrAdapter

  # Works the same in all modes
  {:ok, container} = VordrAdapter.create_container(
    "alpine:latest",
    name: "my-container",
    config: %{read_only_root: true}
  )

  {:ok, result} = VordrAdapter.verify_image(
    "alpine:latest",
    "sha256:abc123..."
  )

  containers = VordrAdapter.list_containers()
  connected? = VordrAdapter.connected?()
  ```
  """

  alias CerroShared.ContainerTypes

  require Logger

  # ═══════════════════════════════════════════════════════════════════════════
  # COMPILE-TIME MODE DETECTION
  # ═══════════════════════════════════════════════════════════════════════════

  if Code.ensure_loaded?(Vordr.Container) do
    #
    # ╔═══════════════════════════════════════════════════════════════════════╗
    # ║  SNAPPED MODE: Vörðr Available Locally                                ║
    # ║  Direct function calls - zero serialization, nanosecond latency       ║
    # ╚═══════════════════════════════════════════════════════════════════════╝
    #

    @mode :snapped

    @doc "Get current adapter mode (compile-time constant)"
    def mode, do: @mode

    @doc "Check if connected to Vörðr (always true in snapped mode)"
    def connected?, do: true

    @doc "Create and start a container"
    @spec create_container(String.t(), ContainerTypes.create_opts()) ::
            {:ok, ContainerTypes.container_info()} | {:error, term()}
    def create_container(image, opts \\ %{}) do
      Logger.debug("[VordrAdapter:snapped] Creating container: #{image}")
      Vordr.Container.create(image, opts)
    end

    @doc "Verify an image with signature and SBOM checks"
    @spec verify_image(String.t(), String.t()) ::
            {:ok, ContainerTypes.verification_result()} | {:error, term()}
    def verify_image(image, digest) do
      Logger.debug("[VordrAdapter:snapped] Verifying image: #{image}")
      Vordr.Verification.verify(image, digest)
    end

    @doc "List all containers"
    @spec list_containers() :: {:ok, [ContainerTypes.container_info()]}
    def list_containers do
      Logger.debug("[VordrAdapter:snapped] Listing containers")
      Vordr.Container.list()
    end

    @doc "Get container information by ID"
    @spec get_container(String.t()) ::
            {:ok, ContainerTypes.container_info()} | {:error, :not_found}
    def get_container(id) do
      Logger.debug("[VordrAdapter:snapped] Getting container: #{id}")
      Vordr.Container.get(id)
    end

    @doc "Stop a running container"
    @spec stop_container(String.t(), keyword()) :: :ok | {:error, term()}
    def stop_container(id, opts \\ []) do
      Logger.debug("[VordrAdapter:snapped] Stopping container: #{id}")
      Vordr.Container.stop(id, opts)
    end

    @doc "Remove a container"
    @spec remove_container(String.t()) :: :ok | {:error, term()}
    def remove_container(id) do
      Logger.debug("[VordrAdapter:snapped] Removing container: #{id}")
      Vordr.Container.remove(id)
    end

    @doc "List available images"
    @spec list_images() :: {:ok, [ContainerTypes.image_info()]}
    def list_images do
      Logger.debug("[VordrAdapter:snapped] Listing images")
      Vordr.Image.list()
    end

  else
    #
    # ╔═══════════════════════════════════════════════════════════════════════╗
    # ║  SEPARATE MODE: Vörðr is Remote Service                               ║
    # ║  HTTP/MCP communication with JSON-RPC 2.0                             ║
    # ╚═══════════════════════════════════════════════════════════════════════╝
    #

    alias CerroShared.MCPTypes

    @mode :separate
    @endpoint Application.compile_env(:svalinn, :vordr_endpoint, "http://localhost:8080")

    @doc "Get current adapter mode (compile-time constant)"
    def mode, do: @mode

    @doc "Check if connected to Vörðr via HTTP health check"
    def connected? do
      case Req.get("#{@endpoint}/health", receive_timeout: 1000) do
        {:ok, %{status: 200}} -> true
        _ -> false
      end
    end

    @doc "Create and start a container via MCP"
    @spec create_container(String.t(), ContainerTypes.create_opts()) ::
            {:ok, ContainerTypes.container_info()} | {:error, term()}
    def create_container(image, opts \\ %{}) do
      Logger.debug("[VordrAdapter:separate] Creating container via MCP: #{image}")

      mcp_call("vordr_container_create", %{
        image: image,
        name: Map.get(opts, :name),
        config: Map.get(opts, :config)
      })
    end

    @doc "Verify an image via MCP"
    @spec verify_image(String.t(), String.t()) ::
            {:ok, ContainerTypes.verification_result()} | {:error, term()}
    def verify_image(image, digest) do
      Logger.debug("[VordrAdapter:separate] Verifying image via MCP: #{image}")

      mcp_call("vordr_verify_image", %{
        image: image,
        digest: digest,
        check_sbom: true,
        check_signature: true
      })
    end

    @doc "List all containers via MCP"
    @spec list_containers() :: {:ok, [ContainerTypes.container_info()]}
    def list_containers do
      Logger.debug("[VordrAdapter:separate] Listing containers via MCP")
      mcp_call("vordr_container_list", %{})
    end

    @doc "Get container information via MCP"
    @spec get_container(String.t()) ::
            {:ok, ContainerTypes.container_info()} | {:error, :not_found}
    def get_container(id) do
      Logger.debug("[VordrAdapter:separate] Getting container via MCP: #{id}")
      mcp_call("vordr_container_get", %{id: id})
    end

    @doc "Stop a container via MCP"
    @spec stop_container(String.t(), keyword()) :: :ok | {:error, term()}
    def stop_container(id, opts \\ []) do
      Logger.debug("[VordrAdapter:separate] Stopping container via MCP: #{id}")

      mcp_call("vordr_container_stop", %{
        id: id,
        timeout: Keyword.get(opts, :timeout, 10)
      })
    end

    @doc "Remove a container via MCP"
    @spec remove_container(String.t()) :: :ok | {:error, term()}
    def remove_container(id) do
      Logger.debug("[VordrAdapter:separate] Removing container via MCP: #{id}")
      mcp_call("vordr_container_remove", %{id: id})
    end

    @doc "List images via MCP"
    @spec list_images() :: {:ok, [ContainerTypes.image_info()]}
    def list_images do
      Logger.debug("[VordrAdapter:separate] Listing images via MCP")
      mcp_call("vordr_image_list", %{})
    end

    # ─────────────────────────────────────────────────────────────────────────
    # MCP Communication Layer
    # ─────────────────────────────────────────────────────────────────────────

    defp mcp_call(method, params) do
      request = MCPTypes.build_request("tools/call", %{
        name: method,
        arguments: params
      })

      case Req.post("#{@endpoint}/mcp", json: request) do
        {:ok, %{status: 200, body: %{"result" => result}}} ->
          {:ok, result}

        {:ok, %{status: 200, body: %{"error" => error}}} ->
          {:error, error["message"]}

        {:ok, %{status: status}} ->
          {:error, "HTTP #{status}"}

        {:error, reason} ->
          Logger.error("[VordrAdapter:separate] MCP call failed: #{inspect(reason)}")
          {:error, reason}
      end
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # COMMON HELPERS (Available in Both Modes)
  # ═══════════════════════════════════════════════════════════════════════════

  @doc """
  Get a human-readable description of the current mode.

  Returns:
  - `:snapped` - Vörðr is embedded, direct function calls
  - `:separate` - Vörðr is remote, HTTP/MCP communication
  """
  def mode_description do
    case mode() do
      :snapped -> "Snapped (direct calls, ~1μs latency)"
      :separate -> "Separate (HTTP/MCP, ~1-5ms latency)"
    end
  end

  @doc """
  Get health status for monitoring.

  Returns a map with adapter mode and connection status.
  """
  def health do
    %{
      mode: mode(),
      mode_description: mode_description(),
      connected: connected?(),
      endpoint: endpoint_info()
    }
  end

  defp endpoint_info do
    case mode() do
      :snapped -> "internal"
      :separate -> Application.get_env(:svalinn, :vordr_endpoint, "http://localhost:8080")
    end
  end
end
