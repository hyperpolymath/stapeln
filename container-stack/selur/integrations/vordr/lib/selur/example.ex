# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Vordr.Selur.Example do
  @moduledoc """
  Example usage of selur bridge from Elixir/Vörðr.
  """

  alias Vordr.Selur.Bridge

  def run do
    wasm_path = Path.expand("../../zig/zig-out/bin/selur.wasm", __DIR__)

    IO.puts("=== selur Vörðr Integration Example ===\n")

    case Bridge.new(wasm_path) do
      {:ok, bridge} ->
        IO.puts("✓ Bridge created successfully")

        case Bridge.memory_size(bridge) do
          {:ok, size} ->
            IO.puts("✓ Memory size: #{size} bytes\n")

          {:error, reason} ->
            IO.puts("✗ Failed to get memory size: #{reason}")
        end

        # Create container
        case Bridge.create_container(bridge, "nginx:latest") do
          {:ok, container_id} ->
            IO.puts("✓ Container created: #{container_id}")

            # Start container
            case Bridge.start_container(bridge, container_id) do
              :ok ->
                IO.puts("✓ Container started: #{container_id}")

                # List containers
                case Bridge.list_containers(bridge) do
                  {:ok, containers} ->
                    IO.puts("✓ Running containers (#{length(containers)}):")
                    Enum.each(containers, fn id -> IO.puts("  - #{id}") end)

                  {:error, reason} ->
                    IO.puts("✗ List failed: #{reason}")
                end

                # Inspect container
                case Bridge.inspect_container(bridge, container_id) do
                  {:ok, info_json} ->
                    IO.puts("✓ Container info: #{String.slice(info_json, 0..100)}...")

                  {:error, reason} ->
                    IO.puts("✗ Inspect failed: #{reason}")
                end

                # Stop container
                case Bridge.stop_container(bridge, container_id) do
                  :ok ->
                    IO.puts("✓ Container stopped: #{container_id}")

                  {:error, reason} ->
                    IO.puts("✗ Stop failed: #{reason}")
                end

                # Delete container
                case Bridge.delete_container(bridge, container_id) do
                  :ok ->
                    IO.puts("✓ Container deleted: #{container_id}")

                  {:error, reason} ->
                    IO.puts("✗ Delete failed: #{reason}")
                end

              {:error, reason} ->
                IO.puts("✗ Start failed: #{reason}")
            end

          {:error, reason} ->
            IO.puts("✗ Create failed: #{reason}")
        end

        # Cleanup
        :ok = Bridge.free(bridge)
        IO.puts("\n✓ Bridge freed")

      {:error, reason} ->
        IO.puts("✗ Bridge creation failed: #{reason}")
    end
  end
end
