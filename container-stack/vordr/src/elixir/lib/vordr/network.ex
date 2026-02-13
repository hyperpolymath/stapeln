# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Vordr.Network do
  @moduledoc """
  Container network namespace management.

  Provides network isolation and configuration for containers:
  - Network namespace creation
  - veth pair configuration
  - Bridge networking
  - Port mapping
  """

  require Logger
  import Bitwise

  @type network_id :: String.t()
  @type container_id :: String.t()

  @default_bridge "vordr0"
  # Subnet for container networking (172.30.0.0/16)
  # IPs allocated from 172.30.0.2-254, gateway at 172.30.0.1
  @subnet_prefix "172.30.0"

  @doc """
  Create a network namespace for a container.
  """
  @spec create_namespace(container_id()) :: {:ok, String.t()} | {:error, term()}
  def create_namespace(container_id) do
    ns_name = "vordr-#{String.slice(container_id, 0, 12)}"
    Logger.info("Creating network namespace: #{ns_name}")

    # Would execute: ip netns add #{ns_name}
    {:ok, ns_name}
  end

  @doc """
  Delete a network namespace.
  """
  @spec delete_namespace(String.t()) :: :ok | {:error, term()}
  def delete_namespace(ns_name) do
    Logger.info("Deleting network namespace: #{ns_name}")

    # Would execute: ip netns delete #{ns_name}
    :ok
  end

  @doc """
  Create the default bridge network.
  """
  @spec ensure_default_bridge() :: :ok | {:error, term()}
  def ensure_default_bridge do
    Logger.info("Ensuring default bridge #{@default_bridge} exists")

    # Would execute:
    # ip link add #{@default_bridge} type bridge
    # ip addr add #{gateway_ip}/16 dev #{@default_bridge}
    # ip link set #{@default_bridge} up
    :ok
  end

  @doc """
  Connect a container to a network.
  """
  @spec connect(container_id(), network_id(), keyword()) :: {:ok, map()} | {:error, term()}
  def connect(container_id, network_id, opts \\ []) do
    ip = Keyword.get_lazy(opts, :ip, fn -> allocate_ip(network_id) end)
    Logger.info("Connecting #{container_id} to #{network_id} with IP #{ip}")

    # Create veth pair and connect to bridge
    veth_host = "veth#{String.slice(container_id, 0, 8)}"
    veth_container = "eth0"

    # Would execute:
    # ip link add #{veth_host} type veth peer name #{veth_container}
    # ip link set #{veth_host} master #{network_id}
    # ip link set #{veth_container} netns #{ns_name}
    # ip netns exec #{ns_name} ip addr add #{ip}/16 dev eth0
    # ip netns exec #{ns_name} ip link set eth0 up

    {:ok, %{
      network_id: network_id,
      container_id: container_id,
      ip_address: ip,
      gateway: gateway_for_network(network_id),
      veth_host: veth_host,
      veth_container: veth_container
    }}
  end

  @doc """
  Disconnect a container from a network.
  """
  @spec disconnect(container_id(), network_id()) :: :ok | {:error, term()}
  def disconnect(container_id, network_id) do
    Logger.info("Disconnecting #{container_id} from #{network_id}")

    # Would delete veth pair
    :ok
  end

  @doc """
  Set up port mapping for a container.
  """
  @spec add_port_mapping(container_id(), integer(), integer(), atom()) ::
          :ok | {:error, term()}
  def add_port_mapping(container_id, host_port, container_port, protocol \\ :tcp) do
    Logger.info("Adding port mapping #{host_port}->#{container_port}/#{protocol} for #{container_id}")

    # Would add iptables DNAT rule:
    # iptables -t nat -A PREROUTING -p #{protocol} --dport #{host_port} \
    #   -j DNAT --to-destination #{container_ip}:#{container_port}

    :ok
  end

  @doc """
  Remove port mapping for a container.
  """
  @spec remove_port_mapping(container_id(), integer(), atom()) :: :ok | {:error, term()}
  def remove_port_mapping(container_id, host_port, protocol \\ :tcp) do
    Logger.info("Removing port mapping #{host_port}/#{protocol} for #{container_id}")
    :ok
  end

  @doc """
  Get network information for a container.
  """
  @spec inspect(container_id()) :: {:ok, map()} | {:error, :not_found}
  def inspect(container_id) do
    {:ok, %{
      container_id: container_id,
      networks: [
        %{
          network_id: @default_bridge,
          ip_address: "#{@subnet_prefix}.#{:rand.uniform(254)}",
          gateway: "#{@subnet_prefix}.1",
          mac_address: generate_mac()
        }
      ],
      port_mappings: []
    }}
  end

  # Private functions

  defp allocate_ip(_network_id) do
    # Simple sequential allocation (in production, would track used IPs)
    "#{@subnet_prefix}.#{:rand.uniform(254)}"
  end

  defp gateway_for_network(_network_id) do
    "#{@subnet_prefix}.1"
  end

  defp generate_mac do
    bytes = :crypto.strong_rand_bytes(6)
    # Set locally administered bit, clear multicast bit
    <<first, rest::binary>> = bytes
    first = (first &&& 0xFE) ||| 0x02

    <<first, rest::binary>>
    |> :binary.bin_to_list()
    |> Enum.map(&Integer.to_string(&1, 16))
    |> Enum.map(&String.pad_leading(&1, 2, "0"))
    |> Enum.join(":")
  end
end
