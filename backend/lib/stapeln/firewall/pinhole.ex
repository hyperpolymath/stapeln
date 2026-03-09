# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Stapeln.Firewall.Pinhole do
  @moduledoc """
  Ephemeral pinhole firewall rules.

  A pinhole is a temporary, scoped network access rule:
  - Source service -> destination service
  - Specific port(s)
  - Time-to-live (auto-expires)
  - Logged and auditable
  """

  @type status :: :active | :expired | :revoked

  @type t :: %__MODULE__{
          id: String.t(),
          source: String.t(),
          destination: String.t(),
          port: non_neg_integer(),
          protocol: String.t(),
          ttl_seconds: non_neg_integer(),
          created_at: DateTime.t(),
          expires_at: DateTime.t(),
          status: status(),
          reason: String.t()
        }

  @derive {Jason.Encoder,
           only: [
             :id,
             :source,
             :destination,
             :port,
             :protocol,
             :ttl_seconds,
             :created_at,
             :expires_at,
             :status,
             :reason
           ]}

  defstruct [
    :id,
    :source,
    :destination,
    :port,
    :protocol,
    :ttl_seconds,
    :created_at,
    :expires_at,
    :status,
    :reason
  ]

  @doc """
  Creates a new Pinhole struct with auto-generated ID and computed expiry.
  """
  @spec new(String.t(), String.t(), non_neg_integer(), String.t(), non_neg_integer(), String.t()) ::
          t()
  def new(source, destination, port, protocol, ttl_seconds, reason) do
    now = DateTime.utc_now()

    %__MODULE__{
      id: generate_id(),
      source: source,
      destination: destination,
      port: port,
      protocol: protocol || "tcp",
      ttl_seconds: ttl_seconds,
      created_at: now,
      expires_at: DateTime.add(now, ttl_seconds, :second),
      status: :active,
      reason: reason
    }
  end

  @doc """
  Returns true if this pinhole has expired based on current time.
  """
  @spec expired?(t()) :: boolean()
  def expired?(%__MODULE__{expires_at: expires_at, status: :active}) do
    DateTime.compare(DateTime.utc_now(), expires_at) != :lt
  end

  def expired?(%__MODULE__{status: status}) when status in [:expired, :revoked], do: true

  @doc """
  Marks a pinhole as revoked.
  """
  @spec revoke(t()) :: t()
  def revoke(%__MODULE__{} = pinhole) do
    %{pinhole | status: :revoked}
  end

  @doc """
  Marks a pinhole as expired.
  """
  @spec expire(t()) :: t()
  def expire(%__MODULE__{} = pinhole) do
    %{pinhole | status: :expired}
  end

  @doc """
  Serializes the pinhole to a JSON-friendly map.
  """
  @spec to_map(t()) :: map()
  def to_map(%__MODULE__{} = p) do
    %{
      id: p.id,
      source: p.source,
      destination: p.destination,
      port: p.port,
      protocol: p.protocol,
      ttl_seconds: p.ttl_seconds,
      created_at: DateTime.to_iso8601(p.created_at),
      expires_at: DateTime.to_iso8601(p.expires_at),
      status: Atom.to_string(p.status),
      reason: p.reason
    }
  end

  # Generate a short unique ID for the pinhole.
  defp generate_id do
    :crypto.strong_rand_bytes(8) |> Base.url_encode64(padding: false)
  end
end
