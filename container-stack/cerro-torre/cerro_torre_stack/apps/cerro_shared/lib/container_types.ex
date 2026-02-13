# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule CerroShared.ContainerTypes do
  @moduledoc """
  Shared container types used by both Svalinn (gateway) and Vörðr (runtime).

  These types ensure zero impedance mismatch between services whether they
  communicate via HTTP/MCP or direct function calls in snapped mode.
  """

  @type container_state :: :created | :running | :stopped | :paused

  @type container_info :: %{
    id: String.t(),
    name: String.t(),
    image: String.t(),
    image_digest: String.t(),
    state: container_state(),
    policy_verdict: String.t(),
    created_at: DateTime.t() | nil,
    started_at: DateTime.t() | nil
  }

  @type image_info :: %{
    id: String.t(),
    tags: [String.t()],
    digest: String.t(),
    size: integer()
  }

  @type create_opts :: %{
    optional(:name) => String.t(),
    optional(:config) => container_config()
  }

  @type container_config :: %{
    optional(:privileged) => boolean(),
    optional(:read_only_root) => boolean(),
    optional(:network_mode) => String.t(),
    optional(:memory) => integer(),
    optional(:cpus) => float()
  }

  @type verification_result :: %{
    verified: boolean(),
    signatures: [String.t()],
    sbom: map() | nil
  }

  @type policy :: %{
    version: String.t(),
    allowed_registries: [String.t()],
    denied_images: [String.t()]
  }
end
