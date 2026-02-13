# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Vordr.Application do
  @moduledoc """
  Vordr OTP Application

  Starts the supervision tree for container orchestration.
  """

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Container registry (ETS-backed)
      {Registry, keys: :unique, name: Vordr.ContainerRegistry},

      # Reversibility journal
      Vordr.Reversibility.Journal,

      # Container supervisor (DynamicSupervisor for containers)
      {DynamicSupervisor, strategy: :one_for_one, name: Vordr.ContainerSupervisor},

      # Telemetry supervisor
      Vordr.Telemetry
    ]

    opts = [strategy: :one_for_one, name: Vordr.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
