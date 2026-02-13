# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Vordr.Telemetry do
  @moduledoc """
  Telemetry and Observability

  Provides metrics and events for container orchestration monitoring.

  ## Events

  - `[:vordr, :container, :state_change]` - Container state transitions
  - `[:vordr, :journal, :transition]` - Journal entries
  - `[:vordr, :reversibility, :rollback]` - Rollback operations

  ## Metrics

  - `vordr.containers.total` - Total container count
  - `vordr.containers.by_state` - Containers by state
  - `vordr.journal.entries` - Journal entry count
  """

  use Supervisor

  def start_link(arg) do
    Supervisor.start_link(__MODULE__, arg, name: __MODULE__)
  end

  @impl true
  def init(_arg) do
    children = [
      {:telemetry_poller, measurements: periodic_measurements(), period: 10_000}
    ]

    # Attach event handlers
    attach_handlers()

    Supervisor.init(children, strategy: :one_for_one)
  end

  defp attach_handlers do
    :telemetry.attach_many(
      "vordr-logger",
      [
        [:vordr, :container, :state_change],
        [:vordr, :journal, :transition],
        [:vordr, :reversibility, :rollback]
      ],
      &handle_event/4,
      nil
    )
  end

  defp handle_event([:vordr, :container, :state_change], _measurements, metadata, _config) do
    require Logger
    Logger.debug("Telemetry: container #{metadata.container_id} #{metadata.from} → #{metadata.to}")
  end

  defp handle_event([:vordr, :journal, :transition], _measurements, metadata, _config) do
    require Logger
    Logger.debug("Telemetry: journal #{metadata.container_id} #{metadata.from} → #{metadata.to}")
  end

  defp handle_event([:vordr, :reversibility, :rollback], _measurements, metadata, _config) do
    require Logger
    Logger.info("Telemetry: rollback #{metadata.container_id} steps=#{metadata.steps}")
  end

  defp handle_event(_event, _measurements, _metadata, _config), do: :ok

  defp periodic_measurements do
    [
      {__MODULE__, :container_metrics, []}
    ]
  end

  @doc false
  def container_metrics do
    counts = Vordr.Containers.count()

    :telemetry.execute(
      [:vordr, :containers],
      %{total: Map.get(counts, :total, 0)},
      %{}
    )

    Enum.each(counts, fn {state, count} ->
      :telemetry.execute(
        [:vordr, :containers, :by_state],
        %{count: count},
        %{state: state}
      )
    end)
  end
end
