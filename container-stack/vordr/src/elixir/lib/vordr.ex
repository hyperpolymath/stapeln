# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Vordr do
  @moduledoc """
  Vordr - Formally Verified Container Orchestration

  The Elixir orchestration layer provides:

  - **Container State Machine**: Type-safe state transitions with GenStateMachine
  - **Reversibility Layer**: Bennett-reversible operations for safe rollback
  - **Supervision Tree**: Fault-tolerant container management

  ## Architecture

  ```
  ┌─────────────────────────────────────────────────────────────────┐
  │                    Vordr.Application                             │
  │  ┌───────────────────────────────────────────────────────────┐  │
  │  │                 Vordr.Supervisor                           │  │
  │  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────────┐   │  │
  │  │  │ Registry    │  │ Container   │  │ Reversibility   │   │  │
  │  │  │ (ETS)       │  │ Supervisor  │  │ Journal         │   │  │
  │  │  └─────────────┘  └──────┬──────┘  └─────────────────┘   │  │
  │  │                          │                                 │  │
  │  │           ┌──────────────┼──────────────┐                 │  │
  │  │           ▼              ▼              ▼                 │  │
  │  │    ┌───────────┐  ┌───────────┐  ┌───────────┐          │  │
  │  │    │ Container │  │ Container │  │ Container │          │  │
  │  │    │ GenServer │  │ GenServer │  │ GenServer │          │  │
  │  │    │ (c-abc)   │  │ (c-def)   │  │ (c-xyz)   │          │  │
  │  │    └───────────┘  └───────────┘  └───────────┘          │  │
  │  └───────────────────────────────────────────────────────────┘  │
  └─────────────────────────────────────────────────────────────────┘
  ```

  ## Example Usage

      # Start a container
      {:ok, pid} = Vordr.Containers.start_container("nginx:1.26", name: "web")

      # Transition state
      :ok = Vordr.Containers.start(pid)
      :ok = Vordr.Containers.pause(pid)
      :ok = Vordr.Containers.resume(pid)

      # Rollback with reversibility
      :ok = Vordr.Reversibility.rollback(pid, steps: 2)

  ## State Machine

  The container lifecycle follows a formally verified state machine:

      ImageOnly → Created → Running ⇄ Paused
                     │          │        │
                     │          ▼        │
                     │       Stopped ←───┘
                     │          │
                     ▼          ▼
                   Removed ← ───┘

  Each transition is logged for reversibility.
  """

  @doc """
  Returns the current version of Vordr.
  """
  @spec version() :: String.t()
  def version, do: "0.1.0"

  @doc """
  Returns system information.
  """
  @spec info() :: map()
  def info do
    %{
      version: version(),
      elixir_version: System.version(),
      otp_release: System.otp_release(),
      containers: Vordr.Containers.count(),
      uptime_ms: System.monotonic_time(:millisecond)
    }
  end
end
