# SPDX-License-Identifier: PMPL-1.0-or-later
# Vordr Elixir Orchestrator
# Container state machine with reversibility guarantees

defmodule Vordr.MixProject do
  use Mix.Project

  def project do
    [
      app: :vordr,
      version: "0.1.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: "Formally verified container orchestration with reversibility",
      package: package(),
      docs: docs()
    ]
  end

  def application do
    [
      extra_applications: [:logger, :crypto],
      mod: {Vordr.Application, []}
    ]
  end

  defp deps do
    [
      # State machine
      {:gen_state_machine, "~> 3.0"},

      # JSON handling
      {:jason, "~> 1.4"},

      # HTTP client (for MCP communication)
      # Removed Req, Finch, Mint due to compatibility issues.
      # Using HTTPoison as a stable alternative.
      {:httpoison, "~> 2.0"},

      # Telemetry for observability
      {:telemetry, "~> 1.2"},
      {:telemetry_metrics, "~> 1.0"},
      {:telemetry_poller, "~> 1.1"},

      # Testing
      {:stream_data, "~> 1.0", only: [:test, :dev]},

      # Development
      {:dialyxir, "~> 1.4", only: [:dev], runtime: false},
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:ex_doc, "~> 0.31", only: :dev, runtime: false}
    ]
  end

  defp package do
    [
      name: "vordr",
      licenses: ["PMPL-1.0-or-later"],
      links: %{
        "GitLab" => "https://gitlab.com/hyperpolymath/vordr",
        "GitHub" => "https://github.com/hyperpolymath/vordr"
      }
    ]
  end

  defp docs do
    [
      main: "Vordr",
      extras: ["README.md"]
    ]
  end
end
