defmodule Svalinn.MixProject do
  use Mix.Project

  def project do
    [
      app: :svalinn,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.15",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      deps: deps(),
      listeners: [Phoenix.CodeReloader]
    ]
  end

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  def application do
    [
      mod: {Svalinn.Application, []},
      extra_applications: [:logger, :runtime_tools]
    ]
  end

  def cli do
    [
      preferred_envs: [precommit: :test]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
      # Phoenix framework
      {:phoenix, "~> 1.8.3"},
      {:phoenix_live_dashboard, "~> 0.8.3"},
      {:telemetry_metrics, "~> 1.0"},
      {:telemetry_poller, "~> 1.0"},
      {:gettext, "~> 1.0"},
      {:jason, "~> 1.2"},
      {:bandit, "~> 1.5"},

      # Shared types (umbrella)
      {:cerro_shared, in_umbrella: true},

      # HTTP client for remote mode
      # Note: Vörðr dependency is NOT listed here - it's detected at compile time!
      # When vordr app exists in umbrella, adapter automatically uses direct calls.
      # When vordr doesn't exist, adapter uses HTTP/MCP via Req.
      {:req, "~> 0.5"},

      # Authentication
      {:joken, "~> 2.6"},
      {:jose, "~> 1.11"},

      # JSON Schema validation
      {:ex_json_schema, "~> 0.10"},

      # CORS
      {:cors_plug, "~> 3.0"},

      # Testing
      {:dns_cluster, "~> 0.2.0"},
      {:swoosh, "~> 1.16"}
    ]
  end

  # Aliases are shortcuts or tasks specific to the current project.
  # For example, to install project dependencies and perform other setup tasks, run:
  #
  #     $ mix setup
  #
  # See the documentation for `Mix` for more info on aliases.
  defp aliases do
    [
      setup: ["deps.get"],
      precommit: ["compile --warnings-as-errors", "deps.unlock --unused", "format", "test"]
    ]
  end
end
