# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule SelurVordr.MixProject do
  use Mix.Project

  def project do
    [
      app: :selur_vordr,
      version: "1.0.0",
      elixir: "~> 1.16",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      compilers: [:rustler] ++ Mix.compilers(),
      rustler_crates: [
        selur_nif: [
          path: "native",
          mode: rustler_mode(Mix.env())
        ]
      ]
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:rustler, "~> 0.34"}
    ]
  end

  defp rustler_mode(:prod), do: :release
  defp rustler_mode(_), do: :debug
end
