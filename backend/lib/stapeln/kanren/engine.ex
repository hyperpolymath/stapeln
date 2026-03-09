# SPDX-License-Identifier: PMPL-1.0-or-later
# engine.ex - High-level miniKanren security reasoning API for stapeln

defmodule Stapeln.Kanren.Engine do
  @moduledoc """
  High-level API for the miniKanren security reasoning engine.

  Wraps the core miniKanren primitives and security rules into
  a convenient interface that the SecurityScanner can call.
  """

  alias Stapeln.Kanren.SecurityRules

  @doc """
  Analyze a stack's ports and return security findings from the
  miniKanren reasoning engine.

  Returns a list of findings, each a map with :port, :severity,
  :message, and :protocol fields.
  """
  @spec analyze_ports([non_neg_integer()]) :: [map()]
  def analyze_ports(ports) when is_list(ports) do
    db_vulns = SecurityRules.query_db_vulnerabilities()

    # For each port in the stack, check if it matches a known vulnerability
    Enum.flat_map(ports, fn port ->
      # Check database vulnerabilities
      db_findings =
        db_vulns
        |> Enum.filter(fn {p, _sev, _msg} -> p == port end)
        |> Enum.map(fn {p, sev, msg} ->
          %{
            port: p,
            severity: sev,
            message: msg,
            source: :kanren
          }
        end)

      # Check port protocol info
      proto_findings =
        SecurityRules.query_port_info(port)
        |> Enum.map(fn {protocol, risk} ->
          %{
            port: port,
            protocol: protocol,
            risk: risk,
            source: :kanren
          }
        end)

      db_findings ++ proto_findings
    end)
  end

  @doc """
  Query the severity classification for a given severity atom.
  Returns the numeric score.
  """
  @spec severity_score(atom()) :: non_neg_integer()
  def severity_score(severity) do
    case SecurityRules.query_severity_scores()
         |> Enum.find(fn {sev, _score} -> sev == severity end) do
      {_sev, score} -> score
      nil -> 0
    end
  end

  @doc """
  Run a full security reasoning pass on a stack definition.
  Returns enriched vulnerability data suitable for the SecurityScanner.
  """
  @spec reason(map()) :: map()
  def reason(stack) when is_map(stack) do
    services = Map.get(stack, :services, Map.get(stack, "services", []))

    ports =
      services
      |> Enum.map(fn svc ->
        port = Map.get(svc, :port, Map.get(svc, "port"))

        cond do
          is_integer(port) -> port
          is_binary(port) ->
            case Integer.parse(port) do
              {p, _} -> p
              :error -> nil
            end
          true -> nil
        end
      end)
      |> Enum.reject(&is_nil/1)

    port_findings = analyze_ports(ports)

    %{
      engine: :kanren,
      port_findings: port_findings,
      total_findings: length(port_findings),
      ports_analyzed: length(ports),
      severity_distribution: count_by_severity(port_findings)
    }
  end

  defp count_by_severity(findings) do
    findings
    |> Enum.reduce(%{critical: 0, high: 0, medium: 0, low: 0, info: 0}, fn finding, acc ->
      severity = Map.get(finding, :severity, Map.get(finding, :risk, :info))
      Map.update(acc, severity, 1, &(&1 + 1))
    end)
  end
end
