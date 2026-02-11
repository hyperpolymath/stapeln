defmodule Stapeln.ValidationEngine do
  @moduledoc """
  Lightweight API validation rules used by security and gap-analysis features.
  """

  @type finding :: %{
          id: String.t(),
          severity: :low | :medium | :high,
          message: String.t(),
          hint: String.t()
        }

  @type report :: %{
          score: non_neg_integer(),
          findings: [finding()]
        }

  @spec validate(map()) :: report()
  def validate(stack) when is_map(stack) do
    services = Map.get(stack, :services, [])

    findings =
      empty_services_finding(services) ++
        duplicate_names_findings(services) ++ missing_kind_findings(services)

    %{
      score: score(findings),
      findings: findings
    }
  end

  defp empty_services_finding([]) do
    [
      %{
        id: "services.empty",
        severity: :high,
        message: "Stack has no services configured.",
        hint: "Define at least one service before simulation or deployment."
      }
    ]
  end

  defp empty_services_finding(_), do: []

  defp duplicate_names_findings(services) do
    services
    |> Enum.frequencies_by(fn service -> Map.get(service, :name, "") end)
    |> Enum.filter(fn {name, count} -> name != "" and count > 1 end)
    |> Enum.map(fn {name, count} ->
      %{
        id: "services.duplicate_name.#{name}",
        severity: :high,
        message: "Service name `#{name}` appears #{count} times.",
        hint: "Give each service a unique name to avoid rule collisions."
      }
    end)
  end

  defp missing_kind_findings(services) do
    services
    |> Enum.with_index(1)
    |> Enum.flat_map(fn {service, index} ->
      case Map.get(service, :kind) do
        nil ->
          [
            %{
              id: "services.kind_missing.#{index}",
              severity: :medium,
              message: "Service ##{index} has no kind.",
              hint: "Set `kind` (for example `web`, `worker`, or `db`)."
            }
          ]

        "" ->
          [
            %{
              id: "services.kind_empty.#{index}",
              severity: :medium,
              message: "Service ##{index} has an empty kind.",
              hint: "Set a non-empty `kind` value."
            }
          ]

        _ ->
          []
      end
    end)
  end

  defp score(findings) do
    penalty =
      Enum.reduce(findings, 0, fn finding, acc ->
        acc +
          case finding.severity do
            :high -> 20
            :medium -> 10
            :low -> 5
          end
      end)

    max(0, 100 - penalty)
  end
end
