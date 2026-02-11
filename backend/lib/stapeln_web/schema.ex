defmodule StapelnWeb.Schema do
  use Absinthe.Schema

  alias Stapeln.Stacks

  object :service do
    field(:name, non_null(:string))
    field(:kind, :string)
    field(:port, :integer)
  end

  object :stack do
    field(:id, non_null(:id))
    field(:name, non_null(:string))
    field(:description, :string)
    field(:services, non_null(list_of(non_null(:service))))
    field(:created_at, non_null(:string), resolve: &resolve_datetime(:created_at, &1, &2, &3))
    field(:updated_at, non_null(:string), resolve: &resolve_datetime(:updated_at, &1, &2, &3))
  end

  object :finding do
    field(:id, non_null(:string))
    field(:severity, non_null(:string))
    field(:message, non_null(:string))
    field(:hint, non_null(:string))
  end

  object :validation_result do
    field(:score, non_null(:integer))
    field(:findings, non_null(list_of(non_null(:finding))))
    field(:stack, non_null(:stack))
  end

  input_object :service_input do
    field(:name, non_null(:string))
    field(:kind, :string)
    field(:port, :integer)
  end

  input_object :stack_input do
    field(:name, :string)
    field(:description, :string)
    field(:services, list_of(non_null(:service_input)))
  end

  query do
    field :stack, :stack do
      arg(:id, non_null(:id))

      resolve(fn %{id: id}, _ ->
        with {:ok, parsed_id} <- parse_id(id) do
          Stacks.fetch(parsed_id)
        end
      end)
    end

    field :stacks, non_null(list_of(non_null(:stack))) do
      resolve(fn _, _ ->
        Stacks.list()
      end)
    end
  end

  mutation do
    field :create_stack, :stack do
      arg(:input, non_null(:stack_input))

      resolve(fn %{input: input}, _ ->
        Stacks.create(input)
      end)
    end

    field :update_stack, :stack do
      arg(:id, non_null(:id))
      arg(:input, non_null(:stack_input))

      resolve(fn %{id: id, input: input}, _ ->
        with {:ok, parsed_id} <- parse_id(id) do
          Stacks.update(parsed_id, input)
        end
      end)
    end

    field :validate_stack, :validation_result do
      arg(:id, non_null(:id))

      resolve(fn %{id: id}, _ ->
        with {:ok, parsed_id} <- parse_id(id) do
          with {:ok, report} <- Stacks.validate(parsed_id) do
            {:ok, serialize_report(report)}
          end
        end
      end)
    end
  end

  defp resolve_datetime(field, source, _args, _resolution) do
    case Map.get(source, field) do
      %DateTime{} = value -> {:ok, DateTime.to_iso8601(value)}
      value when is_binary(value) -> {:ok, value}
      _ -> {:error, "missing datetime value"}
    end
  end

  defp parse_id(id) do
    case Integer.parse(to_string(id)) do
      {value, ""} when value > 0 -> {:ok, value}
      _ -> {:error, "invalid id"}
    end
  end

  defp serialize_report(report) do
    %{
      score: report.score,
      stack: report.stack,
      findings:
        Enum.map(report.findings, fn finding ->
          %{
            id: finding.id,
            severity: Atom.to_string(finding.severity),
            message: finding.message,
            hint: finding.hint
          }
        end)
    }
  end
end
