defmodule Stapeln.Stacks do
  @moduledoc """
  Stack API context.

  All external API entrypoints should use this context so REST and GraphQL share
  the same behavior and ABI/FFI boundary (`Stapeln.NativeBridge`).
  """

  alias Stapeln.NativeBridge
  alias Stapeln.VeriSimDB

  @spec list() :: {:ok, [map()]}
  def list do
    NativeBridge.list_stacks()
  end

  @spec create(map()) :: {:ok, map()} | {:error, term()}
  def create(attrs) when is_map(attrs) do
    case NativeBridge.create_stack(attrs) do
      {:ok, stack} = ok ->
        VeriSimDB.record(:stack_created, %{stack_id: stack.id, name: stack.name})
        ok

      error ->
        error
    end
  end

  @spec fetch(pos_integer()) :: {:ok, map()} | {:error, :not_found}
  def fetch(id) when is_integer(id) and id > 0 do
    NativeBridge.get_stack(id)
  end

  @spec update(pos_integer(), map()) :: {:ok, map()} | {:error, :not_found}
  def update(id, attrs) when is_integer(id) and id > 0 and is_map(attrs) do
    case NativeBridge.update_stack(id, attrs) do
      {:ok, stack} = ok ->
        VeriSimDB.record(:stack_updated, %{stack_id: stack.id, name: stack.name})
        ok

      error ->
        error
    end
  end

  @spec validate(pos_integer()) :: {:ok, map()} | {:error, :not_found}
  def validate(id) when is_integer(id) and id > 0 do
    NativeBridge.validate_stack(id)
  end

  @spec security_scan(pos_integer()) :: {:ok, map()} | {:error, :not_found}
  def security_scan(id) when is_integer(id) and id > 0 do
    case NativeBridge.security_scan(id) do
      {:ok, report} = ok ->
        VeriSimDB.record(:security_scan, %{
          stack_id: id,
          grade: Map.get(report, :grade),
          vulnerability_count: length(Map.get(report, :vulnerabilities, []))
        })

        ok

      error ->
        error
    end
  end

  @spec gap_analysis(pos_integer()) :: {:ok, map()} | {:error, :not_found}
  def gap_analysis(id) when is_integer(id) and id > 0 do
    case NativeBridge.gap_analysis(id) do
      {:ok, report} = ok ->
        VeriSimDB.record(:gap_analysis, %{
          stack_id: id,
          gap_count: length(Map.get(report, :gaps, []))
        })

        ok

      error ->
        error
    end
  end
end
