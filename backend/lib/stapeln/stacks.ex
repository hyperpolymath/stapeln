defmodule Stapeln.Stacks do
  @moduledoc """
  Stack API context.

  All external API entrypoints should use this context so REST and GraphQL share
  the same behavior and ABI/FFI boundary (`Stapeln.NativeBridge`).
  """

  alias Stapeln.NativeBridge

  @spec list() :: {:ok, [map()]}
  def list do
    NativeBridge.list_stacks()
  end

  @spec create(map()) :: {:ok, map()} | {:error, term()}
  def create(attrs) when is_map(attrs) do
    NativeBridge.create_stack(attrs)
  end

  @spec fetch(pos_integer()) :: {:ok, map()} | {:error, :not_found}
  def fetch(id) when is_integer(id) and id > 0 do
    NativeBridge.get_stack(id)
  end

  @spec update(pos_integer(), map()) :: {:ok, map()} | {:error, :not_found}
  def update(id, attrs) when is_integer(id) and id > 0 and is_map(attrs) do
    NativeBridge.update_stack(id, attrs)
  end

  @spec validate(pos_integer()) :: {:ok, map()} | {:error, :not_found}
  def validate(id) when is_integer(id) and id > 0 do
    NativeBridge.validate_stack(id)
  end

  @spec security_scan(pos_integer()) :: {:ok, map()} | {:error, :not_found}
  def security_scan(id) when is_integer(id) and id > 0 do
    NativeBridge.security_scan(id)
  end

  @spec gap_analysis(pos_integer()) :: {:ok, map()} | {:error, :not_found}
  def gap_analysis(id) when is_integer(id) and id > 0 do
    NativeBridge.gap_analysis(id)
  end
end
