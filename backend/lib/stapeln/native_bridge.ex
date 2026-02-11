defmodule Stapeln.NativeBridge do
  @moduledoc """
  Single API boundary for ABI/FFI interaction.

  Contract source of truth:
  - Idris2 ABI definitions: `src/abi/Types.idr`, `src/abi/Foreign.idr`
  - Zig FFI implementation: `ffi/zig/src/main.zig`

  If `STAPELN_NATIVE_FFI_BIN` points to an executable, this module calls it.
  Otherwise it transparently falls back to pure Elixir runtime modules.
  """

  alias Stapeln.StackStore
  alias Stapeln.ValidationEngine

  @type op ::
          :list_stacks | :create_stack | :get_stack | :update_stack | :validate_stack

  @spec backend() :: :zig_cli | :elixir
  def backend do
    case native_bin() do
      nil -> :elixir
      _ -> :zig_cli
    end
  end

  @spec list_stacks() :: {:ok, [map()]}
  def list_stacks do
    dispatch(:list_stacks, %{})
  end

  @spec create_stack(map()) :: {:ok, map()} | {:error, term()}
  def create_stack(attrs) when is_map(attrs) do
    dispatch(:create_stack, attrs)
  end

  @spec get_stack(pos_integer()) :: {:ok, map()} | {:error, :not_found}
  def get_stack(id) when is_integer(id) and id > 0 do
    dispatch(:get_stack, %{id: id})
  end

  @spec update_stack(pos_integer(), map()) :: {:ok, map()} | {:error, :not_found}
  def update_stack(id, attrs) when is_integer(id) and id > 0 and is_map(attrs) do
    dispatch(:update_stack, %{id: id, attrs: attrs})
  end

  @spec validate_stack(pos_integer()) :: {:ok, map()} | {:error, :not_found}
  def validate_stack(id) when is_integer(id) and id > 0 do
    dispatch(:validate_stack, %{id: id})
  end

  defp dispatch(op, payload) do
    case call_native(op, payload) do
      {:ok, _value} = ok -> ok
      _ -> call_fallback(op, payload)
    end
  end

  defp call_native(op, payload) do
    with bin when is_binary(bin) <- native_bin(),
         input <- Jason.encode!(payload),
         {output, 0} <- System.cmd(bin, [Atom.to_string(op), input], stderr_to_stdout: true),
         {:ok, decoded} <- Jason.decode(output) do
      decode_native(decoded)
    else
      _ -> {:error, :native_unavailable}
    end
  end

  defp decode_native(%{"ok" => value}), do: {:ok, value}
  defp decode_native(%{"error" => reason}) when is_binary(reason), do: {:error, reason}
  defp decode_native(_), do: {:error, :native_protocol_error}

  defp native_bin do
    case System.get_env("STAPELN_NATIVE_FFI_BIN") do
      nil ->
        nil

      path ->
        System.find_executable(path)
    end
  end

  defp call_fallback(:list_stacks, _payload) do
    {:ok, StackStore.list()}
  end

  defp call_fallback(:create_stack, attrs) do
    StackStore.create(attrs)
  end

  defp call_fallback(:get_stack, %{id: id}) do
    StackStore.get(id)
  end

  defp call_fallback(:update_stack, %{id: id, attrs: attrs}) do
    StackStore.update(id, attrs)
  end

  defp call_fallback(:validate_stack, %{id: id}) do
    with {:ok, stack} <- StackStore.get(id) do
      report = ValidationEngine.validate(stack)
      {:ok, Map.put(report, :stack, stack)}
    end
  end
end
