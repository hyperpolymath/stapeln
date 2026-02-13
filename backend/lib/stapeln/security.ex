defmodule Stapeln.Security do
  @moduledoc """
  Entry point for security-specific controllers and helpers.
  """

  alias Stapeln.Security.PanicAttacker

  def delegate_trace(command, target, schedule) do
    PanicAttacker.start_trace(command, target, schedule)
  end

  def stop_trace do
    PanicAttacker.stop_trace()
  end

  def trace_status do
    PanicAttacker.status()
  end
end
