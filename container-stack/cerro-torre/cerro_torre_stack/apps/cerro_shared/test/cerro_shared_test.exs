defmodule CerroSharedTest do
  use ExUnit.Case
  doctest CerroShared

  test "greets the world" do
    assert CerroShared.hello() == :world
  end
end
