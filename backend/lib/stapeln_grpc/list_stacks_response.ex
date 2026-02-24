defmodule StapelnGrpc.ListStacksResponse do
  @moduledoc false
  use Protobuf, syntax: :proto3

  field(:stacks, 1, repeated: true, type: StapelnGrpc.StackPayload)
end
