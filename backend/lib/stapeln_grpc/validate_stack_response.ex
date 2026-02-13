defmodule StapelnGrpc.ValidateStackResponse do
  @moduledoc false
  use Protobuf, syntax: :proto3

  field :score, 1, type: :int32
  field :findings, 2, repeated: true, type: StapelnGrpc.FindingPayload
  field :stack, 3, type: StapelnGrpc.StackPayload
end

