defmodule StapelnGrpc.ValidateStackRequest do
  @moduledoc false
  use Protobuf, syntax: :proto3

  field :id, 1, type: :string
end

