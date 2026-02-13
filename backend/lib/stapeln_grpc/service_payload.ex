defmodule StapelnGrpc.ServicePayload do
  @moduledoc false
  use Protobuf, syntax: :proto3

  field :name, 1, type: :string
  field :kind, 2, type: :string
  field :port, 3, type: :int32
end

