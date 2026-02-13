defmodule StapelnGrpc.CreateStackRequest do
  @moduledoc false
  use Protobuf, syntax: :proto3

  field :name, 1, type: :string
  field :description, 2, type: :string
  field :services, 3, repeated: true, type: StapelnGrpc.ServicePayload
end

