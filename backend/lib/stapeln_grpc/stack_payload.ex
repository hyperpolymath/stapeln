defmodule StapelnGrpc.StackPayload do
  @moduledoc false
  use Protobuf, syntax: :proto3

  field(:id, 1, type: :string)
  field(:name, 2, type: :string)
  field(:description, 3, type: :string)
  field(:created_at, 4, type: :string, json_name: "createdAt")
  field(:updated_at, 5, type: :string, json_name: "updatedAt")
  field(:services, 6, repeated: true, type: StapelnGrpc.ServicePayload)
end
