defmodule StapelnGrpc.FindingPayload do
  @moduledoc false
  use Protobuf, syntax: :proto3

  field :id, 1, type: :string
  field :severity, 2, type: :string
  field :message, 3, type: :string
  field :hint, 4, type: :string
end

