defmodule StapelnGrpc.Endpoint do
  @moduledoc false
  use GRPC.Endpoint

  run(StapelnGrpc.StackService.Server)
end
