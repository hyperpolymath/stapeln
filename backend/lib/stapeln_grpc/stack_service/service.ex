defmodule StapelnGrpc.StackService.Service do
  @moduledoc false
  use GRPC.Service, name: "stapeln.StackService"

  rpc(:ListStacks, StapelnGrpc.ListStacksRequest, StapelnGrpc.ListStacksResponse)
  rpc(:CreateStack, StapelnGrpc.CreateStackRequest, StapelnGrpc.StackResponse)
  rpc(:GetStack, StapelnGrpc.GetStackRequest, StapelnGrpc.StackResponse)
  rpc(:UpdateStack, StapelnGrpc.UpdateStackRequest, StapelnGrpc.StackResponse)
  rpc(:ValidateStack, StapelnGrpc.ValidateStackRequest, StapelnGrpc.ValidateStackResponse)
end
