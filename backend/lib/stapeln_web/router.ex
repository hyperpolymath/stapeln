defmodule StapelnWeb.Router do
  use StapelnWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", StapelnWeb do
    pipe_through :api

    get "/healthz", HealthController, :show
    get "/stacks", StackController, :index
    post "/stacks", StackController, :create
    get "/stacks/:id", StackController, :show
    put "/stacks/:id", StackController, :update
    post "/stacks/:id/validate", StackController, :validate
  end

  scope "/api" do
    pipe_through :api
    forward "/graphql", Absinthe.Plug, schema: StapelnWeb.Schema
  end
end
