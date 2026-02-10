defmodule StapelnWeb.Router do
  use StapelnWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", StapelnWeb do
    pipe_through :api
  end
end
