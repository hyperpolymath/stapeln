defmodule StapelnWeb.Router do
  use StapelnWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  pipeline :api_authenticated do
    plug StapelnWeb.Plugs.RequireApiToken
  end

  scope "/api", StapelnWeb do
    pipe_through :api

    get "/healthz", HealthController, :show
    post "/auth/register", AuthController, :register
    post "/auth/login", AuthController, :login
  end

  scope "/api", StapelnWeb do
    pipe_through [:api, :api_authenticated]

    get "/stacks", StackController, :index
    post "/stacks", StackController, :create
    get "/stacks/:id", StackController, :show
    put "/stacks/:id", StackController, :update
    post "/stacks/:id/validate", StackController, :validate
    post "/stacks/:id/security-scan", StackController, :security_scan
    post "/stacks/:id/gap-analysis", StackController, :gap_analysis
    post "/stacks/:id/generate", StackController, :generate
    post "/stacks/:id/sign", StackController, :sign_stack
    get "/stacks/:id/verify", StackController, :verify_stack
    get "/audit", AuditController, :index
    get "/auth/me", AuthController, :me
    get "/settings", SettingsController, :show
    put "/settings", SettingsController, :update

    # Ephemeral pinhole firewall management
    post "/firewall/pinholes", FirewallController, :create
    get "/firewall/pinholes", FirewallController, :index
    delete "/firewall/pinholes/:id", FirewallController, :delete
    post "/firewall/check", FirewallController, :check

    post "/security/panic-attacker", SecurityController, :start
    post "/security/panic-attacker/stop", SecurityController, :stop
    get "/security/panic-attacker/status", SecurityController, :status
  end

  scope "/api" do
    pipe_through [:api, :api_authenticated]
    forward "/graphql", Absinthe.Plug, schema: StapelnWeb.Schema
  end
end
