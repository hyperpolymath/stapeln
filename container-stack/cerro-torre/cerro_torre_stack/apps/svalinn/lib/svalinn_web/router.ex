defmodule SvalinnWeb.Router do
  use SvalinnWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
    plug CORSPlug
  end

  # Health and readiness endpoints (no auth required)
  scope "/", SvalinnWeb do
    get "/health", HealthController, :health
    get "/ready", HealthController, :ready
    get "/adapter", HealthController, :adapter_info
  end

  # API v1 endpoints
  scope "/api/v1", SvalinnWeb do
    pipe_through :api

    # Container operations
    resources "/containers", ContainerController, only: [:index, :show, :create, :delete]

    # Image verification
    post "/verify", VerificationController, :verify

    # Image listing (stub for now)
    # get "/images", ImageController, :index
  end

  # Enable LiveDashboard and Swoosh mailbox preview in development
  if Application.compile_env(:svalinn, :dev_routes) do
    # If you want to use the LiveDashboard in production, you should put
    # it behind authentication and allow only admins to access it.
    # If your application does not have an admins-only section yet,
    # you can use Plug.BasicAuth to set up some basic authentication
    # as long as you are also using SSL (which you should anyway).
    import Phoenix.LiveDashboard.Router

    scope "/dev" do
      pipe_through [:fetch_session, :protect_from_forgery]

      live_dashboard "/dashboard", metrics: SvalinnWeb.Telemetry
      forward "/mailbox", Plug.Swoosh.MailboxPreview
    end
  end
end
