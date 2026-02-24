import Config

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :stapeln, StapelnWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "09oxWpFg/+JTtpvubG7IZv5NdfROklSAyHoqBUqY2akbAZId6Zf83GVifWDbML5o",
  server: false

# Print only warnings and errors during test
config :logger, level: :warning

# Initialize plugs at runtime for faster test compilation
config :phoenix, :plug_init_mode, :runtime

# Sort query params output of verified routes for robust url comparisons
config :phoenix,
  sort_verified_routes_query_params: true

config :stapeln, :api_auth,
  enabled: true,
  token: "test-stapeln-token"
