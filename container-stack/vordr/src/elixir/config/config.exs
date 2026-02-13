# SPDX-License-Identifier: PMPL-1.0-or-later

import Config

config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:container_id]

# Import environment specific config
import_config "#{config_env()}.exs"
