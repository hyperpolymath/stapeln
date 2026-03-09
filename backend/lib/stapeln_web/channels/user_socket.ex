# SPDX-License-Identifier: PMPL-1.0-or-later
# UserSocket - Phoenix WebSocket entry point for real-time validation
#
# Clients connect via `ws://<host>/socket/websocket` and can then join
# channels like "stack:lobby" or "stack:<id>" for scoped updates.

defmodule StapelnWeb.UserSocket do
  use Phoenix.Socket

  channel "stack:*", StapelnWeb.StackChannel

  @impl true
  def connect(_params, socket, _connect_info) do
    {:ok, socket}
  end

  @impl true
  def id(_socket), do: nil
end
