# SPDX-License-Identifier: PMPL-1.0-or-later
# StackChannel - Real-time validation, security scanning, and gap analysis
#
# Clients join "stack:lobby" for general broadcasts or "stack:<id>" for
# stack-specific updates.  Three inbound events are supported:
#
#   "validate"      -> runs ValidationEngine, pushes "validation_result"
#   "security_scan" -> runs SecurityScanner,   pushes "security_result"
#   "gap_analysis"  -> runs GapAnalyzer,       pushes "gap_result"
#
# Each handler pushes results back to the caller only (not broadcast),
# keeping the interaction request/response-like while still benefiting
# from the persistent WebSocket connection.

defmodule StapelnWeb.StackChannel do
  use StapelnWeb, :channel

  @impl true
  def join("stack:lobby", _payload, socket) do
    {:ok, socket}
  end

  def join("stack:" <> _id, _payload, socket) do
    {:ok, socket}
  end

  # ---- Inbound events -------------------------------------------------------

  @impl true
  def handle_in("validate", %{"stack" => stack_data}, socket) do
    report = Stapeln.ValidationEngine.validate(stack_data)
    push(socket, "validation_result", %{data: report})
    {:noreply, socket}
  end

  @impl true
  def handle_in("security_scan", %{"stack" => stack_data}, socket) do
    report = Stapeln.SecurityScanner.scan(stack_data)
    push(socket, "security_result", %{data: report})
    {:noreply, socket}
  end

  @impl true
  def handle_in("gap_analysis", %{"stack" => stack_data}, socket) do
    report = Stapeln.GapAnalyzer.analyze(stack_data)
    push(socket, "gap_result", %{data: report})
    {:noreply, socket}
  end
end
