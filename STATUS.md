# stapeln Status (Source of Truth)

**Date:** 2026-02-09

## Product Goal

A reasonably IT-capable 12-year-old can help their parents build a secure container stack without prior container knowledge.

## What Works Today

- **UI Prototype (frontend):** 8 views implemented and navigable.
- **Views:** Network.
- **Views:** Stack.
- **Views:** Lago Grey.
- **Views:** Ports.
- **Views:** Security.
- **Views:** Gaps.
- **Views:** Simulation.
- **Views:** Settings.
- **TEA Architecture:** State, Msg, Update, View pattern in place.
- **Security UX Components:** Port config, security inspector, gap analysis, simulation mode.
- **Import/Export Hooks:** Buttons exist (not fully wired to durable storage).

## Preserved Future Work

- **DOM‑mounter track:** Extracted to `/var/mnt/eclipse/repos/stapeln-dom-mounter`.
- This work is not on the critical path for the container‑hater MVP.

## What Is Not Implemented Yet

- **Backend:** No Phoenix/GraphQL implementation; `backend/lib` is empty.
- **Validation Engine:** No real engine in app; rules exist only in `tests/stapeln.test.js`.
- **Persistence:** No durable save/load outside ad-hoc import/export.
- **Formal Verification Layers:** Idris2/miniKanren are planned, not wired.

## Known Inconsistencies

- Some docs claim a “complete” product; these refer to an internal DOM‑mounter workstream.
- `IMPLEMENTATION-PLAN.md` references “stackur” (legacy).
- `ROADMAP.adoc` is a template and not authoritative.

## Immediate Focus (Next 4 Weeks)

- Truth-align docs and roadmap.
- Backend-first MVP (minimal persistence + validation API), with local fallback if needed.
- End-user onboarding flow focused on “container haters.”
