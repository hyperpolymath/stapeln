# stapeln Execution Plan (2026-02-11)

Scope selected: `1 2 3 4 5 6`

## Objective

Deliver a backend-first MVP while keeping the frontend usable and making documentation truthful and operational.

## Workstream 1: Backend MVP (Persistence + API)

Owner: Backend
Priority: P0
Duration: Week 1-2

Tasks:
- Create minimal Phoenix app skeleton under `backend/lib` and `backend/test`.
- Add persistence for stack drafts (initially local SQLite or Postgres; pick one and document).
- Expose minimal API:
  - `POST /api/stacks`
  - `GET /api/stacks/:id`
  - `PUT /api/stacks/:id`
- Add health endpoint: `GET /healthz`.
- Add one integration test per endpoint.

Definition of done:
- API endpoints pass integration tests.
- Data survives process restart.
- `./scripts/readiness-check.sh` backend gate remains green.

## Workstream 2: Validation Engine Wired In-App

Owner: Backend + Frontend
Priority: P0
Duration: Week 1-2

Tasks:
- Extract existing rule logic from `tests/stapeln.test.js` into reusable module(s).
- Add server-side validation endpoint: `POST /api/stacks/:id/validate`.
- Return normalized validation payload (severity, category, message, fix hint).
- Wire frontend Security/Gaps views to consume validation payload instead of static/demo-only data.
- Add regression tests for at least 10 validation rules.

Definition of done:
- UI displays live validation results from API.
- Validation endpoint has deterministic test fixtures.
- No hard-coded validation placeholders in main flow.

## Workstream 3: Frontend Open Items (Week 2/4 Gaps)

Owner: Frontend
Priority: P1
Duration: Week 2

Tasks:
- Implement Attack Surface Analyzer panel sections currently marked incomplete.
- Finish router/dev-server polish:
  - URL-state navigation consistency.
  - documented local dev command with hot reload behavior.
- Add accessibility verification checklist run (keyboard + screen reader smoke test).

Definition of done:
- `ROADMAP.md` Week 2/4 items can be marked complete where implemented.
- Attack surface panel has real data path (from validation payload, not constants).
- Accessibility smoke test report committed in `docs/`.

## Workstream 4: Simulation Polish

Owner: Frontend
Priority: P1
Duration: Week 2

Tasks:
- Add explicit simulation controls requested by roadmap:
  - Reset button.
  - Step-through mode.
  - Extended speed presets if retained.
- Add richer simulation event log with success/failure semantics.
- Add pre-deployment dry-run summary panel.
- Keep packet-cap and batch-kernel behavior covered by deterministic benchmarks/tests.

Definition of done:
- Simulation controls match roadmap requirements.
- Dry-run panel visible and populated.
- Existing packet-cap test scenario still passes.

## Workstream 5: Docs Truth-Alignment

Owner: Maintainer
Priority: P0
Duration: Immediate

Tasks:
- Keep `STATUS.md` as source-of-truth and update date/status regularly.
- Mark `ROADMAP.adoc` as alias/deprecated pointer to `ROADMAP.md`.
- Remove legacy naming confusion (`stackur`) from active planning docs.
- Ensure all "complete" claims map to implemented code paths.

Definition of done:
- No template/legacy roadmap claims left ambiguous.
- New contributors can identify current truth docs in under 60 seconds.

## Workstream 6: Repo Hygiene / Readiness Green

Owner: Maintainer
Priority: P0
Duration: Immediate + ongoing

Tasks:
- Keep tracked and untracked workspace clean before readiness runs.
- Resolve or commit intentional edits (example current blocker: `container-stack/rokur/README.md`).
- Run `./scripts/readiness-check.sh` before release merges.
- Keep generated ReScript build artifacts out of commits unless required.

Definition of done:
- Readiness check passes all gates, including `Repo clean`.
- No accidental generated-file churn in review diffs.

## Sequence (Execution Order)

1. Workstream 5 (truth-align docs)
2. Workstream 6 (clean gate discipline)
3. Workstream 1 (backend MVP skeleton + persistence)
4. Workstream 2 (validation endpoint + UI wiring)
5. Workstream 4 (simulation polish)
6. Workstream 3 (remaining frontend polish/accessibility)

## Progress Tracking

Use this checklist for the next two weeks:

- [ ] Workstream 1 complete
- [ ] Workstream 2 complete
- [ ] Workstream 3 complete
- [ ] Workstream 4 complete
- [ ] Workstream 5 complete
- [ ] Workstream 6 complete
