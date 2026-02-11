# stapeln Readiness Checklist

Date: 2026-02-11

This checklist defines hard pass/fail gates for local repo health.

## Run all gates

```bash
./scripts/readiness-check.sh
```

## Gates

1. Repo clean
Pass condition:
- No unstaged or staged tracked changes.
- No untracked files.

2. ReScript lockfiles untracked
Pass condition:
- No `rescript.lock` file is tracked in git.

3. Root tests (Deno)
Pass condition:
- `deno test --allow-read --allow-write tests/stapeln.test.js` exits 0.

4. Frontend compile
Pass condition:
- `timeout 120s rescript build` in `frontend/` exits 0.

5. Backend tests
Pass condition:
- `mix deps.get` and `mix test` in `backend/` exit 0.

## Current status notes

- Root tests are now offline-safe (local assertions under `tests/test_assert.js`).
- ReScript lockfiles are ignored via `.gitignore` and removed from tracking.
- `container-stack/` and `selur-compose/` are treated as source docs and should be committed.
