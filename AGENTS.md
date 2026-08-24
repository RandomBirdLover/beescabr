# AGENTS.md

Agent instructions for this repository live in **`CLAUDE.md`** — read it first.

Non-negotiable summary: this project is **test-first (red/green)**. For any new
function or behavior change, write the test first, run it and confirm it FAILS,
then implement, then loop red → green until it passes. Note that the `tests/`
suite is **local-only and not part of this repo** (gitignored since 2026-08-24),
so a fresh clone will not have it; if it is absent, say so plainly rather than
skipping the check silently. Test pure logic directly; inject fakes for the API
(`request_fn` / `request_text_fn`) and use a temp DuckDB for DB code. Full conventions and layer map:
`CLAUDE.md`, `dev-docs/PIPELINE_GUIDE.md`.
