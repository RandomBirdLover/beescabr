# AGENTS.md

Agent instructions for this repository live in **`CLAUDE.md`** — read it first.

Non-negotiable summary: this project is **test-first (red/green)**. For any new
function or behavior change, write the `testthat` test first, run it and confirm
it FAILS, then implement, then loop red → green until it passes, and finally run
the whole suite (`Rscript -e 'library(testthat); test_dir("tests/testthat")'`)
to confirm no regressions. Test pure logic directly; inject fakes for the API
(`request_fn` / `request_text_fn`) and use a temp DuckDB for DB code. Full conventions and layer map:
`CLAUDE.md`, `dev-docs/PIPELINE_GUIDE.md`.
