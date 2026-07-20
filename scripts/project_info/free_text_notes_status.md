# Free-text observation notes — optional (opt-in each run)

The brain (`finding_project_info.R`) flags un-triaged free-text observation notes to
`data/project_info/review/review_inat_unknown_notes.csv`. Reviewing/using them is
**optional**. On an interactive run, `run_pipeline.R` **stage 3b** asks:

> "Review the observation notes now, or proceed without them? (y = review / N = skip)"

Answer `y` and it sources `review_notes.R` (here in `scripts/project_info/`) and runs the
reviewer; anything else proceeds without notes (the reviewer isn't even sourced).
Non-interactive / scheduled runs always skip it.
