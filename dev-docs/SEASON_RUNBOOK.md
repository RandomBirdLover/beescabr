# Season runbook

**Who this is for:** whoever runs the bee program each season. It answers "it is
March and I am back at this, what do I actually do." No R knowledge is assumed
beyond being able to run a command. For how the code is *built*, see
`PIPELINE_GUIDE.md`; this file is about *operating* it.

---

## The short version

Three commands, in this order, from the repo root:

```
Rscript scripts/run_data_cleaning_pipeline.R
Rscript scripts/run_all_analysis_pipeline.R
Rscript scripts/run_publishing_materials_pipeline.R
```

The first asks you a question (see below). The other two run on their own. Then
review `docs/`, and commit and push to update the public site.

You do not need to remember any environment variables. Everything the pipeline
needs to ask, it asks.

---

## 1. Before you run anything: the hand-maintained files

The pipeline pulls bees and plants from iNaturalist by itself. It cannot pull
the things only a person knows. Update these first, or the run will happily
produce last season's answer:

| what | file | when it changes |
|---|---|---|
| who surveyed | `data/project_info/rosters/surveyor_roster.csv` | any new intern or beeple |
| who identified | `data/project_info/rosters/identifier_roster.csv` | a new identifier contributes |
| survey dates | `data/project_info/survey_date_sources/master_intern_survey_log.csv` | each intern survey trip |
| trip-level effort | `data/project_info/master_per_survey_info.csv` | each survey trip |
| beeple calendar | `data/project_info/survey_date_sources/beeple_calendar_windows/YYYY Cabrillo Bee Survey Calendar.pdf` | each new season |
| specimens | `data/specimens/records/cabr_bee_specimens_record_V{n}_{YYYY_MM_DD}.xlsx` | after netting or a new determination |

The roster and the survey log are **two separate files that must stay in sync**.
A new person means editing the roster. A new survey date means editing the log.
A new intern means editing both. People are counted from the roster and effort
from the logs, never the other way around.

Specimens are versioned by filename, and the newest date wins. Save a new
version rather than editing the old one in place.

`MANUAL_INPUTS.md` is the full list, including file-naming rules. If you are not
sure whether a file is hand-kept or generated, check its "Do NOT hand-edit"
section before touching it. A generated file will be overwritten on the next run
and your edit will vanish without warning.

---

## 2. Stage 1: clean the data

```
Rscript scripts/run_data_cleaning_pipeline.R
```

It opens with a menu. Pick a number:

| # | mode | what it does | when to use it |
|---|---|---|---|
| **1** | **Normal run** | Bees **and** plants. Pulls only what is new or edited since last time. Seconds. | **Almost always. This is the default.** |
| 2 | Bees only | Normal bee pull, plants skipped. Leaves plant data stale. | Rarely. Only if the plant pull is broken and you need bee numbers now. |
| 3 | Offline run | No iNaturalist calls at all. Reuses what is cached. | On a plane, or when iNaturalist is down. |
| 4 | Full rebuild | Re-downloads everything from scratch. 40+ minutes. | Once a year at most, and **not alone**. See below. |

**Option 4 needs a bee person, not just patience.** A full rebuild re-resolves
every name against iNaturalist's taxonomy *as it stands today*. Names drift:
a bee may have been renamed, split, lumped, or may simply not exist on
iNaturalist under the name our checklists use. The pipeline will ask you to
judge those. That is a taxonomic call, not a clerical one. Sit down with someone
who knows the local bee fauna before choosing 4.

### The prompts you may see

* **A refresh warning.** Reference data older than a year prompts before it is
  reused. Answer honestly. Saying yes to stale IUCN status is not fatal, but it
  means a conservation status on the site is a year out of date.
* **Taxon verification.** New taxa that need a human eye are listed one at a
  time: `y` to verify, `r` to reject for now, Enter to skip, `x` to stop. Skipping
  is safe. It just asks again next run.

---

## 3. Stage 2: run the analyses

```
Rscript scripts/run_all_analysis_pipeline.R
```

This runs every script in `scripts/analysis/` and writes figures and tables into
this season's folder. Nothing to answer.

**Read the last line.** It prints something like `Ran 36 analysis scripts; 0
failed.` If any failed, it names them. A failed script leaves its *previous*
output in place, so the number on the page will be last season's. Fix the
failure, or at minimum know which page is not to be trusted. The publisher will
also stop you (see stage 3), but do not rely on that as your only check.

---

## 4. Stage 3: publish

```
Rscript scripts/run_publishing_materials_pipeline.R
```

This regenerates the public pages and copies them into `docs/`, which is what
GitHub Pages serves. Then:

```
git status          # see what changed
git add -A
git commit -m "Season <year> update"
git push
```

The live site updates a minute or two after the push.

### If publishing stops

Two guards can halt it. Both leave `docs/` untouched, so the live site is never
half-updated:

* **"No published pages were found."** The analysis has not been run for this
  year yet. Run stage 2 first.
* **"These pages are OLDER than the cleaned data."** A page on disk predates the
  data it is built from, which usually means its analysis script failed in stage
  2. It names the pages. Re-run the analysis and check the failure tally.
  If you are certain the pages are fine as they stand, override it:
  `BEESCABR_ALLOW_STALE=1 Rscript scripts/run_publishing_materials_pipeline.R`

---

## 5. Where this season's work lands

Outputs go to a per-year folder, taken from today's date:

```
data/analysis/nps_report_2026/
```

So 2027's run writes `nps_report_2027/` and leaves 2026 alone. Nothing is
overwritten across seasons.

To rebuild an earlier season, set the year explicitly:

```
BEESCABR_SEASON_YEAR=2026 Rscript scripts/run_all_analysis_pipeline.R
```

Note that `data/` is **not** in git (it holds precise coordinates for at-risk
bees inside the park). These files live on your machine. To hand them to someone,
see `DATA_ACCESS.md`. The public site in `docs/` carries only aggregated,
non-sensitive output, and that *is* in git.

---

## 6. Checking your work

```
Rscript -e 'library(testthat); test_dir("tests/testthat")'
```

Expect `FAIL 0`. A few warnings are normal and expected. The tests do not touch
the real data or the network, so they are always safe to run. If something fails
after you have changed a file, the test is usually right: these encode data
quirks that were hard to find. Understand why it fails before changing it.

---

## 7. Once a year

* **Full rebuild (menu option 4)** with a bee person present, to pick up
  taxonomic changes.
* **Reference data refresh.** The pipeline prompts when a cache is over a year
  old. `BEESCABR_REFRESH=1` on the cleaning pipeline forces a re-check early.
* **Re-read `LIMITATIONS.md`.** It records what this data can and cannot support.
  The trend analysis in particular is not yet strong enough to claim a population
  trend, and that section explains exactly why.

---

## 8. When something breaks

1. Read the error. These scripts try to say what is wrong in plain words.
2. Re-run with menu option **3 (offline)**. If it works offline, the problem is
   the API or the network, not your data.
3. Run the tests. They isolate a code problem from a data problem.
4. `git status` and `git diff` to see whether something local changed.
5. The pipeline never writes to the live site on its own. A failed run cannot
   break the public pages. You can always try again.
