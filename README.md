# beescabr

Native bee biodiversity pipeline for Cabrillo National Monument (CABR), comparing lethal (museum specimen) and non-lethal (iNaturalist photo) survey methods.

## 🐝 Live site

Browse the public field guides, checklists, and interactive maps online — no code or downloads needed:

**https://randombirdlover.github.io/beescabr/**

Includes the bee field guides (species + genus), park summary tables, least-sampled bees, the survey-transect and bee-bounty maps, and an interactive **Bee Occurrence Explorer** (filter every record by genus, species, transect, and method).

**Publishing is its own pipeline stage** — `scripts/run_publishing_materials_pipeline.R` — alongside data cleaning and analysis. The pipeline runs in three stages, each with its own runner:

1. `scripts/run_data_cleaning_pipeline.R` — ingest iNaturalist → DuckDB cache → cleaned tables
2. `scripts/run_all_analysis_pipeline.R` — every figure, table, and report/journal HTML
3. `scripts/run_publishing_materials_pipeline.R` — re-render the **public** pages and sync them into `docs/` (which GitHub Pages serves)

```
source("scripts/run_publishing_materials_pipeline.R")                     # rebuild the public site into docs/ (then git push to deploy)
Sys.setenv(BEESCABR_DEPLOY = "1"); source("scripts/run_publishing_materials_pipeline.R")   # also commit + push docs/ (auto-deploy)
```

`docs/` is committed to git; GitHub Pages redeploys on every push. The publish modules live in `scripts/website/` (the primitive alone is `source("scripts/website/publish_pages.R")`), the way cleaning lives in `scripts/inat_observations/` and analysis in `scripts/analysis/`.

---

## Running a season

**Who this is for:** whoever runs the bee program each season. It answers "it is
March and I am back at this, what do I actually do." No R knowledge is assumed
beyond being able to run a command in RStudio.

Open `beescabr.Rproj` in RStudio first -- that sets the working directory to the
repository root, which every path in the project assumes.

---

## The short version

Three commands, in this order, from the repo root:

```
source("scripts/run_data_cleaning_pipeline.R")
source("scripts/run_all_analysis_pipeline.R")
source("scripts/run_publishing_materials_pipeline.R")
```

The first asks you a question (see below). The other two run on their own. Then
review `docs/`, and commit and push to update the public site.

You do not need to remember any environment variables. Everything the pipeline
needs to ask, it asks.

---

## 0. First time on this machine: install the packages

```
source("scripts/utils/install_requirements.R")
```

Installs everything the pipeline needs and names anything it could not install. A few
packages (`sf`, `pdftools`) need system libraries; on macOS
`brew install gdal proj geos poppler` covers them. Run it once per machine, and again after
an R upgrade.

---

## 1. Before you run anything: the hand-maintained files

The pipeline pulls bees and plants from iNaturalist by itself. It cannot pull
the things only a person knows. Update these first, or the run will happily
produce last season's answer:

| what | file | when it changes |
|---|---|---|
| everyone: who surveyed, identified, researched | `data/project_info/rosters/people_manual.csv` | anyone new, BEFORE their first season |
| survey dates | `data/project_info/surveys/survey_date_sources/master_intern_survey_log_manual.csv` | each intern survey trip |
| beeple calendar | `data/project_info/surveys/survey_date_sources/beeple_calendar_windows/YYYY Cabrillo Bee Survey Calendar.pdf` | each new season |
| specimens | `data/specimens/records/cabr_bee_specimens_record_V{n}_{YYYY_MM_DD}.xlsx` | after netting or a new determination |

A person is typed in ONCE, in `people_manual.csv`, whoever they are. The intern log
refers to them by `person_id`, never by name. A returning beeple needs no edit at
all: their tagged observations become that season's surveys on their own.

Anything ending `_generated` is written by the pipeline. Never edit it.
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

## 1b. Your own API credentials (first run on a new machine)

Two services need a key. **They are personal: never use someone else's, and never copy
another person's `data/secrets/` folder.** A pull runs as whoever's account signed in, and
that account's trust level decides what data comes back.

| service | what it unlocks | needed? |
|---|---|---|
| iNaturalist | TRUE coordinates for SENSITIVE taxa (*Bombus crotchii* and the like), for observers who trust this account | Optional. Without it the pull still works, but those coordinates come back OBSCURED. |
| IUCN Red List | conservation status on the field guides | Optional. Without it the status column is blank. |

The pipeline **asks** for anything missing on its first run and offers to save it to
`data/secrets/` (gitignored, and written readable only by you). Nothing is stored in a
script, and typing is hidden if the `askpass` package is installed.

**Setting up iNaturalist, once:**

1. Sign in to iNaturalist **as the account observers already trust** (the park's own
   account). On iNaturalist, coordinate trust is granted by each observer to a specific
   account. It is not a project setting and it does NOT transfer. Our surveyors trusted
   `@randombirdlover` and `@cabrillonationalmonument`, so a brand-new account would get
   obscured coordinates until each observer trusted it too.
2. Go to <https://www.inaturalist.org/oauth/applications/new>
3. Name it something identifiable, e.g. `officialbeescabr`.
4. Set the callback URL to `http://localhost:3000/beescabr`
5. Copy the Client ID and Client Secret; the pipeline will ask for them.

The first run opens a browser once so you can click Authorize. Later runs are silent.
**Every run prints which account it is signed in as** — check that line. If it names
somebody else, you are pulling on their credentials and should replace them.

The IUCN key is free from <https://api.iucnredlist.org>.

---

## 2. Stage 1: clean the data

```
source("scripts/run_data_cleaning_pipeline.R")
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
source("scripts/run_all_analysis_pipeline.R")
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
source("scripts/run_publishing_materials_pipeline.R")
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
  `Sys.setenv(BEESCABR_ALLOW_STALE = "1"); source("scripts/run_publishing_materials_pipeline.R")`

---

## 5. Where this season's work lands

Outputs go to a per-year folder, taken from today's date:

```
data/analysis/2026_generated/
```

So 2027's run writes `2027_generated/` and leaves 2026 alone. Nothing is
overwritten across seasons.

To rebuild an earlier season, set the year explicitly:

```
Sys.setenv(BEESCABR_SEASON_YEAR = "2026"); source("scripts/run_all_analysis_pipeline.R")
```

Note that `data/` is **not** in git (it holds precise coordinates for at-risk
bees inside the park). These files live on your machine. To hand them to someone,
see `DATA_ACCESS.md`. The public site in `docs/` carries only aggregated,
non-sensitive output, and that *is* in git.

---

## 6. Checking your work

```
library(testthat); test_dir("tests/testthat")
```

Expect `FAIL 0`. A few warnings are normal and expected. The tests do not touch
the real data or the network, so they are always safe to run. If something fails
after you have changed a file, the test is usually right: these encode data
quirks that were hard to find. Understand why it fails before changing it.

---

## 7. Starting a new season

At the top of a season, before the first run, update the things only a person
knows. Nothing here is automatic -- the pipeline will otherwise produce last
season's answer without complaining.

**The sheets you update by hand** (section 1 has the full table):

* **The beeple calendar** -- drop this year's PDF into
  `data/project_info/surveys/survey_date_sources/beeple_calendar_windows/`,
  named `YYYY Cabrillo Bee Survey Calendar.pdf`. This is the one that is easy to
  forget, and without it the season has no survey windows.
* **Anyone new** -- add them to `people_manual.csv` BEFORE their first survey, so
  their records attach to a person rather than a bare name.
* **The intern survey log** -- new trips, as they happen.
* **Specimens** -- a new versioned `.xlsx` after netting or a new determination.

**Then, once a year:**

* **Full rebuild (menu option 4)** with a bee person present, to pick up
  taxonomic changes.
* **Reference data refresh.** The pipeline prompts when a cache is over a year
  old. `Sys.setenv(BEESCABR_REFRESH = "1")` on the cleaning pipeline forces a
  re-check early.
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


---

## People

| Name | Role |
|------|------|
| James Hung | Principal Investigator |
| Patricia Simpson | Project lead; photographer overseer |
| Taro Katayama | NPS CABR biologist; R supervisor |
| Ashley Kim | Intern overseer |
| Jess Mullins | Museum specimen identification |
| Brandi Sanchez | Scientists in Parks intern; pipeline development |
| Shahan Derkarabetian | SDNHM Curator of Invertebrate Zoology; specimen deposit contact |
| Goran Bozinovic | Specialist (*Dufourea* genetic sampling) |

---


## Repository structure

```
beescabr/
  scripts/
    run_data_cleaning_pipeline.R        # STAGE 1: ingest + clean + checklists (interactive)
    run_all_analysis_pipeline.R         # STAGE 2: every analysis in scripts/analysis/
    run_publishing_materials_pipeline.R # STAGE 3: rebuild the public pages and copy to docs/
    config.R                            # PATHS, ids, CRS, API versions, season folder
    inat_observations/                  # iNaturalist ingest + cleaning (engine/ holds the API + DuckDB layers)
    specimens/                          # specimen workbook cleaning + QC
    project_info/                       # the "brain": who surveyed, when, and the tag/field crosswalk
    checklists/                         # tiered species checklists (CABR / Point Loma / SD County)
    reference/                          # taxonomy + plant lookups, id resolution, verification prompts
    analysis/                           # every figure and table (theme_beescabr.R holds ALL colours)
    publish/                            # builds docs/ for GitHub Pages
    spatial/                            # boundary + transect geometry helpers
    utils/                              # run-mode menu, refresh checks, per-script warning capture
  tests/testthat/                       # unit tests; never touch the real data or the network
  dev-docs/                             # developer documentation (see the map below)
  docs/                                 # the PUBLIC SITE, served by GitHub Pages
  data/                                 # gitignored -- NOT on GitHub (see .gitignore and DATA_ACCESS.md)
    inat_observations/                  # cache/ (DuckDB), inat_clean/, review/ QC worklists
    specimens/                          # records/ (versioned workbooks), specimens_clean/
    project_info/
      rosters/                          # WHO: surveyor + identifier + research-team rosters
      surveys/                          # WHEN and WHERE: effort log, transects, date sources, review/
      crosswalk/                        # the shared tag/field dictionary, and its review/
    checklists/                         # generated tier checklists + IUCN status cache
    reference/                          # curated/ (hand-edited), generated/, source/ taxonomy lookups
    spatial/                            # boundary + transect shapefiles, basemap cache
    analysis/                           # figures and tables, in a per-season folder (nps_report_YYYY)
  CLAUDE.md                             # rules for coding agents
  README.md
```

---

## Data rules

- **GBIF = regional reference context only.** GBIF records are never used as CABR survey records. CABR specimens are being deposited to SDNHM (via Pam Horsley) and are not yet in GBIF.
- **`data/` is gitignored** — no specimen, iNat, or personal data is on GitHub. Exception: shapefiles under `data/spatial/shapefiles/`.
- **`data/analysis/` is generated.** Never edit output CSVs by hand — re-run the relevant script instead.
- **Buffers are generated in R**, not stored as shapefiles. Do not commit `Buffer_10m.*` or any derived shapefile.

---

---

## Documentation map

Every document in this repository, in the order a newcomer should read them.
All of them live in `dev-docs/` unless noted.

| Read | Document | What it answers |
| --- | --- | --- |
| 1 | **README.md** (this file) | What is this, how do I set it up, what do I run? |
| 2 | **DATA_ACCESS.md** | How do I get the data? It is not in this repo. Written for non-programmers. |
| 4 | **WEBSITE_GUIDE.md** | The public site: how a page is built and published, the layout every page follows, and why the maps are built two different ways. |
| 5 | **MANUAL_INPUTS.md** | Which files must a human maintain by hand, and when? Read before any season. |
| 6 | **PIPELINE_GUIDE.md** | How is the code laid out, how does data flow, how do I extend it? |
| 7 | **LIMITATIONS.md** | What this pipeline knowingly trades away, and which data problems only a protocol change can fix. |
| 8 | *(generated)* | What every analysis output is: `data/analysis/<year>_generated/findings_index.csv`, one row per analysis, plus a note in each folder. |
| 9 | **ANALYSIS_DECISIONS.md** | The *why* behind each analysis: scope, parameters, statistical tests. |
| 10 | **ARCGIS_SPATIAL_MAPPING.md** | Where every boundary and transect layer came from, and the known coastal discrepancy. |
| 11 | **ANALYSIS_ROADMAP.md** | Stakeholder questions, triaged into a buildable plan. |
| 12 | **VERIFICATION.md** | What the verification workflow is, and how to answer the prompt. |
| 13 | **SPECIMEN_CHANGELOG.md** | Version history of the specimen spreadsheet, and how to bump it. |
| 14 | **SCRIPTS_GUIDE.md** · **TESTS_GUIDE.md** | Notes on the script layout and the test suite. |
| 15 | **TODO.md** | Open work, by area. |
| 16 | **HANDOFF_CHECKLIST.md** | What to hand over, and what the next person needs on day one. |

Agent instructions live in **CLAUDE.md** (rules) and **AGENTS.md** (a pointer to it).

*There is exactly one `README.md` in this repository — this one. Every other
document has a distinct name so nothing is ambiguous in search results or tabs.*
