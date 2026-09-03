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
Rscript scripts/run_publishing_materials_pipeline.R                     # rebuild the public site into docs/ (then git push to deploy)
BEESCABR_DEPLOY=1 Rscript scripts/run_publishing_materials_pipeline.R   # also commit + push docs/ (auto-deploy)
```

`docs/` is committed to git; GitHub Pages redeploys on every push. The publish modules live in `scripts/publish/` (the primitive alone is `Rscript scripts/publish/publish_pages.R`), the way cleaning lives in `scripts/inat_observations/` and analysis in `scripts/analysis/`.

---

## Quickstart — run order

New here? Do the one-time **Setup** below, then run the three pipeline stages in
order. Each is a single command from the repo root; you do not source individual
scripts by hand.

```bash
# 1. INGEST + CLEAN — pulls iNaturalist into the DuckDB cache, cleans, exports tables
Rscript scripts/run_data_cleaning_pipeline.R

# 2. ANALYSIS — rebuilds every figure and table into data/analysis/ (no re-ingest)
Rscript scripts/run_all_analysis_pipeline.R

# 3. PUBLISH — copies the public pages into docs/ and rebuilds the landing page
Rscript scripts/run_publishing_materials_pipeline.R
```

Stage 1 is the slow one (it hits the API) and is **interactive** — it stops to ask
you to resolve unknown tags, verify new taxa, and confirm specimen IDs. Stages 2
and 3 are fast and non-interactive; run them alone whenever you have only changed
plotting or wording.

Stage 1 opens with a menu, so there is nothing to memorize: choose **1 (Normal
run)** unless you have a reason not to. The same modes are available as switches
if you are scripting it:

```bash
BEESCABR_SKIP_INGEST=1 Rscript scripts/run_data_cleaning_pipeline.R   # reuse the cache, no API calls
BEESCABR_FULL_INGEST=1 Rscript scripts/run_data_cleaning_pipeline.R   # re-fetch everything from scratch
```

**Running a season? Start with dev-docs/SEASON_RUNBOOK.md.** It walks the whole
season end to end, including what to update by hand first and what to do when a
stage stops. For how the code is laid out instead, see
**dev-docs/PIPELINE_GUIDE.md**.

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

## Setup

One-time package installation. Each script also has its own commented-out `install.packages()` line, but this is the full list — run once:

```r
install.packages(c(
  "tidyverse", "httr2", "stringr", "sf", "readxl", "writexl",
  "lubridate", "BeeBDC"
))

# Optional, for later GBIF work:
# install.packages(c("rgbif", "gbifdb"))
```


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
| 3 | **SEASON_RUNBOOK.md** | It is a new season and I am running this. What do I do, in order? Start here to *operate* the pipeline. |
| 4 | **WEBSITE_GUIDE.md** | The public site: how a page is built and published, the layout every page follows, and why the maps are built two different ways. |
| 5 | **MANUAL_INPUTS.md** | Which files must a human maintain by hand, and when? Read before any season. |
| 6 | **PIPELINE_GUIDE.md** | How is the code laid out, how does data flow, how do I extend it? |
| 7 | **LIMITATIONS.md** | What this pipeline knowingly trades away, and which data problems only a protocol change can fix. |
| 8 | **ANALYSIS_CATALOG.md** | What every analysis output is, one entry per figure/table. |
| 9 | **analysis_decisions.md** | The *why* behind each analysis: scope, parameters, statistical tests. |
| 10 | **ARCGIS_SPATIAL_MAPPING.md** | Where every boundary and transect layer came from, and the known coastal discrepancy. |
| 11 | **analysis_roadmap.md** | Stakeholder questions, triaged into a buildable plan. |
| 12 | **VERIFICATION_DESIGN.md** · **verification_guide.md** | What the verification workflow is, and how to answer its prompts. |
| 13 | **SPECIMEN_CHANGELOG.md** | Version history of the specimen spreadsheet, and how to bump it. |
| 14 | **SCRIPTS_GUIDE.md** · **TESTS_GUIDE.md** | Notes on the script layout and the test suite. |
| 15 | **TODO.md** | Open work, by area. |

Agent instructions live in **CLAUDE.md** (rules) and **AGENTS.md** (a pointer to it).

*There is exactly one `README.md` in this repository — this one. Every other
document has a distinct name so nothing is ambiguous in search results or tabs.*
