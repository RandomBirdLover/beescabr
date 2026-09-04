# beescabr

Native bee pipeline for the long-term monitoring of native bee biodiversity at Cabrillo National Monument (CABR).

## 🐝 Live site

Browse the public field guides, checklists, and interactive maps online — no code or downloads needed:

**https://randombirdlover.github.io/beescabr/**

Includes the bee field guides (species + genus), park summary tables, least-sampled bees, the survey-transect and bee-bounty maps, and an interactive **Bee Occurrence Explorer** (filter every record by genus, species, transect, and method).

The site is built by stage 3 of the pipeline, below. `docs/` is committed to git,
and GitHub Pages redeploys on every push.

---

## Running it

```
config.R                                  loads first, on its own
   |                                      every script sources it; you never run it
   v
1. run_data_cleaning_pipeline.R           ingest + clean       slow, asks questions
   |
   v
2. run_all_analysis_pipeline.R            figures + tables     fast, silent
   |
   v
3. run_publishing_materials_pipeline.R    the public site      fast, silent
```

```r
source("scripts/run_data_cleaning_pipeline.R")
source("scripts/run_all_analysis_pipeline.R")
source("scripts/run_publishing_materials_pipeline.R")
```

**Run them in that order.** Each stage reads what the one before it wrote, and
stage 3 stops rather than publish a page older than the data.

Open `beescabr.Rproj` in RStudio first -- that sets the working directory, which
every path in the project assumes.

**The full guide is `dev-docs/PIPELINE_GUIDE.md`**: first run on a new machine,
API keys, the files you maintain by hand, what each stage does, starting a new
season, and what to do when something breaks.

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
- **Before handing `data/` to anyone, delete `data/secrets/`.** It holds your iNaturalist and IUCN API keys. A key is personal: a pull runs as whoever signed in, and coordinate trust is granted by each observer to a specific account. The next person makes their own OAuth app under the park's iNaturalist account — the pipeline asks on their first run. See `dev-docs/DATA_ACCESS.md`.
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
| 3 | **WEBSITE_GUIDE.md** | The public site: how a page is built and published, the layout every page follows, and why the maps are built two different ways. |
| 4 | **MANUAL_INPUTS.md** | Which files must a human maintain by hand, and when? Read before any season. |
| 5 | **PIPELINE_GUIDE.md** | **The main guide.** Part 1: running a season, start to finish. Part 2: how the code is built. |
| 6 | **LIMITATIONS.md** | What this pipeline knowingly trades away, and which data problems only a protocol change can fix. |
| 7 | *(generated)* | What every analysis output is: `data/analysis/<year>_generated/findings_index.csv`, one row per analysis, plus a note in each folder. |
| 8 | **ANALYSIS_DECISIONS.md** | The *why* behind each analysis: scope, parameters, statistical tests. |
| 9 | **ARCGIS_SPATIAL_MAPPING.md** | Where every boundary and transect layer came from, and the known coastal discrepancy. |
| 10 | **VERIFICATION.md** | What the verification workflow is, and how to answer the prompt. |
| 11 | **SPECIMEN_CHANGELOG.md** | Version history of the specimen spreadsheet, and how to bump it. |
| 12 | **SCRIPTS_GUIDE.md** · **TESTS_GUIDE.md** | Notes on the script layout and the test suite. |
| 13 | **FUNCTIONS.md** | *(generated)* Every function called from more than one file: what it does and what to pass it. |
| 14 | **TODO.md** | Open work, by area. |

Agent instructions live in **CLAUDE.md** (rules) and **AGENTS.md** (a pointer to it).

*There is exactly one `README.md` in this repository — this one. Every other
document has a distinct name so nothing is ambiguous in search results or tabs.*
