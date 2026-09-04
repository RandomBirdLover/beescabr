# Getting the code and the data

## Just want the results?

**[randombirdlover.github.io/beescabr](https://randombirdlover.github.io/beescabr/)** —
every map, figure and table. Nothing to download.

## What you get, and what you don't

| | |
|---|---|
| **In the repo** | the code (`scripts/`), the website (`docs/`), the docs |
| **NOT in the repo** | `data/` — all of it |

`data/` is kept off GitHub. Why, in full, is at the bottom of this page.

## Getting it

```
1. Download the code   github.com/RandomBirdLover/beescabr -> Code -> Download ZIP
2. Ask for the data    from the project lead: a data/ folder, by drive or cloud
3. Put it inside       beescabr/
                         scripts/
                         docs/
                         data/     <-- goes here
```

Inside `data/`: `specimens/` (netted bees), `inat_observations/` (photographed
bees), `project_info/` (survey logs, rosters), `spatial/` (park and transect
maps), plus `checklists/`, `reference/`, `analysis/` and `secrets/`.

## Running it yourself

Needs R. Full walkthrough in `PIPELINE_GUIDE.md`; the short version:

**Open `beescabr.Rproj` in RStudio first.** That sets the working directory, and
every path in the project assumes it. Skip this and the first line fails.

```r
source("scripts/utils/install_requirements.R")        # once
source("scripts/run_data_cleaning_pipeline.R")
source("scripts/run_all_analysis_pipeline.R")
source("scripts/run_publishing_materials_pipeline.R")
```

The cleaning pipeline **asks what kind of run you want** and prints the choices:

| | | |
|---|---|---|
| 1 | Normal run | pull only what is new since last time (seconds) |
| 2 | Bees only | plants skipped, so plant data goes stale |
| 3 | **Offline run** | no iNaturalist calls at all — reuse what you were given |
| 4 | Full rebuild | re-download everything, ~40+ min, needs bee expertise |

**Pick 3 the first time.** It uses the `data/` you were handed and touches no API.

You do not need to set any environment variable, and you should not: a flag left
over from an earlier run makes the pipeline skip this menu without saying so.

> `sf` and `ggspatial` sometimes need system libraries (GDAL, GEOS, PROJ). If they
> fail: [r-spatial.github.io/sf](https://r-spatial.github.io/sf/)

## Why `data/` is not public

| | |
|---|---|
| **It names people** | rosters carry the names and emails of volunteers and interns who signed up to survey bees, not to be published |
| **It holds API keys** | a key in `data/secrets/` is a personal credential; a leaked one is used under its owner's name |
| **Precise coordinates** | iNaturalist records carry GPS to the metre inside a national monument, including where the rare bees were found |
| **It is not the record of truth** | most of `data/` is rebuilt from iNaturalist and the specimen sheet; publishing a stale copy invites someone to cite a number that has since changed |

The whole folder is gitignored, so it stays out unless someone forces it in.
Nothing is hidden about the *results*: every figure, map and table is on the
public site. What is withheld is the personal and locational detail behind them.
