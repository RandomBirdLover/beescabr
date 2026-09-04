# Getting the code and the data

## Just want the results?

**[randombirdlover.github.io/beescabr](https://randombirdlover.github.io/beescabr/)** —
every map, figure and table. Nothing to download.

## What you get, and what you don't

| | |
|---|---|
| **In the repo** | the code (`scripts/`), the website (`docs/`), the docs |
| **NOT in the repo** | `data/` — all of it |

`data/` is kept off GitHub for three reasons: it holds people's names and emails,
it holds API keys, and much of it the pipeline rebuilds anyway.

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
maps), plus `checklists/`, `reference/`, `analysis/`.

## Running it yourself

Needs R. Full walkthrough in `PIPELINE_GUIDE.md`; the short version:

```r
source("scripts/utils/install_requirements.R")        # once
source("scripts/run_data_cleaning_pipeline.R")
source("scripts/run_all_analysis_pipeline.R")
source("scripts/run_publishing_materials_pipeline.R")
```

To reuse the data you were given instead of re-downloading from iNaturalist:

```r
Sys.setenv(BEESCABR_SKIP_INGEST = "1"); source("scripts/run_data_cleaning_pipeline.R")
```

> `sf` and `ggspatial` sometimes need system libraries (GDAL, GEOS, PROJ). If they
> fail: [r-spatial.github.io/sf](https://r-spatial.github.io/sf/)

## Never make public

| | Where |
|---|---|
| **API keys** | `data/secrets/` |
| **Rosters** (names, emails) | `data/project_info/rosters/` |

The whole `data/` folder is gitignored, so as long as you don't force it in,
you're safe.

## Questions

| Who | For what |
|---|---|
| Brandi Sanchez — Conservation Legacy, Scientists in Parks | how the code and cleaned data work |
| Taro Katayama — NPS, Cabrillo National Monument | holds the data; can send you `data/` |
