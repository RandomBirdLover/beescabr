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

Needs R, and `beescabr.Rproj` opened in RStudio first — that sets the working
directory every path assumes.

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

**Everything else is in `PIPELINE_GUIDE.md`**: installing, API keys, the run menu
(pick **Offline run** to use the `data/` you were handed), and the three stages.

## Why `data/` is not public

| | |
|---|---|
| **It names people** | rosters carry the names and emails of volunteers and interns who signed up to survey bees, not to be published |
| **It holds API keys** | a key in `data/secrets/` is a personal credential; a leaked one is used under its owner's name |
| **Precise coordinates** | iNaturalist records carry GPS to the metre inside a national monument, including where the rare bees were found |
| **It is not the record of truth** | most of `data/` is rebuilt from iNaturalist and the specimen sheet; publishing a stale copy invites someone to cite a number that has since changed |

## Handing the data to someone else

**Delete `data/secrets/` from any copy that leaves your machine.** A key is personal:
an iNaturalist pull runs as whoever signed in, and coordinate trust is granted by each
observer to a specific account. The next operator makes **their own OAuth app under the
park's iNaturalist account** — not a new account of their own, which would get obscured
coordinates until every observer trusted it separately. The pipeline asks for the values
on their first run and saves them to their own `data/secrets/`.

Nothing else needs removing. `data/` is gitignored, and no names appear in the README
or in this file.

---

The whole folder is gitignored, so it stays out unless someone forces it in.
Nothing is hidden about the *results*: every figure, map and table is on the
public site. What is withheld is the personal and locational detail behind them.
