# The files you maintain by hand

Everything else the pipeline writes. **If a name ends in `_generated`, never edit
it** — the next run overwrites it, silently.

## The rule

```
  hand-maintained  →  the pipeline reads it   →  _generated output
  (you keep these correct)                       (never edit; re-run instead)
```

A wrong generated number is fixed **upstream**: in one of the files below, or in
the script. Never in the output.

## 1. People and effort

| File | Where | When it changes |
|---|---|---|
| `people_manual.csv` | `project_info/rosters/` | **Anyone new, before their first season** |
| `master_intern_survey_log_manual.csv` | `project_info/surveys/survey_date_sources/` | Each intern survey trip |
| `YYYY Cabrillo Bee Survey Calendar.pdf` | `.../beeple_calendar_windows/` | Each new season |
| `research_team_photos/` | `project_info/rosters/` | Team changes |

**Two files, two jobs — keep them in sync:**

| | Answers | Authority for |
|---|---|---|
| the roster | *who exists* | headcounts |
| the intern log | *how much* | effort, trips |

| Situation | What to edit |
|---|---|
| New **beeple** | roster only — their tagged observations become surveys on their own |
| New **intern** | roster **and** log |
| More dates for an existing intern | log only |
| General public contributor | nothing — automatic |

> A past bug counted people from the *effort log*, which stores netters by first
> name and iNat folks by handle. One person recurred across trips and could not be
> deduped, so the totals inflated. People come from the roster. Always.

## 2. Specimens

| File | Where |
|---|---|
| `cabr_bee_specimens_record_V{n}_{YYYY_MM_DD}.xlsx` | `specimens/records/` |

Newest version wins. **Save a new version; never edit an old one.**

## 3. Spatial

| File | Where |
|---|---|
| `cabr_bee_transects.shp` + sidecars | `spatial/shapefiles/transects/` |
| `cabr_survey_access_routes.shp` | `spatial/shapefiles/access_routes_to_transects/` |
| `transect_years_manual.csv` | `spatial/shapefiles/transects/` |

A shapefile is a *set* of files sharing a name. Keep them together.

## 4. External reference

| File | Where |
|---|---|
| `San Diego County Bee Species Checklist, v3.xlsx` | `reference/source/holway_2026/` |

## 5. Curated corrections

All in `data/reference/hand_curated/`. These are where you fix a taxon the
pipeline keeps getting wrong.

| File | Fixes |
|---|---|
| `manual_taxon_overrides.csv` | taxonomy the auto-resolver got wrong |
| `specimen_additions.csv` | specimen-only species missing from the lookup |
| `plant_specimen_overrides.csv` | plant names on specimen labels |
| `plant_park_confirmed.csv` | plants confirmed present in the park |
| `verified_taxa.csv` / `rejected_taxa.csv` | taxa you accepted or excluded |

## Naming rules

| | |
|---|---|
| Specimens | `cabr_bee_specimens_record_V{n}_{YYYY_MM_DD}.xlsx` |
| Calendars | `YYYY Cabrillo Bee Survey Calendar.pdf` |
| Columns | bare rank names — `genus`, not `taxon_genus` |
| Suffixes | `_manual` = yours · `_generated` = the pipeline's |

> iNaturalist and GBIF **exports are retired**. Do not drop export files in — the
> pipeline pulls from the API itself.
