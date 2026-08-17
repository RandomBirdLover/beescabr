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

`docs/` is committed to git; GitHub Pages redeploys on every push. The publish modules live in `scripts/publish/` (the primitive alone is `Rscript scripts/publish/publish_pages.R`), the way cleaning lives in `scripts/clean/` and analysis in `scripts/analysis/`.

---

## Quickstart — run order

New here? Do the one-time **Setup** below, then run the pipeline. In normal use, open `scripts/analysis/native_bee_data_analysis.Rmd` and run it chunk by chunk — it sources everything else in order. Individual scripts in dependency order:

1. `scripts/clean/inat_bee_clean.R` — cleans non-lethal bee iNat data (intern + beeple); writes `data/outputs/inat_clean/cabr_inat_bee_clean.csv`.
2. `scripts/clean/inat_plant_clean.R` — cleans non-lethal plant iNat data; writes `data/outputs/inat_clean/cabr_inat_plant_clean.csv`.
3. `scripts/checklists/native_bee_checklist.R` — writes Tier 1 and Tier 2 checklists. Auto-sources `spatial_utils.R`, `utils.R`, and `specimen_bee_clean.R` as needed. **Do not run `specimen_bee_clean.R` standalone first** — it depends on a file Part A of this script writes.
4. `scripts/analysis/native_bee_data_analysis.Rmd` — orchestrator; sources the above and runs the richness/method analysis.

Standalone diagnostics (run only when needed): `scripts/spatial/check_boundaries.R`, `plot_boundaries_individually.R`, `diagnose_county_gap.R`. One-time filter: `scripts/checklists/dorey_bee_checklist.R`.

Full detail: see **Pipeline overview** below.

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

`BeeBDC` was added for Dorey et al. (2023) cross-referencing (`data/reference_exports/dorey_2023/`), but is **deprioritized** — cross-referencing returned zero CABR records and Jess flagged the Dorey iNat records as too old. Left installed in case this gets revisited.

---

## Repository structure

```
beescabr/
  scripts/
    utils/
      utils.R                        # shared read_latest(), require_columns()
      parse_beeple_calendars.py      # parses annual calendar PDFs → beeple_calendar_windows.csv
      survey_dates.R                 # infers beeple survey dates from iNat obs → beeple_survey_dates_official.csv + intern_survey_dates_official.csv
    clean/
      inat_bee_clean.R               # cleans non-lethal bee iNat data (intern + beeple)
      inat_plant_clean.R             # cleans non-lethal plant iNat data
      specimen_bee_clean.R           # cleans lethal CABR specimen data; QC flags; complex match
    checklists/
      native_bee_checklist.R         # PART A: TIER 1 (iNat-only) checklists, SD County/Point Loma/CABR
                                      # PART B: TIER 2 (merged) checklists + CABR specimen checklist
      dorey_bee_checklist.R          # ONE-TIME/MANUAL: filters Dorey et al. (2023) to SD County
    analysis/
      native_bee_data_analysis.Rmd   # main analysis document — sources the above
    spatial/
      spatial_utils.R                # loads + reprojects boundaries; containment checks; 10m transect buffers
      check_boundaries.R             # diagnostic: plots all boundaries overlaid
      plot_boundaries_individually.R # one map per boundary layer
      diagnose_county_gap.R          # computes gap between point_loma_boundary and sd_county_boundary
  data/                            # gitignored — NOT on GitHub (see .gitignore)
    project_info/
      surveyors_by_year.csv        # per-year surveyor roster: username, role, method, technique
      project_tags_fields.csv      # iNat tag / observation-field → keep/flag/exclude crosswalk
      beeple_calendar_windows.csv  # parsed from annual calendar PDFs: one row per (year, person, transect, window)
      beeple_calendar/             # annual calendar PDFs: "YYYY Cabrillo Bee Survey Calendar.pdf"
                                   # drop new year's PDF here and re-run parse_beeple_calendars.py
      intern_survey_dates.csv               # raw intern dates input (one row per person per date)
      beeple_survey_dates_official.csv      # PERMANENT beeple record; one row per window, transects as username columns
                                            # rows marked "manual" or "skipped" are never overwritten
      intern_survey_dates_official.csv      # PERMANENT intern record; one row per date, full names + usernames
      survey_dates_needs_review.csv         # ambiguous/no_obs beeple windows needing manual attention (auto-deleted when resolved)
    reference_exports/
      gbif/                        # GBIF regional reference exports
      dorey_2023/                  # BeeBDC (Dorey et al. 2023) global dataset, filtered to SD County
      holway_2026/                 # Dr. Holway's SD County Bee Species Checklist v3, flattened to CSV
    cabr_surveys/
      lethal/                      # all versions of specimen file live here
        cabr_bee_specimens_record_V1_2026-05-04.xlsx
        cabr_bee_specimens_record_V2_2026-05-29.xlsx
        ...
        cabr_bee_specimens_record_V{n}_{YYYY-MM-DD}.xlsx   ← newest = authoritative
        deposit/                   # permanent specimen transfers
        loans/                     # temporary specimen transfers
      nonlethal/
        inat_bee/                  # iNat SD County bee exports (inat_native_bees_sdcounty_25_mi_buffer_*)
        inat_plant/                # iNat Point Loma Peninsula plant exports (inat_plants_point_loma_peninsula_*)
    spatial/
      transects/
        cabr_bee_transects.shp     # source of truth for transect buffers
      boundaries/
        cabr/
          nps_official/            # NPS-authoritative shapefiles
            cabr_boundary_nps_official.shp
            cabr_tracts_nps_official.shp
          cabr_survey_box.shp      # hand-drawn survey inclusion polygon (see Spatial analysis)
        point_loma/                # point_loma_boundary.shp
        san_diego_county/          # sd_county_boundary.shp (Union+Dissolve — see Spatial analysis)
                                    # DIAGNOSTIC_county_gap.shp (diagnostic output)
    outputs/                       # generated by scripts — do not edit manually
      inat_clean/
        cabr_inat_bee_clean.csv
        cabr_inat_plant_clean.csv
        qc/
          cabr_inat_bee_unknown_tags.csv
          cabr_inat_plant_unknown_tags.csv
      checklists/
        cabr/
          cabr_combined_native_bee_checklist.csv   # TIER 2 merged (iNat + specimens)
          cabr_inat_bee_checklist.csv              # TIER 1 iNat-only
          cabr_specimen_bee_checklist.csv          # specimen-only
        point_loma/
          pl_inat_native_bee_checklist.csv         # TIER 1
          pl_native_bee_checklist.csv              # TIER 2
        sd_county/
          sd_county_inat_native_bee_checklist.csv  # TIER 1
          sd_county_native_bee_checklist.csv       # TIER 2
      specimens/
        cabr_specimen_bee_record_clean.csv
        cabr_specimen_bee_missing.csv
        cabr_specimen_bee_duplicates.csv
      reference/
        bee_taxonomy_lookup.csv
  SPECIMEN_CHANGELOG.md            # version history for specimen spreadsheet
  README.md
```

---

## Data rules

- **GBIF = regional reference context only.** GBIF records are never used as CABR survey records. CABR specimens are being deposited to SDNHM (via Pam Horsley) and are not yet in GBIF.
- **`data/` is gitignored** — no specimen, iNat, or personal data is on GitHub. Exception: shapefiles under `data/spatial/`.
- **`data/outputs/` is generated.** Never edit output CSVs by hand — re-run the relevant script instead.
- **Buffers are generated in R**, not stored as shapefiles. Do not commit `Buffer_10m.*` or any derived shapefile.

---

## Updating survey participation (people & effort)

The NPS summary participation table counts **people** and **effort** from two
different files. Keeping them straight matters: a past bug counted people from
the effort log and inflated the totals, because that log stores netters by first
name and iNat folks by handle, so one person recurs across trips and cannot be
deduped. **The roster is the sole authority for *who*; the effort log is for
*how much*.**

| File | Path | Role | Edit by hand? |
| --- | --- | --- | --- |
| `surveyor_roster.csv` | `data/project_info/` | Canonical people list, one row per person-year (full name, role, method). **Authority for headcounts.** | ✅ Yes |
| `master_intern_survey_log.csv` | `data/project_info/survey_date_sources/` | Curated intern survey days (both lethal net days and non-lethal iNat days). | ✅ Yes |
| `master_per_survey_info.csv` | `data/project_info/` | **Generated output** — rebuilt from the two files above by `finding_project_info.R`. Used for effort only (trip counts, method split), never for headcounts. | ❌ No — never hand-edit |

**What to edit when**

- **New beeple (community scientist):** add them to `surveyor_roster.csv` only.
  Their survey dates flow in automatically from their tagged iNaturalist
  observations.
- **New intern:** add them to `surveyor_roster.csv` **and** add their net/photo
  days to `sources/master_intern_survey_log.csv`. Intern days are not all
  tag-derivable, so the log is their source of truth.
- **More survey dates for existing interns:** add them to
  `sources/master_intern_survey_log.csv`, then re-run the pipeline.
- **Never** edit `master_per_survey_info.csv` directly. Re-run
  `finding_project_info.R` (or the full pipeline) to rebuild it.

**Gotcha:** the roster (people) and the intern log (dates) are two separate
manual files that must stay in sync. If you add a new intern's dates to the log
but forget to add the person to the roster, the effort counts update but the
headcount does not. Rule of thumb: a new person means edit the roster; a new
intern date means edit the log; a new intern means edit both.

General public contributors (iNaturalist observers not on the roster) and the
total-contributor count update automatically from the iNaturalist data. No
manual entry is needed for those.

---

## Naming conventions

### iNat and GBIF exports

```
inat_native_bees_sdcounty_25_mi_buffer_YYYY-MM-DD.csv
inat_plants_point_loma_peninsula_YYYY-MM-DD.csv
gbif_bees_sdcounty_YYYY-MM-DD.csv
```

`inat_native_bees_sdcounty_25_mi_buffer` is one master export covering all SD County bees except *Apis mellifera* (excluded at export time — see **iNat export instructions**). "Native" refers only to this honey-bee exclusion; no other non-native species are filtered. `native_bee_checklist.R` spatially splits this export into three tiers (SD County / Point Loma / CABR) — the three tier checklists are all derived independently from this one file, not nested.

Date = download date, YYYY-MM-DD. Drop directly into `data/cabr_surveys/nonlethal/inat_bee/`. Scripts auto-detect the newest file via `read_latest()`.

### Specimen file

```
cabr_bee_specimens_record_V{n}_{YYYY-MM-DD}.xlsx
```

All versions live in `data/cabr_surveys/lethal/`. The pipeline always reads the newest. See **Specimen version management** below.

### Deposit and loan tracking files

```
deposit/cabr_bee_specimen_record_deposit_{taxon}_{recipient}.xlsx
loans/cabr_bee_loan_{taxon}_{recipient}.xlsx
```

### Column naming: bare rank names

All pipeline outputs use **bare taxonomic rank names** — `kingdom`, `phylum`, `class`, `order`, `superfamily`, `family`, `subfamily`, `tribe`, `subtribe`, `genus`, `subgenus`, `species`, `subspecies` — not iNat's `taxon_kingdom_name` / `taxon_genus_name` wrapping.

Two special cases:
- `taxon_id` — kept as-is (not shortened to `id`, which collides with the observation ID column).
- `taxon_complex_name` → `complex`; `taxon_complex_id` → `complex_taxon_id`. In Tier 2 / Holway-format outputs the `Complex` column is prefixed `"(Complex) "` (e.g. `"(Complex) Diadasia australis"`) so complex-level IDs aren't misread as confirmed species.

**Deliverable exception:** one-off polished outputs (reports, presentations) may use simplified headers on that copy only — do not change the pipeline naming.

---

## Specimen version management

All versions of the specimen Excel file live in `data/cabr_surveys/lethal/`. `read_latest()` picks up the newest automatically.

**When you update the specimen file:**
1. Open the current version in Excel
2. Make your changes
3. **Save As** → `cabr_bee_specimens_record_V{n+1}_{YYYY-MM-DD}.xlsx` in the same folder
4. Add a row to `SPECIMEN_CHANGELOG.md`
5. Commit and push `SPECIMEN_CHANGELOG.md`

**Do NOT** delete or overwrite old versions. **Do NOT** rename without updating the version number and date.

**Version bump rule:** any deletion or structural change (column added, removed, renamed; values corrected or reclassified) = new version. Adding rows does not require a bump but should be noted in the changelog.

---

## iNat export instructions

One master export per refresh cycle — covers all of SD County and is reused for all three geographic tier checklists. No need to re-export per tier.

Go to **inaturalist.org/observations/export** and apply these settings:

### Filters

| Field | Value |
|-------|-------|
| Taxon | `Anthophila` (all bees) |
| Without taxon | *Apis mellifera* (Western Honey Bee) |
| Place | **San Diego County 25 Mile Buffer** |
| Quality grade | Any |
| Geoprivacy | Include obscured |
| Captive/Cultivated | Unchecked (exclude) |

**Quality grade = Any, not Research Grade:** Research Grade requires species-level ID. Most bees are only identifiable to genus — restricting to Research Grade would silently drop a large share of valid observations.

**Exclude *Apis mellifera*:** honey bee volume bloats file size without adding value for a native-bee analysis. This exclusion is at export time, so a future honey-bee question requires a separate export.

**25 Mile Buffer, not plain county:** survey-relevant observations near the county edge shouldn't be cut off by the administrative polygon boundary. Spatial filtering to actual boundaries of interest happens downstream in `spatial_utils.R`.

For plants, export taxon `Plantae` separately using the same Place setting.

### Columns — uncheck to reduce file size

- Observation fields
- Description
- Tag list
- Sounds

### Required columns

`id`, `observed_on`, `quality_grade`, `taxon_id`, `scientific_name`, `common_name`, `taxon_rank`, `latitude`, `longitude`, `positional_accuracy`, `geoprivacy`, `captive_cultivated`, `user_login`, `place_guess`

### After downloading

Rename to the convention above and drop into the correct subfolder. Do not keep the default iNat filename.

---

## iNaturalist API

Two scripts pull from the API rather than the CSV export:

- `scripts/clean/inat_bee_clean.R` — fetches survey observations, tags, and observation fields via `/v1/observations`.
- `scripts/checklists/native_bee_checklist.R` — per-taxon taxonomy/ancestry lookups (~400 calls, ~3–4 min), via the v1 taxa endpoints.

**Endpoint:** `https://api.inaturalist.org/v1/observations`. Read-only; no authentication required. Max `per_page = 200`; `inat_bee_clean.R` pages with an `id_above` cursor. Rate limit: ~1 request/second; scripts include `Sys.sleep(1)`.

**Why the API and not the export:** iNat's CSV export only includes observation fields the *exporter* has personally used — fields attached by other observers are invisible. The API returns every field on every observation (`ofvs`), which is what the crosswalk triage requires.

**Why v1:** v1 is what iNaturalist's own site and apps run on — it is the most stable choice. v2 exists but has returned incomplete results on some queries. iNat's deprecated version is v0 (Rails), not v1.

**If v1 is retired:** swap `/v1/` → `/v2/` and add a `fields` parameter (v2 returns minimal data by default; v1 returns everything). Watch iNaturalist's forum (News & Updates) for any sunset notice.

---

## Pipeline overview

`scripts/analysis/native_bee_data_analysis.Rmd` is the orchestrator — in practice, just run the Rmd chunk by chunk. Listed individually here for reference:

```
1. scripts/spatial/spatial_utils.R      loads boundary shapefiles (CABR, Point Loma, SD County)
                                         + cabr_bee_transects.shp → generates buffer_10m in memory.
                                         Sourced automatically by native_bee_checklist.R.

2. scripts/clean/inat_bee_clean.R        fetches non-lethal bee survey observations from the
                                         iNaturalist API (v1): roster observers, CABR box, bees
                                         minus Apis mellifera. Fills any "fill in" field options
                                         from the API, clips to cabr_survey_box, cleans
                                         (dates/missing flags/data_source), and TRIAGES every
                                         observation against project_tags_fields.csv into
                                         keep / flag / exclude. Adding rows/variants to the
                                         crosswalk changes behavior with no code edit.
                                         → data/outputs/inat_clean/cabr_inat_bee_clean.csv
                                         → data/outputs/inat_clean/qc/cabr_inat_bee_unknown_tags.csv

   scripts/clean/inat_plant_clean.R      same pipeline for non-lethal plant survey observations.
                                         → data/outputs/inat_clean/cabr_inat_plant_clean.csv
                                         → data/outputs/inat_clean/qc/cabr_inat_plant_unknown_tags.csv

3. scripts/checklists/native_bee_checklist.R
                                   PART A: reads the master iNat export, spatially splits into
                                   three tiers (SD County / Point Loma / CABR) using boundaries
                                   from spatial_utils.R, then deduplicates each subset to unique
                                   taxa. Hits iNat API ~400 calls once across all 3 tiers.
                                   → data/outputs/checklists/sd_county/sd_county_inat_native_bee_checklist.csv
                                   → data/outputs/checklists/point_loma/pl_inat_native_bee_checklist.csv
                                   → data/outputs/checklists/cabr/cabr_inat_bee_checklist.csv
                                   TIER 1 (iNat-only) checklists. Only re-run if a file is missing.

                                   PART B: builds TIER 2 (merged) checklists in Holway-format
                                   columns, folding in CABR specimen evidence and a Holway
                                   cross-check for SD County.
                                   → data/outputs/checklists/cabr/cabr_combined_native_bee_checklist.csv
                                   → data/outputs/checklists/cabr/cabr_specimen_bee_checklist.csv
                                   → data/outputs/checklists/point_loma/pl_native_bee_checklist.csv
                                   → data/outputs/checklists/sd_county/sd_county_native_bee_checklist.csv

                                   DEPENDENCY: Part B auto-sources specimen_bee_clean.R if its
                                   output is missing. But specimen_bee_clean.R needs
                                   sd_county_inat_native_bee_checklist.csv (written by Part A).
                                   This resolves correctly when the script runs as a whole.
                                   Do NOT run specimen_bee_clean.R standalone before
                                   native_bee_checklist.R has run at least once.

4. scripts/clean/specimen_bee_clean.R    reads newest cabr_bee_specimens_record_V{n} → QC-flags
                                         missing lat/long, date, sdnhm_id, ucsd_id, genus →
                                         matches complex/complex_taxon_id against the SD County
                                         TIER 1 checklist. Normally runs automatically via
                                         native_bee_checklist.R Part B, not standalone.
                                         → data/outputs/specimens/cabr_specimen_bee_record_clean.csv
                                         → data/outputs/specimens/cabr_specimen_bee_missing.csv

5. scripts/analysis/native_bee_data_analysis.Rmd
                                         sources 2–4 above (which source 1), then runs the
                                         richness/method comparison.
```

`scripts/utils/utils.R` (shared `read_latest()` and `require_columns()`) is sourced by every script — not a standalone step.

---

## Reviewing unknown tags and fields

Both `inat_bee_clean.R` and `inat_plant_clean.R` triage observations against the crosswalk. Anything unrecognized is ignored, but the console prints an **ACTION NEEDED** block and writes QC files. Check after each run.

### Unknown tags

**`data/outputs/inat_clean/qc/cabr_inat_bee_unknown_tags.csv`** (bees) and **`cabr_inat_plant_unknown_tags.csv`** (plants).

This list is normally long and mostly harmless — camera/lens tags (`D500`, `300mm f/4`), species names, photo filenames, `City Nature Challenge`, etc. Ignore those. Scan for one thing only: a tag that looks like a **missed survey tag** — a new typo or new survey year. If you spot one, add it as an `inat_variant` on the matching crosswalk row, re-run, and those observations move from `flag` to `keep`.

This list won't trend to zero and shouldn't.

### Unknown fields

A separate **ACTION NEEDED** block covers unknown observation fields — structured key-value fields (e.g. `Nesting bee`, `on ground?`) with no crosswalk row. Unlike unknown tags, this list **should trend toward zero**: every field observers actually use should eventually have a row telling the script what to do with it.

When you see an unknown field:
1. Look it up on iNat by field ID or name.
2. Decide: relevant? Add a row with `type = obs_field`. Not relevant? Add a row with `type = ignore`.
3. Re-run — it should disappear from the ACTION NEEDED block.

---

## Console prompts — standard keys

Every interactive prompt in the pipeline uses one consistent set of keys, so you never have to guess what `Enter` will do. There are two kinds.

**Decision gates — stop, or keep going.** These appear when a run hits something you might want to fix first (duplicate specimen IDs, off-transect survey pins, spell-check flags). They all use the same words:

- Type **`skip`** to continue past the gate (leave it for later), or **`stop`** to halt the run so you can fix it now.
- Case-insensitive, and common synonyms work: `s` / `continue` / `c` / `go` / `ok` / `y` / `yes` all continue; `x` / `halt` / `fix` / `n` / `no` all stop.
- A bare **Enter**, or anything the prompt doesn't recognize, **re-asks** instead of guessing — so a stray keystroke can never silently skip a real problem or halt the run.

**Heads-up prompts — nothing to decide.** When a step is only telling you something (e.g. "here are the maps to send your surveyors"), it ends with **"Press Enter to continue"**, and there `Enter` always means continue.

**Item-by-item reviewers** — `review_crosswalk`, `review_windows` (and transect ties), `review_notes`, `review_plant_names`. Each walks one item at a time behind a `>` prompt and prints its own legend on screen. They share the same control keys:

- **`<Enter>`** — accept the highlighted (`*`) suggestion, where one is shown.
- **`s`** — skip this item for now (it comes back next run).
- **`q`** — save everything and quit the reviewer.
- **`?`** — show the key help again.

Each reviewer adds its own action keys on top of those — e.g. windows: `y`/`n`/`u` and `l`=list URLs; crosswalk: `i`=ignore, `n`=new concept, `1,2`=file under concepts; plant names: `a`=add-as-new, or a number to file under a canonical — all listed in that reviewer's on-screen legend.

---

## project_tags_fields.csv — crosswalk reference

`data/project_info/project_tags_fields.csv` controls `inat_bee_clean.R` and `inat_plant_clean.R`. Adding or editing rows changes script behavior without touching any R code.

### Column reference

| Column | What it holds |
|--------|---------------|
| `name` | Human-readable tag or field name |
| `field_id` | iNat observation field ID(s); blank for tags and derived fields |
| `category` | Grouping label (Transect, Beeple, Intern, Exclude, Field, Timing-Weather, Derived, Location) |
| `type` | Controls how the script handles this row — see Type values below |
| `datatype` | Expected value type: `text`, `taxon`, `numeric`, `time` |
| `allowed_values` | Pipe-separated valid values; `"fill in"` = auto-fetch from iNat API on next run |
| `applies_to` | Which export this row applies to: `bee`, `plant`, `both`, or `exclude` |
| `method_context` | Survey method(s): `non-lethal`, `lethal`, `both`, or `n/a` |
| `current_location` | Where the script looks for this value: `tag_list`, `obs_field`, `description (notes)`, `tag_list (photo-metadata tags)` |
| `inat_variants` | Known alternate spellings/capitalizations in raw iNat data |
| `specimen_plot_variants` | How this tag/field appears in physical plot or specimen sheets |
| `verified` | `TRUE` = confirmed against real data; `FALSE` = added but not yet spot-checked |
| `notes` | What this tag/field means and when to use it |
| `script_rules` | Logic the script applies beyond allowed_values (normalization, derivation heuristics, overrides) |

### Type values

| type | Meaning |
|------|---------|
| `tag` | iNat observation tag (free-text label). Script looks in `tag_list`. |
| `obs_field` | iNat observation field (structured key-value). Script looks in `ofvs` from the API. |
| `notes_field` | Value extracted from the free-text description/notes field. |
| `derived` | Computed by the script from other data; not directly from iNat. |
| `ignore` | **Field exists in iNat data but is not processed. The observation is kept — only this field is skipped.** Use for survey-irrelevant fields (e.g. `Fasciation`, `Leaf aroma`). |
| `exclude` | **Tag marks an entire observation to be dropped.** The observation is removed, not just the field. Use for non-Cabrillo projects swept in by the SD County pull, or pilot data excluded by the PI. |

`ignore` and `exclude` are deliberately different: `ignore` = keep the obs, skip the field; `exclude` = drop the obs entirely.

---

## Complex rank handling

iNaturalist uses **Complex** for cryptic species groups that can't be distinguished from photos (e.g. *Andrena osmioides*, *Diadasia australis*).

- Each taxon has `complex` (complex name) and `complex_taxon_id` columns in the checklist.
- Complexes are not excluded from richness counts — each unique `taxon_id` counts as one taxon.
- `complex` is the join key for matching iNat photo observations against museum specimens. Exact `taxon_id` match is preferred; complex-level matches are flagged separately.
- In Tier 2 / Holway-format outputs, `Complex` values are prefixed `"(Complex) "` (e.g. `"(Complex) Diadasia australis"`) so they aren't misread as confirmed species binomials.

---

## Spatial analysis

Transect buffers are generated in R via `scripts/spatial/spatial_utils.R`. Default = 10m. To change:

```r
buffer_dist_m <- 5  # change this one line in spatial_utils.R
```

CRS: EPSG:26946 (NAD83 / California zone 6, meters).

### Boundary layers

| File | Location | Source | Notes |
|------|----------|--------|-------|
| `cabr_boundary_nps_official.shp` | `boundaries/cabr/nps_official/` | NPS Land Resources Division (UNIT_CODE = CABR) | Official monument boundary (~160 acres). Used as a provenance label only — not a spatial filter. |
| `cabr_tracts_nps_official.shp` | `boundaries/cabr/nps_official/` | NPS Land Resources Division | Official NPS tract boundaries. |
| `cabr_survey_box.shp` | `boundaries/cabr/` | Hand-drawn in ArcGIS Pro | The actual CABR-tier inclusion geometry. See below. |
| `point_loma_boundary.shp` | `boundaries/point_loma/` | City of San Diego "PENINSULA" community plan district (CPCODE 30) | Unmodified authoritative source. |
| `sd_county_boundary.shp` | `boundaries/san_diego_county/` | County of San Diego Open Data Portal, unioned with `point_loma_boundary` then dissolved | **Not the raw county boundary alone** — a single dissolved polygon covering County + Point Loma combined. Original county-only file not preserved. |

All shapefiles are in EPSG:26946. `spatial_utils.R` calls `st_transform()` on load as a defensive no-op.

**`sd_county_boundary.shp` note:** because this is a Union+Dissolve of county + Point Loma, "Point Loma within SD County" is true by construction. If you ever need the unmodified county boundary, re-download it separately.

**1m noise buffer:** the Union+Dissolve left microscopic slivers along the Point Loma coastline (~4 sq ft total) — floating-point noise, not a real gap, but enough to make `st_contains()` return `FALSE`. `spatial_utils.R` applies a 1m buffer to `sd_county_boundary` on load to absorb this. See `diagnose_county_gap.R` for details.

### CABR survey area vs. official NPS boundary

The BST transect begins on Navy-owned land south of the official CABR (NPS) boundary, but this area is surveyed as part of CABR (per Taro, 2026-06-22).

`cabr_survey_box` is a hand-drawn polygon (not a formula buffer) extending past `cabr_boundary` on the north, south, and southeast. It is the actual CABR-tier inclusion geometry for spatial joins — not `cabr_boundary`. `cabr_boundary` is a provenance label only: every point gets an `inside_nps_boundary` TRUE/FALSE flag, but this never excludes a point from being counted as CABR.

`spatial_utils.R` runs `st_contains(cabr_survey_box, cabr_boundary)` on every load to confirm the box still fully contains the official boundary.

### Known coastal discrepancy

All boundary layers are standardized to EPSG:26946. Three containment checks run in ArcGIS Pro:

| Check | Result |
|---|---|
| `point_loma_boundary` within `sd_county_boundary` | **PASS** (true by construction) |
| `cabr_boundary` within `point_loma_boundary` | **FAIL** |
| `cabr_boundary` within `sd_county_boundary` | **FAIL** |

Both failures occur because `cabr_boundary` (NPS source) extends slightly into the water beyond where the city and county draw the coastline. This is an expected feature of independently-digitized boundaries, not an error. None of the three boundaries have been edited to force containment. If acreage totals don't reconcile exactly across tiers, this coastal overlap is the expected explanation.

`spatial_utils.R` reports all three checks via `message()` (not `warning()`) on every load, since FAIL is the known correct state for two of them.

---

## Forage selectivity — "likes it" vs. "just gets it"

Raw visit counts (the field guides' *Top flowers* / *Top plant*, and any "most-visited" ranking) blend three things: how much a plant was blooming, how heavily it was sampled, and whether the bee actually prefers it. To separate genuine preference from mere availability, `scripts/analysis/forage_selectivity.R` (a shared, single-source module) runs — per bee genus — a **matched** Monte-Carlo chi-square goodness-of-fit test. Rather than compare a genus's visits to the whole-season plant marginal, it compares them to what the **rest of the community recorded in the same (month, year, survey-method) cells the genus appears in** (leave-one-out so an abundant genus can't define its own baseline; weighted by the genus's own distribution across those cells). So the availability baseline is corrected for three confounders at once: **phenology** (a plant blooming when the bee wasn't out can't count against it), **year** (a one-good-year bloom under drought/rain can't masquerade as preference), and **method** (net specimens sample different plants than iNat photos). Thin cells fall back method-preserving: `(year,month,method) → (month,method) → (month) → overall`. A genus is **selective** if it deviates (p < 0.05) and has ≥ 20 plant-visit records; otherwise it's a **generalist** or has **too few records**. Each selective genus's **preferred plant** is the one most over-used relative to that matched availability (highest observed/expected). The plain overall-abundance p-value is kept alongside (`chi_p_abundance`) for comparison. **Observer identity is deliberately not controlled** — it's spread across 10–48 observers per genus (top observer ≤ 33%), so it averages out rather than needing a matching axis or a mixed model.

That one module drives **both** downstream products, so they can never disagree:

- the **interaction-web colors** (`interactions_network.R`, `interactions_web_genus.png` / `_species.png`) — selective genera get a distinct color, generalists / too-sparse genera stay neutral grey; and
- the by-genus field guide's **Forage preference** column (`bee_field_guide_genus.R`).

A per-genus summary — the statistics (both p-values) plus the finding — is written to `data/analysis/interactions/interactions/forage_selectivity_summary.csv` (same `*_summary.csv` convention as the other analyses). Line thickness in the two overview webs encodes each bee's *preference share*; the per-genus focused webs use raw counts. Plant labels are common names (see `plant_names.R`).

**Findings (data as of 2026-08-02): 17 of 31 bee genera are selective, and the set is stable** across every level of control — plain abundance → +month → +year → +method all return essentially the same selective genera. That stability *is* the result: these preferences are robust, not artifacts of when, what year, or how the bees were sampled. What the year control *did* change is some of the **favorites** (the plant a genus most over-uses), because a "favorite" measured against a whole-season average can be a good-year bloom rather than a true preference:

- *Bombus* — recorded most on wild buckwheat (*Eriogonum*); its favorite was milkvetch under month-only control, but against **same-year-and-month** availability it shifts to **deervetch** (*Acmispon*, ~46×). Milkvetch was partly a good-year artifact.
- *Diadasia* — **prickly pear** (*Opuntia*, ~108×), *stronger* under year control; a textbook cactus specialist.
- *Andrena* — **goldfields** (*Lasthenia*, ~23×); *Habropoda* — **sages** (*Salvia*, ~34×); *Anthophora* — **stinkweed** (*Cleomella*, ~17×); *Hylaeus* — **baccharis** (~19×); *Lasioglossum* — **spurges** (*Euphorbia*, ~7×).
- *Halictus* is weakly-but-significantly selective (*Deinandra*, ~2.7×) once flight timing is accounted for — it is *not* the clean generalist the plain abundance test suggested. Clear generalists (visit ≈ in proportion to availability): *Megachile*, *Nomada*.

One honest limit on the *favorite*: the selective *set* is rock-solid, but the single named favorite can wobble for a bee with several strong preferences (Bombus likes both deervetch and milkvetch) — argmax just names the top one. The `forage_selectivity_summary.csv` carries `years_spanned` and `top_year_pct` per genus so a reader can weigh how many years back each finding (e.g. *Diadasia* 99 records / 9 years / 30% max = bulletproof; *Hylaeus* 29 records / 5 years / 59% in one year = real but thinner).

**Residual caveats (stated in the guide and figures too):** "availability" is the community's realized plant *use* per cell (a strong proxy, not an independent bloom census); verdicts near p = 0.05 (e.g. *Dianthidium*) are borderline; and **plant detectability is not controlled** (see the confounder audit and limitations below).

---

## What each analysis controls for (confounders)

The park's sampling is uneven — heavily weighted to one or two survey years (2024 dominates), seasonally skewed (interns survey ~Mar–Sep, "beeple" year-round), ~92% iNaturalist photos vs. ~8% net specimens, and spread across dozens of observers. Those are all confounders. Whether an analysis *needs* to control for them depends on whether it makes an **inferential claim** (something beyond "here is what we recorded") or is **descriptive**. We deliberately do **not** control for confounders in the descriptive analyses — only in the inferential one(s).

**Descriptive analyses** — report what was observed, inherit the sampling biases *by design*, and should be read as "what we saw," not "what is true": the field guides' *Most-recorded flowers* / *Most-used plant*, `interactions_top_plants.R`, the raw-count interaction heatmaps and webs, `bee_bounties.R`, `rare_bee_plants.R`, `records_per_genus_by_evidence.R`, and the coverage maps. These are honest as long as they're labelled descriptively (which is why "Top flowers" became "Most-recorded flowers"). No confounder control is applied or needed.

**Inferential analyses** — make a claim beyond description, so confounders matter:

1. **Forage selectivity** (`forage_selectivity.R` → web colors + *Forage preference* column). Controls for overall abundance, **month, year, and survey method** (the matched-cell chi-square described above). Not controlled: **observer** (spread over many observers → averages out) and **plant detectability** (see below — uncontrollable here). This is the most fully-controlled analysis in the pipeline.

2. **Within-genus niche partitioning — H2′** (`interactions_genus_species_webs.R`). Tests whether a genus's *species* divide up plant genera more than chance. It **no longer uses the plain `r2dtable` null** (which only fixes marginals). Instead it uses a **confounder-aware null: it permutes bee-species labels within (month × method) strata**, so it only calls partitioning "real" if a genus's species split plants *more than their differing flight seasons and sampling methods already explain*. Stratifying by month × method (not also year) is deliberate — a genus's own species overlap in years, so year is a weak within-genus confound and finer strata would leave nothing to permute (power collapses); `n_permutable` is reported per genus so low-power cases are visible. Effect of the control: under the stricter null, **Melissodes and Habropoda drop to non-significant** — their apparent partitioning was largely seasonal — while genuine specialists (Perdita, Diadasia, Anthophora, …) stay significant.

**Sampling-based estimators** — not "preference" tests, but they assume roughly even effort: Chao2 richness (`genera_and_species_accumulation.R`, coverage completeness), rarefaction (`rarefaction_*.R`), diversity indices (`diversity_indices.R`), and the phenology Rayleigh tests. They're standard and defensible, but with effort this uneven (2024-heavy, seasonal) their confidence intervals understate true uncertainty — treat point estimates as approximate.

---

## Known limitations

Like any research pipeline, this one makes trade-offs and carries assumptions that are worth stating explicitly.

**Taxonomy follows iNaturalist, which is a moving target.** The `bee_taxonomy_lookup.csv` is built fresh from the iNat API each run, so genus and species names track iNat's current taxonomy — but iNat itself lags behind the primary literature and occasionally disagrees with other authorities (e.g. ITIS, Discover Life). A taxon reclassification on iNat between runs can silently change which checklist row an observation joins to, or cause a previously-matching specimen name to no longer match.

**The specimen taxonomy spell-check is a heuristic, not an authority.** `specimen_bee_clean.R` flags genus and species names that don't appear in the taxonomy lookup, which catches most typos and some outdated names. It will not catch: names that are still valid iNat taxa but incorrect for the specimen at hand; rank changes that don't alter the genus+species string; or iNat synonyms that haven't been cleaned up yet.

**Genus-level and subgenus-level identifications are kept, not excluded.** The pipeline follows a "possible at CABR" philosophy — an iNat observation identified only to *Lasioglossum* counts as evidence of *Lasioglossum* at CABR. This is intentional (excluding them would lose real data), but it means checklist presence evidence varies in precision: some rows are confirmed to species, others only to genus.

**The combined checklist "Museum Collection" column reflects CABR survey specimens only.** The specimen sheet is the sole source for the X in that column. Bees observed on iNat but not collected are not counted as specimen evidence, even when the identification is unambiguous. This is by design — the column tracks physical specimens in the collection — but it means the column is not a proxy for "seen at CABR."

**Spatial tiers depend on iNat place boundaries, which are user-contributed.** The CABR, Point Loma, and SD County tiers are defined by iNat's place geometries, not by authoritative NPS or government shapefiles (except where `spatial_utils.R` applies the NPS CABR boundary for fine-grained spatial clipping). Minor boundary inconsistencies between iNat places and official shapefiles can cause observations to appear in one tier but not another unexpectedly.

**Output files must be manually deleted before re-running.** `write_fresh()` does not overwrite existing CSVs — if a prior output exists, the new run skips the write and leaves stale data in place. Delete `data/outputs/` contents before each run until this is resolved. (See also: TODO below.)

**Forage "preference" is matched on month/year/method, but two confounders remain.** The selectivity test (`forage_selectivity.R`, driving the *Forage preference* column and the web colors) matches availability to each genus's own month, year and method cells (see "Forage selectivity" and the confounder audit above). What it still can't fix: (1) **plant detectability** — a bee on a big showy flower is far more likely to be photographed than the same bee on a tiny inconspicuous one, and since our availability proxy is itself photo-derived, that bias sits on both sides of the comparison. The clean fix would be an *independent* bloom census (plant-survey phenology), but that prepared plant data does not exist / is not available, so detectability is an acknowledged, uncorrectable limitation here. (2) "Availability" is the community's realized plant *use* per cell, not a true bloom measurement; and verdicts near p = 0.05 (e.g. *Dianthidium*) are borderline. **Observer identity is not controlled but does not need to be** — it's spread across 10–48 observers per genus, so it averages out. See the confounder audit for the one test that still needs work (H2′).

---

## TODO

### Data — iNat bees

- [ ] **iNat observation cleanup** (beeple all years; worst: 2022 & 2025). Note: casual grade and obscured obs get dropped by the R pipeline, so these must be fixed on-platform first.
  1. **Pull + sort (R):** SD County Anthophila + target plants, Quality Grade = Any. Sort by tier (CABR box / Point Loma / SD County), by user (beeple + interns), and by date → rounds (define what a round is first).
  2. **Flag (R) — output 5 lists:** (a) missing tags on round dates, (b) tag ≠ location (spatial-join mismatch), (c) missing IDs (e.g. *Agapostemon* at genus), (d) missing obs fields (bee "visited flower?" / plant "flowering?"), (e) casual pile — set aside, don't fix in R.
  3. **Clean on-platform:** anyone-fixable → assign to beeple (tags, IDs, annotations). Observer-only (geoprivacy, captive flag, missing date/location) → contact observer. Triage casual pile: recoverable via project trust (obscured) / observer nudge (captive) / mark dead (no date or location).
  4. **Write cleaning instructions for beeple.**
  - Involves Jess, Patricia, James Hung, John Ascher. Coordination approach (claimed batches, ID disagreement process) and obs only the original observer can fix are still open questions.
- [ ] **Fix observations placed in the ocean:** some beeple/intern observations have GPS coordinates landing in water. Correct on iNat (original observer or curator) or flag in the pipeline for manual review.

### Data — iNat plants

- [ ] **Plant iNat export:** pull via "Point Loma Peninsula" place (~40k obs, under 200k cap). Add observation fields at download (this was missed last time). Quality grade = Any. **Scope: Point Loma / CABR only** — no SD County plant tier (county-wide plants exceed 200k cap; site level is the correct scope anyway). Don't treat iNat's place as ground truth — clip to own boundary in R via `st_within()`. *(Export pulled; `inat_plant_clean.R` not yet run.)*
- [ ] **Obscured plants:** for your own threatened plants, use `private_latitude`/`private_longitude` columns (true coords). Other people's obscured obs aren't exportable — get via project trust or add known species to checklist by hand. Host-plant ID use case: obscuration doesn't matter (name is visible).
- [ ] Clean plant iNat data
- [ ] Plant checklist
- [ ] Plant phenology

### Bee Specimens

- [ ] **Dr. Doug Yanega (UCR):** (a) 4 specimens to add to the official checklist — Jess says don't add to physical collection, add as "x" (museum specimen record only). (b) Needs to identify 70 *Colletes*, 10 *Hylaeus*, 15 *Perdita*, and 1 *Andrena* to species — requires an in-person trip to UCR.
- [ ] **Physical specimen box audit:** check boxes for duplicate specimens and remove any physical error flags. Identify any unidentified specimens still in box.
- [ ] **Get new SDNHM IDs from Shahan** to replace the 29 sdnhm_ids zeroed out in V13 (duplicate tags that need new labels).
- [ ] Formal specimen deposit to SDNHM (Shahan Derkarabetian)

### Pipeline design

- [ ] **Output files must be manually deleted before re-running.** `write_fresh()` does not overwrite existing CSVs — if a prior output exists, the new run silently skips the write and you get stale data. Delete the relevant files in `data/outputs/` before each run until this is fixed.
- [ ] **Ask Mitchell Nuckols:** does the interactive "did you review tags/fields?" prompt in `inat_bee_clean.R` conflict with how Taro wants to use this? If Taro just wants one button that runs everything and produces outputs (which is what the Rmd suggests), then stopping mid-run for user input breaks that. May need a different approach — e.g. always output the QC files and let the Rmd surface a warning instead of stopping.

### Spatial / infrastructure

- [ ] Spatial join: assign observations to transects using `buffer_10m`
- [ ] **Infer `end_transect` for non-lethal intern surveys:** use iNat obs timestamps + spatial join with transect shapefiles to determine which transect each intern finished on. Update `cabr_bee_survey_dates.csv` once inferred.
- [ ] **Casual-grade observations missing from export:** "Quality Grade = Any" does not appear to include Casual obs in the downloaded CSV (known iNat issue #4186). Need a second export pass with `quality_grade = Casual` and merge with main export.

### Analysis

- [ ] **Reconstruction of bee identifications:** specimens help identify non-IDed iNat obs; iNat helps direct future collecting efforts.
- [ ] **Independent bloom phenology for the availability baseline (refinement).** The selectivity test uses the community's realized plant *use* per cell as the availability proxy. A stronger version would build availability from the survey plant-bloom data (`phenology_activity.R`'s flowering records) so it's an independent bloom census rather than use-derived — this would also be the only real handle on the plant-detectability confound. (Note: as of 2026-08 no prepared plant-bloom dataset is available.)
- [ ] Bee phenology vs. plant phenology
- [ ] iNat vs. specimen / lethal vs. non-lethal comparison — do we find more bees with iNat or specimens?
- [ ] Camera quality comparison (camera vs. phone)
- [ ] Intern vs. beeple comparison
- [ ] Year vs. year comparison
- [ ] 10-minute survey analysis
- [ ] What should we target specifically for future collecting?

### Writing / presentation

- [ ] **Literature review:** read community science projects
- [ ] **Writing:** how many different bee species found at different geographic/taxonomic levels using iNaturalist
- [ ] Presentation

### Future

- [ ] Update methods and use iNat projects

### Done

- [x] iNat observation IDs — *Agapostemon* (genus-only) and *Augochlorella* (mislabeled) resolved
- [x] Gymnosperms covered — plant export uses Plantae/vascular plants, not Angiospermae
- [x] SDNHM museum specimens — asked Shahan Derkarabetian; no relevant holdings to add
- [x] *Andrena cerasifolii* / *Andrena impolita* complex — confirmed both under Complex *Andrena cerasifolii*; no specimens of either in collection currently
- [x] Integrate `read_latest()` into `native_bee_data_analysis.Rmd`
- [x] Add `complex` column to specimen sheet (V9); renamed to bare `complex` pipeline-wide (2026-06-24)
- [x] Point Loma/CABR/SD County boundary shapefiles + CABR survey box
- [x] iNat export → spatial split into three geographic tiers (implemented 2026-06-23)
- [x] TIER 2 merged checklists in Holway format (implemented 2026-06-24)
- [x] Subgenus rank correctly populated for taxa identified directly to subgenus level (fixed 2026-06-25)
- [x] iNat observation-field discovery via API integrated into `inat_bee_clean.R` (2026-07-06)
- [x] **Matched forage-selectivity test (month + year + method)** — `forage_selectivity.R` matches each genus's expected plant use to what the community recorded in the same (month, year, survey-method) cells (leave-one-out, method-preserving fallback); controls phenology, climate-year, and net-vs-photo method. The selective set is stable across all control levels; drives the field-guide *Forage preference* column and web colors; keeps the overall-abundance p for comparison. Confounder audit for the whole pipeline written into the README (2026-08-02)
- [x] **Confounder-aware H2′ null** — `interactions_genus_species_webs.R` replaced the fixed-marginal `r2dtable` null with a permutation of species labels *within month × method strata*, so within-genus niche-partitioning is only called real beyond what flight-season/method differences explain. Under it, Melissodes and Habropoda drop to non-significant; power (`n_permutable`) reported per genus (2026-08-02)

---

*This README will be expanded further as the project progresses.*
