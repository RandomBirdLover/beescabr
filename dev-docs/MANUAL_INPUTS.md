# beescabr — manually maintained inputs

Every file below is **hand-created, hand-edited, or downloaded and dropped in by a person**, then read by the pipeline. Nothing here is produced by a script, so if one goes missing or is named wrong, the pipeline silently uses stale data or skips it. Anything a script writes is *generated* and is listed at the bottom under "Do not hand-edit."

Verified on 2026-08-18 by checking, for each file, that no pipeline script writes it (only reads it).

---

## 1. People, effort & participation

| File | Path | What it is | How it enters | Update trigger |
|---|---|---|---|---|
| `surveyor_roster.csv` | `data/project_info/rosters/` | Canonical people list, one row per person-year (full name, role, method, technique, handle). **Sole authority for "who."** | Hand-typed | Any new intern or beeple |
| `identifier_roster.csv` | `data/project_info/rosters/` | Who identified specimens / photos (the ID-ers). | Hand-typed | New identifier contributes |
| `research_team_roster.csv` | `data/project_info/rosters/research_team_roster/` | Research-team people for the People page (+ `research_team_photos/` headshots alongside). | Hand-typed + image drop | Team changes |
| `master_intern_survey_log.csv` | `data/project_info/survey_date_sources/` | Intern net/photo survey **dates** (the intern survey calendar/log). Effort, not identity. | Hand-typed | Each intern survey trip |
| `master_per_survey_info.csv` | `data/project_info/` | Trip-level effort log, one row per survey. | Hand-typed | Each survey trip |
| Beeple survey calendars | `data/project_info/survey_date_sources/beeple_calendar_windows/` → `YYYY Cabrillo Bee Survey Calendar.pdf` | Annual beeple survey windows, one PDF per year. | Download / export PDF, drop in | Each new season; then re-run the calendar parser |

> The roster (people) and the intern log (dates) are **two separate files that must stay in sync**. New person → edit the roster. New survey date → edit the log. A new intern means editing both.

### How to update participation (people & effort)

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

---

## 2. Specimens

| File | Path | What it is | How it enters | Update trigger |
|---|---|---|---|---|
| `cabr_bee_specimens_record_V{n}_{YYYY_MM_DD}.xlsx` | `data/specimens/records/` | The specimen ID sheet. Newest version = authoritative (picked by date in the filename). | Hand-edited Excel, Save As new version | Any ID / QC / structural change (bump `V{n}`) |
| Deposit / loan tracking files | `data/specimens/records/deposit/`, `data/specimens/records/loans/` | Where individual specimens were sent. | Hand-maintained | Specimens deposited or loaned |

> Log every specimen-sheet change in `dev-docs/SPECIMEN_CHANGELOG.md`.

## 3. Spatial / GIS (source geodata — drawn or downloaded)

| File | Path | What it is | How it enters |
|---|---|---|---|
| `cabr_bee_transects.shp` (+ sidecars) | `data/spatial/transects/` | The survey transect lines. | Drawn in GIS |
| `cabr_survey_access_routes.shp` | `data/spatial/access_routes_to_transects/` | Access routes to the transects. | Drawn in GIS |
| Boundary shapefiles | `data/spatial/boundaries/cabr/`, `.../point_loma/`, `.../san_diego_county/` | Park / peninsula / county boundaries. | Downloaded from authoritative source |
| `transects.csv` | `data/project_info/` | Non-spatial transect attributes (names, lengths, order). | Hand-typed |

## 4. External reference data (downloaded, dropped in)

| File | Path | What it is | How it enters | Note |
|---|---|---|---|---|
| `San Diego County Bee Species Checklist, v3.xlsx` (+ `…README.docx`) | `data/reference/source/holway_2026/` | Holway's authoritative county bee checklist. | Downloaded | Rebuilt into the generated `holway_v3_combined.csv` / `holway_sd_bee_reference_table_v3.csv` by running with `BEESCABR_REBUILD_HOLWAY_REF=1`. When Holway ships a new version, drop it here and rebuild. |
| `iucn_status.csv` | `data/checklists/iucn/` | IUCN conservation status per taxon (drives the "Endangered" badges, e.g. Crotch's bumble bee). | Hand-maintained / downloaded | |
| iNaturalist data | (via API) | Bee + plant observations. | **Pulled via the iNaturalist API**, not a manual file drop | The README's older "download a CSV and drop it in `data/cabr_surveys/nonlethal/inat_bee/`" instructions point at a folder that no longer exists. If you still use manual exports, that path needs fixing. |
| GBIF export | (not in active use) | County bee occurrences. | Optional / future (`rgbif`) | Listed as "optional, for later GBIF work" only. |

## 5. Curated overrides & lookups (hand-edited corrections the pipeline trusts)

| File | Path | What it is |
|---|---|---|
| `manual_taxon_overrides.csv` | `data/reference/curated/` | Hand corrections to taxonomy the automated resolver gets wrong. |
| `specimen_additions.csv` | `data/reference/curated/` | Specimen-only species merged into the taxonomy lookup. |
| `plant_park_confirmed.csv` | `data/reference/curated/` | Plants a botanist confirms are actually in the park. |
| `plant_specimen_overrides.csv` | `data/reference/curated/` | Expert corrections for plants named on specimen labels. |
| `plant_genus_common.csv` | `data/checklists/plants/` | Plant genus → common-name lookup. |
| `verified_taxa.csv` | `data/reference/` | Taxa you have manually verified. |
| `rejected_taxa.csv` | `data/reference/` | Taxa you have manually rejected. |

## 6. iNaturalist QC / review overrides (human-in-the-loop corrections)

These are hand-maintained files the observation pipeline reads as override truth.

| File | Path | What it is |
|---|---|---|
| `qc_review_inat_misid.csv` | `data/inat_observations/review/` | Misidentifications to correct. |
| `qc_review_inat_new_bees_not_on_holway.csv` | `data/inat_observations/review/` | New bees not on the Holway checklist (candidate county additions). |
| `qc_review_inat_mistagged_transects.csv` | `data/inat_observations/review/` | Records tagged to the wrong transect. |
| `qc_review_inat_bee_behavior_survey.csv` / `…_nonsurvey.csv` | `data/inat_observations/review/` | Behavior/foraging corrections (survey vs non-survey). |
| `qc_review_inat_bee_location.csv`, `qc_review_inat_plant_location.csv` | `data/inat_observations/review/location/` | Location fixes for bee / plant observations. |
| `inat_field_id_map.csv` | `data/inat_observations/reference/` | Maps iNaturalist observation-field IDs. |
| `master_crosswalk.csv` | `data/project_info/` | iNaturalist project tags/fields crosswalk (the `project_tags_fields` reference). |

---

## 7. File-naming conventions for the inputs above

### iNat and GBIF exports

```
inat_native_bees_sdcounty_25_mi_buffer_YYYY-MM-DD.csv
inat_plants_point_loma_peninsula_YYYY-MM-DD.csv
gbif_bees_sdcounty_YYYY-MM-DD.csv
```

`inat_native_bees_sdcounty_25_mi_buffer` is one master export covering all SD County bees except *Apis mellifera* (excluded at ingest time via `without_taxon_id` in `scripts/config.R`). "Native" refers only to this honey-bee exclusion; no other non-native species are filtered. `scripts/checklists/` spatially splits the records into three tiers (SD County / Point Loma / CABR) — the three tier checklists are all derived independently from this one file, not nested.

Date = download date, YYYY-MM-DD. Drop directly into `data/cabr_surveys/nonlethal/inat_bee/`. Scripts auto-detect the newest file via `read_latest()`.

### Specimen file

```
cabr_bee_specimens_record_V{n}_{YYYY_MM_DD}.xlsx
```

All versions live in `data/specimens/records/`. The pipeline always reads the newest. See **Specimen version management** below.

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

---

## Do NOT hand-edit (generated — a script writes these)

Listed because they look like inputs but are rebuilt every run. Editing them is wasted work; change the manual source instead.

- `beeple_calendar_windows.csv` — parsed from the calendar PDFs by `scripts/project_info/finding_beeple_calendar.R` (pipeline stage 2d). Edit the PDFs, not this.
- `cabr_official_native_bee_checklist.csv` — written by `scripts/checklists/cabr_bee_checklist.R`.
- `holway_v3_combined.csv`, `holway_sd_bee_reference_table_v3.csv` — built from the Holway `.xlsx` source.
- Everything in `data/reference/generated/`.
- Everything in `data/inat_observations/inat_clean/`.
- `data/specimens/specimens_clean/cabr_specimen_bee_clean.csv`.
- Everything in `data/analysis/**` (the figures, tables, and pages the pipeline produces).

## Documentation drift worth fixing (found while making this list)

- README says `surveyor_roster.csv` lives in `data/project_info/`; it is actually in `data/project_info/rosters/`.
- README's iNat/GBIF section says to drop exports in `data/cabr_surveys/nonlethal/inat_bee/`, a folder that no longer exists (same legacy path we just cleaned out of the specimen docs). Either the API is now the real path, or that instruction needs the correct folder.

---
