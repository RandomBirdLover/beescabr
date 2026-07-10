# CABR Bee Specimen Version History

Tracks changes to `CABR_bee_specimens_V{n}_{YYYY-MM-DD}.xlsx` in `data/cabr_surveys/lethal/`.

---

## How versioning works

All versions of the specimen file live together in `data/cabr_surveys/lethal/`. The R pipeline always uses the newest one automatically — no manual archiving needed.

**When you update the specimen file:**
1. Open the current version in Excel
2. Make your changes
3. Save As → `CABR_bee_specimens_V{n+1}_{YYYY-MM-DD}.xlsx` in the same `lethal/` folder
4. Add a row to the Version History table below describing what changed
5. Commit and push `SPECIMEN_CHANGELOG.md`

Do NOT delete or move old versions. Do NOT overwrite an existing version file.

**Version bump rule:** Any deletion or structural change (column added, removed, or renamed; values corrected or reclassified) = new version number. Adding new rows does not require a bump but should be noted here.

**Naming convention:** `CABR_bee_specimens_V{n}_{YYYY-MM-DD}.xlsx` where the date is when that version was finalized.

---

## Deposition tracking

The `Deposition` column in the specimen file tracks where individual specimens have been sent:

- **Deposit** — permanent transfer, specimens will not be returned (e.g., destructive genetic sampling). Tracking files go in `deposit/`.
- **Loan** — temporary transfer, specimens expected back. Tracking files go in `loans/`.

A `complex` column will be added in V9 to support matching against iNat photo observations at the complex rank level.

---

## Version History

| Version | Date | Modified by | Row count | Cols | Changes |
|---------|------|-------------|-----------|------|---------|
| V13 | 2026-07-10 | Brandi Sanchez | 1,437 | 33 | Major data entry update. (1) **New species IDs by Jess Mullins (2026):** 70 specimens identified to species — *Lasioglossum perichlarus* (~26), *L. microlepoides* (~22), *L. incompletum* (6), *L. gaudiale* (5), *L. robustum* (2), *L. petrellum* (2), *L. sisymbrii* (1), *L. microlepoides*, *Halictus tripartitus* (2). Also 1 previously blank *Lasioglossum* row backfilled to genus+subgenus+species. (2) **`needs_ID` and `ask_ID_from` populated** for 223 specimens: Tobi Hays (163), Doug Yanega (96), Chris Wilson (69), unknown (1). (3) **`complex` added** to 29 *Colletes* specimens (→ *Colletes simulans* complex), flagged for Doug Yanega. (4) **SDNHM IDs zeroed** on 29 specimens that need new tags (at least 1 noted "needs new SDNHM_ID tag"). (5) **Sex corrected** on 98 specimens. (6) Column reorder: `other_notes` and `missing_specimen` swapped. No rows added or removed. |
| V12 | 2026-07-07 | Brandi Sanchez | 1,437 | 33 | Added two new columns: `needs_ID` (Y/N flag for specimens awaiting expert identification) and `ask_ID_from` (name of expert to contact). No values populated yet — columns added in preparation for V13 data entry. |
| V11 | 2026-06-25 | Brandi Sanchez | 1,437 | 31 | Backfilled missing `tribe` values for three genera identified in V10 as having blank tribe across some or all of their specimens. All replacement values cross-referenced against Holway's San Diego County Bee Species Checklist v3 to confirm the correct tribe assignment for each genus. No row count or column structure change. Tribes backfilled: Colletes → Colletini (72 specimens, all previously blank); Hylaeus → Hylaeini (12 specimens, all previously blank); Dufourea → Rophitini (1 specimen previously blank — the other 191 Dufourea rows already had Rophitini correctly populated). Total: 85 rows updated. Performed via openpyxl Python script run from `data/cabr_surveys/lethal/`, writing only into blank tribe cells (no overwrites). |
| V10 | 2026-06-24 | Brandi Sanchez | 1,437 | 31 | Taxonomic columns renamed from the `taxon_*_name` convention back to bare rank names (`order`, `family`, `subfamily`, `tribe`, `genus`, `subgenus`, `species`, `subspecies`) and `taxon_complex_name` → `complex` — part of a pipeline-wide naming decision (2026-06-24, see README "Column naming") reversing V9's rename in the other direction. `Matches`, `Correct UCSD_ID`, `Correct SDNHM`, `Merged SDNHM` columns removed (4 cols) — `merged_sdnhm_id`'s values (the corrected IDs) were copied into `sdnhm_id` directly, then all four QC/correction-tracking columns dropped as redundant. |
| V9 | 2026-06-21 | Brandi Sanchez | 1,437 | 35 | All column names converted to snake_case for consistency with the rest of the pipeline. Taxonomic columns (`Order`, `Family`, `Subfamily`, `Tribe`, `Genus`, `Subgenus`, `Species`, `Subspecies`, `Old Genus`, `Old Species`) renamed to the `taxon_*_name` / `old_*_name` convention used in `native_bee_checklist.R`. `SDNHM`, `Correct SDNHM`, `Merged SDNHM` renamed to `sdnhm_id`, `correct_sdnhm_id`, `merged_sdnhm_id`. `Method / Plant` renamed to `method_or_plant`. Added `taxon_complex_name` column, populated via Genus+Species match against `SD_native_bee_checklist.csv` (104 of 1437 specimens matched to a known complex). |
| V8 | 2026-06-08 | Brandi Sanchez | 1,437 | 34 | Added `Missing Specimen` column. Renamed to `CABR_bee_specimens_V8_2026-06-13.xlsx` on 2026-06-13 (first version with formal date attached; rows sorted by UCSD_ID ascending). |
| V7 | 2026-06-08 | Brandi Sanchez | 1,437 | 33 | No structural changes from V6. |
| V6 | 2026-06-05 | Brandi Sanchez | 1,437 | 33 | Added `Old Genus`, `Old Species`, `Subspecies` columns. |
| V5 | 2026-06-02 | Brandi Sanchez | 1,437 | 30 | Renamed `Collection notes` → `Collection Notes`, `Site classification` → `Site Classification`. Added `Determination Year` column. |
| V4 | 2026-06-01 | Brandi Sanchez | 1,437 | 29 | Split `Coordinates` into separate `Latitude` and `Longitude` columns. |
| V3 | 2026-06-01 | Brandi Sanchez | 1,437 | 28 | Added `Merged SDNHM` column. |
| V2 | 2026-05-29 | Brandi Sanchez | 1,437 | 27 | Added `Matches`, `Correct UCSD_ID`, `Correct SDNHM` columns. |
| V1 | 2026-05-04 | Jess Mullins | 1,437 | 24 | Original specimen identification file. Core data, no QC columns. |

**Note:** V0 was an accidental duplicate of V2 and has been deleted.

---

## Known data notes

**Missing genus/species (785 of 1437 as of V9):** Per Jess (2026-06-21), these are not necessarily data entry errors. Possible explanations:
- Some specimens may have been taken by Jess for other purposes
- Some records may never have had a specimen attached (placeholder rows)
- Remaining unidentified specimens may still be physically in boxes at the Holway Lab, UCSD, awaiting identification

This number should not be treated as a cleaning bug — it likely reflects real backlog/uncertainty in physical specimen processing, not a pipeline issue.

---

## Planned Future Deposit

| Deposit | Date | Version submitted | Notes |
|---------|------|-------------------|-------|
| SDNHM (Shahan Derkarabetian) | TBD | TBD | Formal permanent accession — pending |
| Goran Bozinovic | ~2026-06-01 | V8 | *Dufourea australis*, 40 specimens, destructive genetic sampling |