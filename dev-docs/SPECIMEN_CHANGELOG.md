# Specimen record — version history

File: `data/specimens/records/cabr_bee_specimens_record_V{n}_{YYYY_MM_DD}.xlsx`

**The pipeline reads the newest version.** To change anything, save a new version
with the next number and today's date. Never edit an old one.

## Versions

`Genus` / `Species` = rows identified to that rank. Both only ever go up; that is
the identification backlog closing.

| V | Date | Rows | Genus | Species | What changed |
|---|---|---|---|---|---|
| **19** | 2026-09-02 | 1,145 | 959 | 935 | **Current.** 4 determination years, 1 subspecies |
| 18 | 2026-08-18 | 1,145 | 959 | 935 | Re-save, same day as V17. No data change |
| 17 | 2026-08-18 | 1,145 | 959 | 935 | **+87 species**; 745 `needs_ID` flags cleared; 1 row removed |
| 16 | 2026-08-03 | 1,146 | 959 | 848 | 2 dates, 1 method |
| 15 | 2026-07-28 | 1,146 | 959 | 848 | +69 species, 69 subgenera |
| 14 | 2026-07-20 | 1,146 | 959 | 779 | **Correction pass** — 291 rows removed, **94 dates corrected**, dates converted to ISO text, `missing_specimen` retired |
| 13 | 2026-07-10 | 1,437 | 959 | 779 | +50 genera, +59 species; 223 rows flagged `needs_ID` |
| 12 | 2026-07-07 | 1,437 | 909 | 720 | **+`needs_ID`, `ask_ID_from`** — the ID backlog becomes trackable. +68 species |
| 11 | 2026-06-25 | 1,437 | 909 | 652 | 85 tribes corrected |
| 10 | 2026-06-24 | 1,437 | 909 | 652 | `taxon_*_name` → plain rank names; reconciliation columns retired; 461 SDNHM ids revised |
| 9 | 2026-06-21 | 1,437 | 909 | 652 | **Schema rename** — every column to snake_case |
| 8 | 2026-06-13 | 1,437 | 909 | 652 | **+`Missing Specimen`** — the Dufourea deposit is tracked from here |
| 7 | 2026-06-08 | 1,437 | 909 | 652 | 2 longitudes corrected |
| 6 | 2026-06-05 | 1,437 | 909 | 652 | **Biggest ID pass: +282 genera, +101 species.** +Subspecies / Old Genus / Old Species; `count` filled on all 1,437; 86 coordinates fixed |
| 5 | 2026-06-02 | 1,437 | 627 | 551 | +Determination Year; 191 determinations and 295 order/family values revised; 4 coordinates fixed |
| 4 | 2026-06-01 | 1,437 | 617 | 537 | **Coordinates split into Latitude + Longitude**; 147 collectors corrected |
| 3 | 2026-06-01 | 1,437 | 617 | 537 | +Merged SDNHM column. No data changed |
| 2 | 2026-05-29 | 1,437 | 617 | 537 | +Matches / Correct UCSD_ID / Correct SDNHM reconciliation columns; 60 SDNHM ids corrected |
| 1 | 2026-05-04 | 1,437 | 617 | 537 | Original identification file, from Jess Mullins |

**Net over 19 versions:** 1,437 → 1,145 rows, and species identifications 537 → 935.

V1 came from museum identification; every version after it is Brandi Sanchez's.

## Two things the numbers hide

**A version with "no data change" is not a wasted save.** V3, V8 and V18 changed
only structure or nothing at all. They exist because the rule above is "never
edit an old one" — a re-save is the cheap, correct move.

**Excel churns cells that nobody edited.** A saved sheet rewrites coordinates as
`32.671936000000002` and dates as serial numbers, so a naive diff of V14 reports
1,145 changed dates. Only 94 dates actually differ. The counts in the table
compare values, not spellings.

## Deposits

| Where | When | From | What |
|---|---|---|---|
| Goran Bozinovic | ~2026-06-01 | V8 | *Dufourea australis*, 40 specimens, destructive genetic sampling |
| SDNHM | pending | — | Formal permanent accession |

The Dufourea deposit sheet lives in `records/deposit/` and mirrors the main
record. Its taxonomy and dates are kept in sync by hand.
