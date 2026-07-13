# Verification & the SD bee taxonomy lookup

Plain-English notes on the verification workflow and the taxonomy tables it
uses. (Added 2026-07.)

## What "verification" means here

When a bee turns up on iNaturalist whose **genus, subgenus, complex, species,
or subspecies is not in the Holway reference**, it's something we haven't seen
before, and a human needs to check that the iNaturalist photo/ID is actually
correct. Holway is the trusted baseline; anything beyond it is flagged until
you verify it. Verification is **manual** — the pipeline just surfaces what
needs a look and remembers what you've cleared.

## Renamed files

- `bee_taxonomy_lookup.csv` → **`sd_bee_taxonomy_lookup.csv`** (Holway + iNat)
- `holway_reference_checklist.csv` → **`holway_sd_bee_reference_table.csv`**
  (Holway only, from the occasional interactive builder)

## `sd_bee_taxonomy_lookup.csv`

- Now includes **iNat-observed species**, not just Holway's list.
- Keeps Holway's **Tentative** and **Unpublished** rows; the `holway_status`
  column shows `Described` / `Tentative` / `Unpublished` (blank = not from
  Holway, i.e. an iNat-only name).
- `verified` column: TRUE if the taxon is in Holway **or** you've verified it.
- Column order: `taxon_id, scientific_name, rank, verified, holway_status`,
  then the taxonomic hierarchy kingdom → phylum → class → order → superfamily
  → family → subfamily → tribe → subtribe → genus → subgenus → complex →
  species → subspecies, then `complex_taxon_id, common_name`.
- **To find iNat-only species:** filter where `holway_status` is blank and
  `taxon_id` is filled. (Clear `in_holway` / `in_inat` yes/no columns can be
  added if wanted.)

## Flagging in `inat_bee_clean.R`

- `needs_verification` = TRUE when any of genus/subgenus/complex/species/
  subspecies is new to Holway and not yet verified.
- `new_at_rank` = which level(s) are new (e.g. "genus,species").
- `cabr_inat_to_verify.csv` (in `data/outputs/inat_clean/qc/`) = just the
  flagged observations — your worklist.

## Recording your checks

`data/outputs/reference/verified_taxa.csv` — a simple list you maintain.
Columns: `taxon_id, verified`. Once you've confirmed a bee's photo/ID, add its
`taxon_id` (verified = `Y`). It stops being flagged on the next run.

## "Updated since last time" refresh

The ingest now also re-pulls observations that were **re-identified or edited
on iNaturalist since the last run** (not just brand-new ones), so a scientist
fixing an ID flows back into your data. The cutoff time is stored in
`data/cache/last_ingest.txt`. To re-pull everything from scratch:
`BEESCABR_FULL_INGEST=1`.

## Run order (RStudio)

1. `run_pipeline.R` — ingest + checklists; builds `sd_bee_taxonomy_lookup.csv`
   and refreshes the cache.
2. The clean step → `cabr_inat_bee_clean.csv` + `cabr_inat_to_verify.csv`.
3. Check the photos in the to-verify list; add confirmed `taxon_id`s to
   `verified_taxa.csv`; re-run — they drop off the flag list.
