# CABR Specimen Version History

Tracks changes to `CABR_specimens_V{n}_{YYYY-MM-DD}.xlsx` in `data/cabr_surveys/lethal/`.

**Version bump rule:** Any deletion or structural change (column added, removed, or renamed; values corrected or reclassified) triggers a new version number. Adding new rows or reordering existing rows does not require a version bump.

**Naming convention:** `CABR_specimens_V{n}_{YYYY-MM-DD}.xlsx` where the date is when that version was finalized.

---

## Folder structure

```
data/cabr_surveys/lethal/
  CABR_specimens_V{n}_{YYYY-MM-DD}.xlsx   ← authoritative survey data
  deposit/
    CABR_deposit_{taxon}_{recipient}.xlsx  ← permanent transfers (destructive sampling, etc.)
  loans/
    CABR_loan_{taxon}_{recipient}.xlsx     ← temporary transfers, expected back
```

---

## Deposition tracking

The `Deposition` column in the main specimens file tracks where individual specimens have been sent. Two types:

- **Deposit** — permanent transfer, specimens will not be returned (e.g., destructive genetic sampling). Tracking files go in `deposit/`.
- **Loan** — temporary transfer, specimens expected back. Tracking files go in `loans/`.

---

## Version History

| Version | Date | UCSD_ID range | Row count | Changes |
|---------|------|---------------|-----------|---------|
| V8 | 2026-06-13 | 1–1437 | 1,437 | First version with formal date attached. Rows sorted by UCSD_ID ascending (cosmetic correction, not a data change). All 40 *Dufourea australis* specimens deposited to Goran Bozinovic for destructive genetic sampling — marked in Deposition column. Deposit tracking file: `deposit/CABR_deposit_Dufourea_GoranBozinovic.xlsx`. SDNHM formal deposit pending. Full change history from V1–V7 pending reconstruction (see below). |
| V7 | TBD | TBD | TBD | Pending reconstruction from work computer |
| V6 | TBD | TBD | TBD | Pending reconstruction from work computer |
| V5 | TBD | TBD | TBD | Pending reconstruction from work computer |
| V4 | TBD | TBD | TBD | Pending reconstruction from work computer |
| V3 | TBD | TBD | TBD | Pending reconstruction from work computer |
| V2 | TBD | TBD | TBD | Pending reconstruction from work computer |
| V1 | TBD | TBD | TBD | Pending reconstruction from work computer |

---

## Monday TODO — Reconstruct V1–V7 History

Access old versions on the work computer and for each:

- [ ] Check file modified date (use as proxy for version date if no other record)
- [ ] Note UCSD_ID range and row count
- [ ] Compare column structure to adjacent version — note any added/removed/renamed columns
- [ ] Note any obvious deletions (row count differences)
- [ ] Note any major reclassifications or ID corrections if documented in the file or in email
- [ ] Fill in the table above

Once reconstructed, this file should be committed to GitHub so the history is preserved alongside the pipeline code. The specimen `.xlsx` files themselves remain in `data/` (gitignored).

---

## Planned Future Deposit

| Deposit | Date | Version submitted | Notes |
|---------|------|-------------------|-------|
| SDNHM (Pam Horsley) | TBD | TBD | Formal permanent accession — pending |
| Goran Bozinovic | ~Jun 1, 2026 | V8 | *Dufourea australis*, 40 specimens, destructive genetic sampling |
