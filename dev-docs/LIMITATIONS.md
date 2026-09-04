# Known limitations

**Read this before quoting a number anywhere permanent.**

## The big ones

| Limitation | What it means |
|---|---|
| **Lethal = intern, non-lethal = beeple (2021–23)** | In those years only interns netted and only beeple photographed. The method comparison cannot separate the two. `fair_observer_2024` is the control — read them together. |
| **Trend is a step, not a slope** | 2024 is a jump in effort, not evidence of a population trend. Transect-year cells are pseudoreplicated (p .018 → .124 once accounted for). |
| **Taxonomy follows iNaturalist** | A moving target. The lookup rebuilds from iNat each run, so a name can change under you. |
| **17 bees have no iNat taxon** | Real bees off the county checklist. They carry no id and no iNat link. Joins must fall back to the name or they vanish. |

## Design assumptions

| | |
|---|---|
| Genus- and subgenus-level IDs are **kept**, not excluded | "possible at this rank" rather than "species or nothing" |
| The specimen spell-check is a **heuristic** | It flags for review; it is not an authority |
| Spatial tiers use **iNat place boundaries** | User-contributed, so edges move |
| "Museum Collection" means **CABR survey specimens only** | Not all SDNHM holdings |

## Data problems

### A script can fix these

`flower_visited` free text → resolved to a plant genus · `taxon_rank` casing ·
transect label variants (TP1/TP2 → TP) · blank `is_survey` / `surveyor_type` ·
date formats · package namespace clashes

### These need a protocol change

| Problem | Scale |
|---|---|
| `flower_flowering` almost never filled | 68 of 9,243 rows |
| Intern practice photos not flagged | can't separate from real surveys |
| Un-attributed on-transect records | ~3,868 with no `surveyor_type` |
| Photos stall above species | *Lasioglossum*, *Ceratina* — a real detection limit |
| Specimen ID backlog | 204 keyable at genus |
| Photo-only taxa with no voucher | 30 species, 8 genera |
| Casual iNat coordinate precision | varies to hundreds of metres |

## Structural confounds — state them, don't try to fix them

- All lethal records are intern-collected
- Netting stopped after 2023, so no year has interns doing both methods
- Effort is Mar–Oct heavy and 2024-heavy
