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
| The **plant–bee network uses ALL records** | Both methods, survey and non-survey. A survey-only network loses most of its edges, and "who visits what" is not an effort question. |
| **Bee** phenology uses all records; **plant** phenology is survey-only | A bee only looks seasonal if somebody was out looking. Plant bloom is recorded on the survey walk, so restricting it keeps the effort even. |

Every figure states its own scope in the caption. These two are here because the
*reason* for the scope is not obvious from the scope itself.

## Data problems

### A script can fix these

`flower_visited` free text → resolved to a plant genus · `taxon_rank` casing ·
transect label variants (TP1/TP2 → TP) · blank `is_survey` / `surveyor_type` ·
date formats · package namespace clashes

### These need a protocol change

| Problem | Scale |
|---|---|
| `flower_flowering` almost never filled | 68 of 9,243 rows |
| `is_10min` never filled | 0 of 11,396 rows — record it and the 10-minute survey becomes analysable |
| No camera / device recorded | camera-vs-phone quality is not answerable from these records |
| Intern practice photos not flagged | can't separate from real surveys |
| Un-attributed on-transect records | ~3,868 with no `surveyor_type` |
| Photos stall above species | *Lasioglossum*, *Ceratina* — a real detection limit |
| Specimen ID backlog | 204 keyable at genus |
| Photo-only taxa with no voucher | 30 species, 8 genera |
| Casual iNat coordinate precision | varies to hundreds of metres |

## The plants are a bee project's by-catch, not a plant project

Plant records exist to say what a bee was on. They were never collected as a
plant survey, and three limits follow from that:

| | |
|---|---|
| **No plant checklist** | only a genus → common-name map. Bees have tiered checklists; plants do not. |
| **Phenology cannot be computed** | `flower_flowering` is filled on 68 of 9,243 rows, so bloom timing is not measurable from this data |
| **Availability is use-derived** | the selectivity test uses what the bee community was *recorded on* as the proxy for what was in bloom. That is circular, and it is the honest limit of by-catch data. |

**A plant project of its own would fix all three**, and is the right place to
build real phenology: a plant-side cleaning pipeline, bloom recorded on the
survey walk whether or not a bee was on it, and its own checklist. Threatened
plants would use `private_latitude` / `private_longitude` under the project's own
account, which is also the more secure route — obscured coordinates stay
obscured to everyone else.

Until then, treat every plant result here as *what bees were seen on*, never as
*what was available*.

## Structural confounds — state them, don't try to fix them

- All lethal records are intern-collected
- Netting stopped after 2023, so no year has interns doing both methods
- Effort is Mar–Oct heavy and 2024-heavy
