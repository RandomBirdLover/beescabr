# Analysis roadmap

The stakeholders' questions, triaged. **Mostly built** — kept for the reasoning
behind what was and was not attempted.

Their numbering runs 1–8 then 10–13. There is no #9.

| # | Question (restated) | Category | Status | Key inputs |
|---|---|---|---|---|
| 1 | Which bees are in lethal vs non-lethal, and where don't they overlap? (Venn) | `coverage` | ✅ done | cleaned tables, both ranks |
| 2 | What taxonomic resolution can each method reach? (survey only) | `coverage` | 📋 next | `taxon_rank` × method, survey rows |
| 3 | Top 10 plants bees visit — overall & per month | `interactions` | ✅ done | `plant_genus` (resolved) |
| 4 | Plant→bee visitation network (find specialist/rare-bee links) | `interactions` | ✅ done | `plant_genus` + bee species/genus |
| 5 | Have we found all Cabrillo's species? (accumulation curve) | `completeness` | ✅ done | survey × taxa |
| 6 | Bees in the park but not on transects — do we need more transects? | `coverage` | 📋 | `is_survey` FALSE vs TRUE, `transect`, `url` |
| 7 | What needs ID most often? (target identification) | `coverage` | 📋 | coarse `taxon_rank` counts by taxon |
| 8 | Diversity across years (genus→species), by month, spatially | `diversity` + `phenology` | 📋 | cleaned bee tables + coords + dates |
| 10 | Which CABR-checklist taxa aren't on the Holway checklist? | `coverage` | ✅ done | official checklist `holway` flag (**11 taxa**) |
| 11 | Which surveys caught more (by species): beeple vs intern, lethal vs non-lethal? | `coverage` | 📋 | per-survey info + records (confounded) |
| 12 | Plant phenology from surveys (non-lethal only) | `phenology` | 📋 | plant clean table, flowering by month |
| 13 | #Observations / #trips by month | `phenology` | 📋 | per-survey info + record counts |
| + | Possible species at CABR — max / min / median | `completeness` | 📋 | richness estimators (Chao/Jackknife range) |

## Global caveats

These touch nearly every question. Full detail in `LIMITATIONS.md`.

| | |
|---|---|
| Effort is uneven | Mar–Oct heavy, 2024 heavy |
| Method and observer are confounded before 2024 | only interns netted, only beeple photographed |
| Photos stall above species | for some genera this is a real detection limit |
| "Availability" is the realised plant community | not what was truly on offer |

## Build order

```
  completeness  ->  coverage  ->  interactions  ->  phenology  ->  diversity
  (is the list      (what are      (who visits      (when)         (how much,
   complete?)        we missing?)   what?)                          and where)
```

## Parked

**Find the bee by its plant's flowering season** — a synthesis of #4 and #12.
Proposed, not built: it needs plant phenology to be solid first, and
`flower_flowering` is filled on 68 of 9,243 rows.

**Publications** — scoping only, no commitments made.
