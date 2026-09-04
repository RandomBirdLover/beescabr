# Analysis decisions

The **why** behind every figure and test: scope, ranks, method handling,
parameters, and the statistical test. This is the methods reference for a paper.

## Global conventions

These hold everywhere unless the table below overrides them.

| | |
|---|---|
| **Both ranks, always** | Every diversity / accumulation / interaction result runs at genus *and* species |
| **Scope is stated on every figure** | Never mixed silently. `scope_cap()` prints it. |
| **Method routing** | lethal = specimen table (net) · non-lethal = iNaturalist (photo) |
| **Season window** | Year-to-year comparisons clipped to `WINDOW_MONTHS` |
| **Transects** | BST, UPMON, TP, OT. TP is not oversampled — it is walked in both directions. |
| **Specimen coordinates are transect centroids** | ~980 specimens fall on ~18 points, so a specimen map is misleading |
| **Off-transect specimens are not surveys** | 115 of 980. They count toward park totals, but are excluded from per-survey effort. |
| **Show every genus** | No arbitrary top-N in genus figures |
| **On-transect means within 50 m** | `IBC_OFF_TRANSECT_M`. Casual iNaturalist coordinates drift to hundreds of metres, so a tighter buffer would drop real survey records. 7,440 of 11,396 records resolve to a transect. |

## The three fair windows

| Window | Compares | Controls for | Confounded with |
|---|---|---|---|
| `fair_method_2021_2023` (Mar–Oct) | lethal vs non-lethal | year | **observer** — only interns netted, only beeple photographed |
| `fair_observer_2024` (May–Sep) | beeple vs interns | method, year | *nothing — this is the clean one* |
| *(not built)* interns 2021–23 vs 2024 | lethal vs non-lethal | observer | year |

Read the first two together. Neither means much alone.

## Master table

| Analysis (script) | Scope | Ranks | Method | Key parameters | Test / null |
|---|---|---|---|---|---|
| Accumulation by effort (`genera_and_species_accumulation.R`) | survey-effort (iNat survey-only + all specimens) | both | pooled per transect | `PERMUTATIONS=200`, unit = one survey per (day×transect), zero-catch kept, `seed=1` | `vegan::specaccum(random)`; Chao2 via `specpool` (± SE, no p) |
| CABR vs Holway (`coverage_cabr_vs_holway.R`) | all-records (evidence recomputed from cleaned tables) | species | pooled | grades each non-Holway taxon by specimen/iNat/research-grade evidence | descriptive |
| Method Venn + resolution (`coverage_method_venn.R`) | **all-records** | both | **method split** | colors per method | **χ²** (resolution × method) |
| Off-transect bees (`coverage_offtransect.R`) | **all-records** | both | pooled | park vs transect assignment | descriptive |
| Yield by group (`coverage_yield_by_group.R`) | **survey-only** | both | **method split** × beeple/intern | per-survey yield | descriptive |
| Target-ID list (`coverage_id_targets.R`) | all-records | species-resolution flag | pooled | **all genera** (no top-N); per-method figs share one A–Z genus set with `drop=FALSE` | descriptive |
| Diversity indices + ordination (`diversity_indices.R`) | **survey-only** | both | pooled | `WINDOW_MONTHS=3:9`, `MIN_SITE_REC=15` | **PERMANOVA** `adonis2` Bray–Curtis + NMDS; Shannon/Simpson/Pielou |
| Spatial richness map (`spatial_richness_map.R`) | all-records | both | grid: **iNat only** (real GPS); transect table: both methods | `CRS=32611`, `CELL_M=75`, `MAX_ACCURACY=250`, `RAREFY_N=20` | rarefied grid (iNat) + per-transect richness CSV/bar |
| Top plants (`interactions_top_plants.R`) | 3 disclosed: whole-park (headline), survey-only, method | genus (plant) | split shown | `TOP_N=10`, `TOP_MONTH=12` | descriptive |
| Plant–bee network (`interactions_network.R`) | **all-records** | both | pooled | `SPECIALIST_MAX_PLANTS=2`, `MIN_SHARED=3`, H2′ `nsim=999`, NODF `nsimul=499` | **H2′** vs `r2dtable`; **NODF** vs `oecosimu` quasiswap |
| Per-genus species webs (`interactions_genus_species_webs.R`) | all-records (matches network) | within-genus species | pooled | `MIN_SPECIES`, `MIN_REC` thresholds; `seed=1` | within-genus **H2′** vs `r2dtable` |
| Phenology activity (`phenology_activity.R`) | plants **survey-only**; bees **all-records** | both (bees) | pooled (bees) | drop `flower_flowering=="no"` | **Rayleigh** circular test |
| Effort calendar (`phenology_effort.R`) | survey list | — | shown | `WINDOW_MONTHS=3:9` | descriptive |
| Rarefaction — vegan (`rarefaction_vegan.R`) | **survey-only** | both | 4 comparisons incl. method | `WINDOW_MONTHS=3:9`, rarefy-to-lowest | rarefaction (CIs, no p) |
| Rarefaction — iNEXT (`rarefaction_inext.R`) | **survey-only** | both | incl. obs-vs-specimen | `QVALS=0,1,2` (Hill), `NBOOT=50` | size- & coverage-based Hill numbers |
| Bee Bounties (`bee_bounties.R`) | method-gap (all-records) | both | **method split** | **all gap species (no cap)**, `HALF_WIDTH_M=5` (photo corridor) | descriptive gap lists |
| NPS summary (`nps_summary_tables.R`) | descriptive (all) | species | shown | — | plain counts, no test |

---

## Analysis types, and what each owes you

| Type | Claim | Confounders |
|---|---|---|
| **Descriptive** | reports what was observed | inherits sampling bias *by design*; say so |
| **Inferential** | claims something beyond the data | must be controlled and stated |
| **Estimator** | standardises effort | assumes roughly even effort within a group |

## Two decisions that took discussion

**Plant–bee network uses ALL records**, both methods, survey and non-survey.
A survey-only network loses most of the edges, and the question ("who visits
what") is not an effort question. The scope is captioned on the figure.

**Bee phenology uses all records; plant phenology uses survey-only.**
A bee can only look seasonal if somebody was out looking — but plant bloom is
recorded on the survey walk, so restricting it keeps the effort even.
