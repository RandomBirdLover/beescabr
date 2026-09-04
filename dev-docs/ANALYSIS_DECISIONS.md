# Analysis decisions & parameters

The **why** behind every native-bee analysis: for each graph/test we record its
**scope**, the **ranks** it runs at, how **method** is handled, the **key
parameters** (and their values), the **statistical test + null model**, and the
reasoning for each choice. This is the methods reference for the papers.

Companions: the generated `findings_index.csv` (the output catalog) and
`ANALYSIS_ROADMAP.md` (the stakeholder-question triage), both in this folder. When a parameter
changes in a script, update it here too.

---

## Global conventions (apply to every analysis unless the table overrides them)

**Both ranks, always.** Every diversity / accumulation / interaction result is
produced at two ranks, and the filenames carry the rank:

- **genus rank** — counts any record that pins a genus: `taxon_rank ∈
  {species, subspecies, subgenus, complex, genus}` with a non-empty `genus`.
  Robust, because every genus-resolved record counts.
- **species rank** — counts species-level IDs only: `taxon_rank ∈
  {species, subspecies}` with a non-empty species epithet (subspecies rolled up
  to species). Finer, but smaller.

*Why:* not every bee is identifiable to species (especially photos), so a
species-only view silently drops data and understates the coarser signal. Genus
is the honest robust view; species is the finer view where it's valid.

**Scope — stated on every figure, never mixed silently.**

- **Survey-only** (`is_survey == TRUE`) for anything about **effort, abundance,
  composition, diversity, or yield** — these must be tied to standardized survey
  events.
- **All-records** for **coverage, "what's in the park," maps, method reach, and
  the visitation network** — these are about total knowledge, not per-survey
  effort.

**Method routing.** Lethal = specimen table (net); non-lethal = iNaturalist
table (photo). Methods are **pooled** unless the comparison *is* the method
split (Venn, resolution χ², yield-by-group, bounties).

**Season window.** Year-to-year and seasonal comparisons are clipped to the
intern window **March–September** (`WINDOW_MONTHS <- 3:9`); beeple record
year-round, so leaving it open would confound "season" with "who surveyed."

**Transects: BST, UPMON, TP, OT.** TP is **not** oversampled — it is walked in
halves (TP1/TP2), one half per trip, logged as one `TP` survey-event, the same
unit as the other transects.

**Specimen coordinates are transect centroids.** ~980 specimens fall on ~18
points, so for specimens the **transect is the reliable spatial unit, not raw
lat/long**; only iNaturalist has genuine GPS scatter. Any specimen spatial
analysis aggregates to transect.

**Off-transect specimens — labeled not-survey.** 115 of 980 specimens (S O'Dell's
targeted off-transect netting, 114 of them from 2021) have coordinates but no
transect. As of the specimen-clean fix they are marked `is_survey = FALSE`: they
**count toward park totals** (all-records analyses) but are **excluded from
survey-only and per-transect analyses**. A survey specimen must sit on a transect.
Re-run the pipeline's specimen stage to regenerate `cabr_specimen_bee_clean_generated.csv`
with this labeling, then re-run the analyses.

**Show every genus — no arbitrary top-N.** Genus-level figures display **all
named genera** (31 at present), not a "top N." A genus is omitted only when it
fails a stated **record threshold** tied to the analysis: phenology needs ≥10
records to draw a ridge, per-genus species webs need ≥2 species and ≥20 records,
NMDS/PERMANOVA needs ≥15 records/site. The **Bee Bounties** are the *complete*
gap lists — every species found in one method but not the other (**no cap**),
since a bounty is only useful if exhaustive. The one remaining ranked shortlist
is **Top-plants** (top-10 **plant** genera), a most-visited headline where the
cap is deliberate.
In paired per-method figures, a genus with zero records for one method still
holds its row (`scale_y_discrete(drop = FALSE)`) so the panels line up
genus-for-genus (e.g. photo-only *Xenoglossa* shows an empty specimen row until
its net specimens are entered).

---

## Master table

| Analysis (script) | Q | Scope | Ranks | Method | Key parameters | Test / null |
|---|---|---|---|---|---|---|
| Accumulation by effort (`genera_and_species_accumulation.R`) | Q5 | survey-effort (iNat survey-only + all specimens) | both | pooled per transect | `PERMUTATIONS=200`, unit = one survey per (day×transect), zero-catch kept, `seed=1` | `vegan::specaccum(random)`; Chao2 via `specpool` (± SE, no p) |
| CABR vs Holway (`coverage_cabr_vs_holway.R`) | Q10 | all-records (evidence recomputed from cleaned tables) | species | pooled | grades each non-Holway taxon by specimen/iNat/research-grade evidence | descriptive |
| Method Venn + resolution (`coverage_method_venn.R`) | Q1/Q2 | **all-records** | both | **method split** | colors per method | **χ²** (resolution × method) |
| Off-transect bees (`coverage_offtransect.R`) | Q6 | **all-records** | both | pooled | park vs transect assignment | descriptive |
| Yield by group (`coverage_yield_by_group.R`) | Q11 | **survey-only** | both | **method split** × beeple/intern | per-survey yield | descriptive |
| Target-ID list (`coverage_id_targets.R`) | Q7 | all-records | species-resolution flag | pooled | **all genera** (no top-N); per-method figs share one A–Z genus set with `drop=FALSE` | descriptive |
| Diversity indices + ordination (`diversity_indices.R`) | Q8 | **survey-only** | both | pooled | `WINDOW_MONTHS=3:9`, `MIN_SITE_REC=15` | **PERMANOVA** `adonis2` Bray–Curtis + NMDS; Shannon/Simpson/Pielou |
| Spatial richness map (`spatial_richness_map.R`) | Q8 | all-records | both | grid: **iNat only** (real GPS); transect table: both methods | `CRS=32611`, `CELL_M=75`, `MAX_ACCURACY=250`, `RAREFY_N=20` | rarefied grid (iNat) + per-transect richness CSV/bar |
| Top plants (`interactions_top_plants.R`) | Q3 | 3 disclosed: whole-park (headline), survey-only, method | genus (plant) | split shown | `TOP_N=10`, `TOP_MONTH=12` | descriptive |
| Plant–bee network (`interactions_network.R`) | Q4 | **all-records** | both | pooled | `SPECIALIST_MAX_PLANTS=2`, `MIN_SHARED=3`, H2′ `nsim=999`, NODF `nsimul=499` | **H2′** vs `r2dtable`; **NODF** vs `oecosimu` quasiswap |
| Per-genus species webs (`interactions_genus_species_webs.R`) | Q4+ | all-records (matches network) | within-genus species | pooled | `MIN_SPECIES`, `MIN_REC` thresholds; `seed=1` | within-genus **H2′** vs `r2dtable` |
| Phenology activity (`phenology_activity.R`) | Q12 | plants **survey-only**; bees **all-records** | both (bees) | pooled (bees) | drop `flower_flowering=="no"` | **Rayleigh** circular test |
| Effort calendar (`phenology_effort.R`) | Q13 | survey list | — | shown | `WINDOW_MONTHS=3:9` | descriptive |
| Rarefaction — vegan (`rarefaction_vegan.R`) | — | **survey-only** | both | 4 comparisons incl. method | `WINDOW_MONTHS=3:9`, rarefy-to-lowest | rarefaction (CIs, no p) |
| Rarefaction — iNEXT (`rarefaction_inext.R`) | — | **survey-only** | both | incl. obs-vs-specimen | `QVALS=0,1,2` (Hill), `NBOOT=50` | size- & coverage-based Hill numbers |
| Bee Bounties (`bee_bounties.R`) | — | method-gap (all-records) | both | **method split** | **all gap species (no cap)**, `HALF_WIDTH_M=5` (photo corridor) | descriptive gap lists |
| NPS summary (`nps_summary_tables.R`) | — | descriptive (all) | species | shown | — | plain counts, no test |

---

## Scope choices that took discussion (the reasoning, in full)

### Plant–bee network — **all-records** (both methods, survey + non-survey)
The network's "plant" is the flower recorded **on each bee observation**
(`plant_genus`, resolved as part of bee cleaning), **not** the standalone plant
survey table. Because every bee in the park was identified and had its flower
added on iNaturalist, the flower annotations are clean park-wide — so using all
bee records (not just survey ones) is consistent with the data, and pulls in the
non-survey observations that still carry a real bee×plant interaction. The
"plants weren't cleaned park-wide" caveat applies only to the separate plant
table, which this analysis never touches.

Verified both ways against the data — the choice does not move the conclusions:

| | records | plant genera | bee taxa | connectance | H2′ |
|---|---|---|---|---|---|
| genus, all-records | 9,457 | 84 | 31 | 0.17 | **0.292** |
| genus, survey-only | 7,832 | 78 | 29 | 0.18 | **0.311** |
| species, all-records | 5,274 | 69 | 74 | 0.086 | **0.434** |
| species, survey-only | 4,391 | 63 | 69 | 0.092 | **0.457** |

H2′ shifts by ~0.02 either way; the published 0.29 (genus) / 0.43 (species) hold.
**Decision: keep all-records**, and caption the scope on the figures.

### Bee phenology — bees **all-records**, plants **survey-only**
Deliberately asymmetric, for a data-cleaning reason:

- **Bees** were identified and had flowers added **park-wide** on iNaturalist, so
  every bee record is trustworthy → no survey filter (more records, better
  seasonal curves).
- **Plants** were **not** cleaned park-wide, so only **survey** plant records are
  reliable → filter to `is_survey == TRUE`, and additionally drop the handful of
  records explicitly marked `flower_flowering == "no"`. By protocol a plant is
  logged only when in flower, so survey plant records = the **flowering** signal
  (bloom timing, not year-round presence). `flower_flowering` is ~99% blank, so
  it is used only to remove explicit "no" rows, never as the positive filter.

Verified both ways — including non-survey bees changes **no** seasonality verdict:

| rank | all-records significant | survey-only significant | verdict flips |
|---|---|---|---|
| genus | 24/24 | 23/23 | **0** |
| species | 34/34 | 29/29 | **0** |

(All-records makes a few more taxa meet the n≥10 test threshold; every taxon
tested in both scopes agrees.) **Decision: keep bees all-records, plants
survey-only.**

---

## Statistics & null models (one line each)

- **χ²** — resolution (species-level vs coarser) × method (net vs photo).
- **Chao2** — incidence richness estimate from singletons/doubletons; reported ± SE, no p-value.
- **PERMANOVA** (`adonis2`, Bray–Curtis) + **NMDS** — composition by transect + year, survey-only, Mar–Sep.
- **H2′ specialization** (Blüthgen 2006) — self-contained; null = `r2dtable` (Patefield, fixed marginals); one-sided (more specialized than chance).
- **NODF nestedness** — `vegan::oecosimu`, quasiswap null, **one-sided** (`alternative = "greater"`, matching H2′). Reconciled — was two-sided.
- **Rayleigh** — circular uniformity per taxon; seasonal if p < 0.05.
- **Rarefaction** — vegan rarefy-to-lowest + iNEXT Hill q0/q1/q2 (size- and coverage-based); CIs, no p-value.
- Descriptive analyses (Venn, off-transect, id-targets, top-plants, bounties, NPS) legitimately carry no p-value.

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

---

## What each analysis controls for (confounders)

The park's sampling is uneven — heavily weighted to one or two survey years (2024 dominates), seasonally skewed (interns survey ~Mar–Sep, "beeple" year-round), ~92% iNaturalist photos vs. ~8% net specimens, and spread across dozens of observers. Those are all confounders. Whether an analysis *needs* to control for them depends on whether it makes an **inferential claim** (something beyond "here is what we recorded") or is **descriptive**. We deliberately do **not** control for confounders in the descriptive analyses — only in the inferential one(s).

**Descriptive analyses** — report what was observed, inherit the sampling biases *by design*, and should be read as "what we saw," not "what is true": the field guides' *Most-recorded flowers* / *Most-used plant*, `interactions_top_plants.R`, the raw-count interaction heatmaps and webs, `bee_bounties.R`, `rare_bee_plants.R`, `records_per_genus_by_evidence.R`, and the coverage maps. These are honest as long as they're labelled descriptively (which is why "Top flowers" became "Most-recorded flowers"). No confounder control is applied or needed.

**Inferential analyses** — make a claim beyond description, so confounders matter:

1. **Forage selectivity** (`forage_selectivity.R` → web colors + *Forage preference* column). Controls for overall abundance, **month, year, and survey method** (the matched-cell chi-square described above). Not controlled: **observer** (spread over many observers → averages out) and **plant detectability** (see below — uncontrollable here). This is the most fully-controlled analysis in the pipeline.

2. **Within-genus niche partitioning — H2′** (`interactions_genus_species_webs.R`). Tests whether a genus's *species* divide up plant genera more than chance. It **no longer uses the plain `r2dtable` null** (which only fixes marginals). Instead it uses a **confounder-aware null: it permutes bee-species labels within (month × method) strata**, so it only calls partitioning "real" if a genus's species split plants *more than their differing flight seasons and sampling methods already explain*. Stratifying by month × method (not also year) is deliberate — a genus's own species overlap in years, so year is a weak within-genus confound and finer strata would leave nothing to permute (power collapses); `n_permutable` is reported per genus so low-power cases are visible. Effect of the control: under the stricter null, **Melissodes and Habropoda drop to non-significant** — their apparent partitioning was largely seasonal — while genuine specialists (Perdita, Diadasia, Anthophora, …) stay significant.

**Sampling-based estimators** — not "preference" tests, but they assume roughly even effort: Chao2 richness (`genera_and_species_accumulation.R`, coverage completeness), rarefaction (`rarefaction_*.R`), diversity indices (`diversity_indices.R`), and the phenology Rayleigh tests. They're standard and defensible, but with effort this uneven (2024-heavy, seasonal) their confidence intervals understate true uncertainty — treat point estimates as approximate.

---

---

## Spatial analysis

Moved to **`ARCGIS_SPATIAL_MAPPING.md`** (this folder), which now holds all layer
provenance, the coastal discrepancy, and how the layers are used.

---

## Pending parameter notes

- **Spatial richness map — rebuilt.** The fine `CELL_M=75` grid now uses
  **iNaturalist only** (real GPS); specimens (transect centroids) are summarised
  in a companion **per-transect richness** table + bar chart
  (`transect_richness.csv`, `transect_richness.png`), both methods pooled.
- **NODF tail** (network) — resolved: one-sided (`alternative = "greater"`), consistent with H2′.
- **iNEXT `NBOOT=50`** — fine for exploration; raise to ≥100 for the final
  publication figures.

---
