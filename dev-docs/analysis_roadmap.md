# Analysis roadmap — stakeholder questions, triaged

Turns the stakeholders' question dump into a buildable plan. Each question is
restated, tagged with a catalog category (see `data/analysis/README.md`), checked
for feasibility against the **actual** cleaned data, and annotated where the
concept or the inputs need a second look. Duplicates are merged; #5 is already
built.

Feasibility key: ✅ ready with current data · ⚠️ needs a prep step first ·
❗ concept/definition to confirm before building.

---

## Global caveats (these touch almost every question)

- **Survey season window.** Interns only surveyed ~**March/April–September**, so
  any month-to-month or seasonal comparison must be restricted to that window (or
  it will read a sampling gap as a real absence). This especially bites
  **beeple vs intern** and **lethal vs non-lethal** comparisons, because beeple
  (citizen) records run year-round while intern/lethal records don't.
- **Detection bias by method.** Photos (non-lethal) systematically under-detect
  small/cryptic bees — the stakeholders' *Lasioglossum* (Dialictus) example is
  exactly right: nets (lethal) catch and key them, cameras can't. So "found by one
  method only" is often a **detection** story, not purely a "sample here more" one.
  The data bears this out: lethal records reach **species 79%** of the time
  (779/983) vs non-lethal **~52%** (3,730/7,247 survey records) — see #2.
- **Survey-only vs whole-park.** Most questions want `is_survey == TRUE`. #6 is the
  deliberate exception (it asks about the non-transect remainder). State the filter
  every time.
- **Confounds to name up front.** All lethal records are `surveyor_type == "intern"`;
  the OT transect is non-lethal only; so "lethal vs non-lethal" and "intern vs
  beeple" are partly the same axis. Report them, don't pretend they're independent.
- **Intern training photos.** Decide whether to exclude interns' training-period
  photos (flagged as a to-confirm data step) so practice IDs don't inflate counts.

---

## Master triage

| # | Question (restated) | Category | Feasible | Key inputs |
|---|---|---|---|---|
Numbers are the **stakeholders' official question numbers** (their list runs 1–8 then
10–13 — there is **no #9**).

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

---

## By category

### `completeness` — have we found everything?

**#5 (done → extend).** The per-transect accumulation + Chao2 is built
(`genera_and_species_accumulation.R`). To answer the park-level version literally
("all species at Cabrillo?"), add a **pooled park-level** curve (all survey records
together) with its Chao asymptote.

**"Possible species (max/min/median, over/under-estimate)."** This is the richness-
estimator family: **observed = floor (min)**, **point estimates (Chao1/Chao2,
Jackknife1/2, bootstrap) = median-ish**, **upper CI = plausible max**. Run per scope
(CABR, and if the SD/PL record sets are available, those too). One small table:
scope × estimator → a defensible range instead of a single number. Natural
extension of the completeness run.

### `coverage` — who/what/where are we missing?

The cheapest, highest-value cluster — most of it is **checklist arithmetic, no
modeling**, and several are near-instant.

- **#1 Venn (lethal vs non-lethal).** Directly from the official checklist's
  `specimen` (121 TRUE) and `inat` (165 TRUE) boolean columns → a 2-set Venn of
  taxa. The "where do we need more sampling" reading is really "taxa one method
  misses" — frame it with the detection-bias caveat (the Dialictus point). Sub-part
  (genus→species, which species each method lacks) is a straight checklist filter,
  **no survey needed**, as the stakeholders noted.
- **#2 Taxonomic resolution by method (survey only).** Their hypothesis is broadly
  right but sharpen it: lethal is **79% to species**, non-lethal **~52%**, with a
  big non-lethal **subgenus/genus tail** (2,167 subgenus + 1,152 genus). Deliver as
  a stacked %-of-rank bar, lethal vs non-lethal.
- **#7 Target-ID list.** Count records stuck at coarse ranks (genus/complex/tribe/
  subgenus) by taxon → the most common unresolved taxa are the ID priorities. Split
  lethal vs non-lethal (they stall at different ranks). Feeds directly into #2.
- **#10 CABR not on Holway.** The `holway` flag is FALSE for **11** of the 189
  checklist taxa → those 11 are the answer. One filtered table.
- **#6 Off-transect bees.** `is_survey == FALSE` (walk-in / non-transect park
  records) vs the transect set → taxa seen only off-transect, with their `url`s so
  reviewers can check the plant/context. Speaks to "do we need more/other
  transects." ✅ but interpret spatially.
- **#11 Yield by group.** Species-rank catch per survey, beeple vs intern and lethal
  vs non-lethal. ⚠️ **confounded** (lethal = all intern) — report both cuts but say
  so, and restrict to the season window.

### `interactions` — plant–bee

- **#3 Top-10 plants visited (overall + per month).** Counts of bee records per
  `flower_visited` plant. ⚠️ prep step: `flower_visited` is a **plant-name string**,
  so resolve names → plant taxon/genus first (there's already a
  `plant_name_resolution_cache` + `plant_taxonomy_lookup` in `data/reference/` to
  lean on). Fill is good (specimens 91%, iNat 78%). "Just by counts" is fine for a
  first pass — flag that counts reflect **effort + detectability**, not preference.
  ❗ confirm "by park" = whole-park records vs survey-only, and whether it's
  non-lethal only or both.
- **#4 Visitation network (rare-bee specialists).** Exactly their recipe: **plant
  genus (rows) × bee species (cols)**, 1 per co-occurrence, both methods pooled,
  then project the bipartite graph. Notes: (a) needs the same name→plant-genus
  resolution as #3; (b) presence (1/0) answers "who visits whom", weighting cells by
  **counts** additionally shows strength; (c) "specialist" only makes sense at **bee
  species** rank, so drop genus-only bees for that read; (d) "rare/lesser-seen" bee =
  low **relative frequency** (proportion), not a hard count.
  **Method (per the Shizuka ecological-networks tutorial,**
  <https://dshizuka.github.io/networkanalysis/networktypes_ecolnets.html>**):**
  incidence matrix → `igraph::graph_from_incidence_matrix()` (auto-tags plant vs bee
  node types) → `bipartite::plotweb()` for the two-row interaction picture →
  `bipartite_projection()` (≡ `t(bmat) %*% bmat`) to collapse to a **bee–bee network
  linked by shared plants**, which surfaces specialist/rare-bee structure directly.
  **Two networks (mirrors the accumulation run, and handles that not all bees are
  ID'd to species):** a **bee-genus** network (plant genus × bee genus — every
  genus-resolved record; the complete/robust view) and a **bee-species** network
  (plant genus × bee species — sparser, and the only one where the specialist read
  is valid). Deps: `igraph`, `bipartite` — install-guarded at the top of
  `interactions_network.R`, NOT in utils.R (the pipeline never needs them).

### `phenology` — timing

- **#12 Plant phenology (non-lethal only).** From the plant clean table's flowering
  signal by month — lethal excluded on purpose (their dates are unreliable). ✅
  Restrict to the survey window.
- **#13 Obs/trips by month.** #trips from per-survey info, #observations from record
  counts, ratio by month. ✅ Simple effort curve; window caveat.
- **#8 (monthly slice).** Month-by-month diversity turnover — see diversity.

### `diversity` — how much, and where

- **#8.** Three sub-questions bundled: (a) **diversity across years**, genus vs
  species/subgenus (Shannon/Simpson + richness); (b) **month-by-month** within the
  window (→ overlaps phenology); (c) **spatial concentration** — where in the park
  diversity peaks, from coordinates (a hotspot map or per-transect/grid diversity).
  ⚠️ Split into a `diversity` run (indices by year/transect) and a small **spatial**
  piece; the monthly slice rides with phenology. Watch the season window and the
  lethal=intern confound.

---

## Suggested build order

1. **Coverage quick wins — #10, #1, #2, #7.** Mostly checklist arithmetic; each is a
   small table/figure and answers a concrete stakeholder ask almost immediately.
2. **#13 then #12** (effort + plant phenology) — light, and set up the season-window
   handling everything else reuses.
3. **#3 → #4** (plants, then the network) — do the name-resolution prep once, reuse
   for both.
4. **Completeness extension** — park-level pooled curve + the possible-species
   range table.
5. **#8 diversity** (years/spatial) and **#6** (off-transect gap) — the heavier,
   more interpretive ones.
6. **#11** last, framed carefully given the confounds.

## Decisions (from stakeholders, 2026-07-21)

- **#3 scope:** run it **three ways** — whole-park, then survey-only, then split by
  **both methods** — **CABR only** (not PL/SD).
- **Possible-species scope:** **CABR only.** No PL/SD record sets needed.
- **Intern training photos:** no separate exclusion — practice/training shots aren't
  a distinct thing to strip out here; counts stand as-is.
- **"Rare bee" for #4:** defined by **relative frequency (proportion)**, not an
  absolute count — a bee seen at a much lower proportion than the rest drops into a
  "lesser-seen" group. No hard cutoff.
- **Plant name→genus resolution (prereq for #3/#4):** stakeholder is fixing the
  `flower_visited` → plant resolution now; #3/#4 build once that lands.

---

## Pending / proposed — "find the bee by its plant's flowering season" (synthesis of #4 + #12)

**Status:** ❗ awaiting supervisor input on two architecture calls before building (raised 2026-07-23).

**The idea.** We have three separate pieces — flowering-plant phenology (#12, now bloom-only),
bee phenology, and the plant→bee visitation network (#4) — but nothing that *joins* them into
"during plant X's bloom, which bees are on it and when, so here's when/where to look." All the
ingredients exist in the data (bee date + `plant_genus` + `bee_on_flower` + `latitude`/`longitude`).

**Locked in (from the 2026-07-23 chat):**

- **Scope:** BOTH ranks — bee **genus** (dense) and bee **species** (where records allow).
- **Deliverables:** (a) overlap timelines, (b) when-to-find matrix (taxon × month heatmap),
  (c) lookup table (CSV + doc), and (d) an **ArcGIS map** — a spatial layer of bee-on-plant
  visitation points (attributes: date, month, bee genus/species, plant genus, method, transect),
  since every bee record already carries coordinates (same as the Q8 richness maps).

**Open — needs supervisors (these drive the whole architecture):**

1. **Orientation:** plant-first ("I'm at plant X, what bees?"), bee-first ("I want bee Y, where/when?"),
   or both (one core plant×bee×month table, rendered both ways).
2. **Timing basis:** actual on-plant sightings (direct, but sparse — especially at species rank),
   phenology overlay (bee activity curve ∩ bloom curve; smooth but inferential), or both
   (overlay window with real sightings marked on top). Interacts with "both ranks": species-level
   *actual* pairs will often be too thin, so we may end up overlay-at-species / actual-at-genus.
3. **ArcGIS map specifics:** what it maps — individual visitation points (filterable) vs aggregated
   per-plant hotspot cells — and format: **GeoJSON** and/or **CSV with lat/long** (Arc "XY Table to
   Point") are cleanest; shapefile (`.shp`) if the workflow requires it. (The point-map leans on the
   *actual-sightings* timing basis; the overlay approach isn't inherently spatial.)

**Next step:** once (1)–(3) come back, spec the architecture concretely and build. Likely home:
a new `interactions` (or a small new `cooccurrence`/`fieldguide`) run + a spatial export for Arc.

**Update 2026-07-24 — the method-gap piece is BUILT.** The actionable half of this (the "reverse
voucher" idea) is done as the **Bee Bounties** (`bee_bounties.R` → `data/analysis/bee_bounties/`):
- *Specimen Bee Bounty* — in iNat photos, no specimen → net a voucher (30 species + 8 genera).
- *iNaturalist Bee Bounty* — in specimens, not on iNat → get a photo (17 species + 1 genus).
Each with where/when/on-what context. Still pending for the full field guide: the plant-centric
top-10 (bloom window + specialists/generalists) and the **ArcGIS map** (questions 1–3 above).

---

## Done since 2026-07-23 (this batch)

- **Rarefaction** (`rarefaction_vegan.R` + `rarefaction_inext.R` → `data/analysis/rarefaction/`):
  effort-controlled diversity by transect, year, and beeple-vs-intern. vegan verified; iNEXT written
  (runs on a machine with internet). See `rarefaction/README.md` for which method to trust.
- **Bee Bounties** (see update above).
- **NPS summary tables** (`nps_summary_tables.R` → `data/analysis/nps_summary/`): participation, bees,
  methods, plants — descriptive counts only, for the data-focused NPS report.
- **Data-issues working notes** (`docs/data_issues_notes.md`, unofficial): script-fixable vs
  needs-project-manager — seeds the Melittology "lessons learned" section.

## Publications (parked — scoping only)

- **Journal of Melittology** (interpretive; bee-centered community-science monitoring): does non-lethal
  supplement lethal, vouchers-to-ID-iNat, strengths/weaknesses, lessons learned. Fed by Q1/Q2/Q6/Q11 +
  bounties + data-issues notes.
- **NPS peer-reviewed** (data-focused, no interpretation): the `nps_summary` tables, in Taro's format.
