# Known limitations

Everything this pipeline knowingly trades away, in one place: pipeline/design
assumptions, methodological trade-offs we accepted on purpose, and data problems
split by whether a script can fix them. Merged 2026-08-24 from three files that
covered the same ground (README "Known limitations", PITFALLS.txt,
data_issues_notes.md).

---

## 1. Pipeline and design assumptions


Like any research pipeline, this one makes trade-offs and carries assumptions that are worth stating explicitly.

**Taxonomy follows iNaturalist, which is a moving target.** The `bee_taxonomy_lookup.csv` is built fresh from the iNat API each run, so genus and species names track iNat's current taxonomy — but iNat itself lags behind the primary literature and occasionally disagrees with other authorities (e.g. ITIS, Discover Life). A taxon reclassification on iNat between runs can silently change which checklist row an observation joins to, or cause a previously-matching specimen name to no longer match.

**The specimen taxonomy spell-check is a heuristic, not an authority.** `specimen_bee_clean.R` flags genus and species names that don't appear in the taxonomy lookup, which catches most typos and some outdated names. It will not catch: names that are still valid iNat taxa but incorrect for the specimen at hand; rank changes that don't alter the genus+species string; or iNat synonyms that haven't been cleaned up yet.

**Genus-level and subgenus-level identifications are kept, not excluded.** The pipeline follows a "possible at CABR" philosophy — an iNat observation identified only to *Lasioglossum* counts as evidence of *Lasioglossum* at CABR. This is intentional (excluding them would lose real data), but it means checklist presence evidence varies in precision: some rows are confirmed to species, others only to genus.

**The combined checklist "Museum Collection" column reflects CABR survey specimens only.** The specimen sheet is the sole source for the X in that column. Bees observed on iNat but not collected are not counted as specimen evidence, even when the identification is unambiguous. This is by design — the column tracks physical specimens in the collection — but it means the column is not a proxy for "seen at CABR."

**Spatial tiers depend on iNat place boundaries, which are user-contributed.** The CABR, Point Loma, and SD County tiers are defined by iNat's place geometries, not by authoritative NPS or government shapefiles (except where `spatial_utils.R` applies the NPS CABR boundary for fine-grained spatial clipping). Minor boundary inconsistencies between iNat places and official shapefiles can cause observations to appear in one tier but not another unexpectedly.

**Output files must be manually deleted before re-running.** `write_fresh()` does not overwrite existing CSVs — if a prior output exists, the new run skips the write and leaves stale data in place. Delete the relevant generated files (under `data/observations/inat_clean/`, `data/checklists/`, `data/analysis/`) before each run until this is resolved. (See also: TODO.md.)

**Forage "preference" is matched on month/year/method, but two confounders remain.** The selectivity test (`forage_selectivity.R`, driving the *Forage preference* column and the web colors) matches availability to each genus's own month, year and method cells (see "Forage selectivity" and the confounder audit above). What it still can't fix: (1) **plant detectability** — a bee on a big showy flower is far more likely to be photographed than the same bee on a tiny inconspicuous one, and since our availability proxy is itself photo-derived, that bias sits on both sides of the comparison. The clean fix would be an *independent* bloom census (plant-survey phenology), but that prepared plant data does not exist / is not available, so detectability is an acknowledged, uncorrectable limitation here. (2) "Availability" is the community's realized plant *use* per cell, not a true bloom measurement; and verdicts near p = 0.05 (e.g. *Dianthidium*) are borderline. **Observer identity is not controlled but does not need to be** — it's spread across 10–48 observers per genus, so it averages out. See the confounder audit for the one test that still needs work (H2′).

---

---

## 2. Methodological trade-offs accepted on purpose

new trade-offs come up.


1. MISSING-SURVEY DETECTION IS PER-DATE, NOT PER-TRANSECT
--------------------------------------------------------
What we do: the "was a planned survey missed?" check treats a scheduled date as
covered if ANY tagged survey exists near it -- regardless of who did it or which
transect it was on. (Beeple covered each other's shifts and swapped transects, so
tying a plan to a specific person + transect was unreliable and produced false
"missing" flags.)

The downside: we catch missing DATES (scheduled days with zero survey evidence),
but we do NOT catch a missing TRANSECT. If UPMON was surveyed but TP was skipped
that same week, the date still reads as "covered," so the dropped TP transect is
never flagged.

Consequence for analysis: per-transect coverage/effort may be incomplete -- a
transect can have gaps this method will not surface. Any analysis that depends on
per-transect effort should verify coverage separately against the raw beeple
calendar, not rely on per_survey_information.csv alone.


2. MISSING-SURVEY DETECTION IS BY TAGS, MIGHT MISS SURVEYS THAT DIDN'T HAVE A SINGLE TAG
--------------------------------------------------------

---

## 3. Data problems: what a script can fix vs what needs a protocol change

---

## Fixable now, in cleaning/analysis scripts

These we already handle (or easily can) in R — no protocol change needed.

- **`flower_visited` free-text plant names** → resolved to a `plant_genus` via the
  name-resolution cache. Scriptable, ongoing as new names appear.
- **`taxon_rank` casing / label variants** (e.g. "Species" vs "species") → standardized
  in cleaning.
- **Transect label variants** (TP1 / TP2 → TP; casing/whitespace) → normalized to the
  four transects.
- **Blank `is_survey` / `surveyor_type`** → handled explicitly (treated as
  "unattributed" where empty) rather than silently dropped.
- **Date parsing** (formats → day-of-year / month) → scripted.
- **Package/namespace clashes** (e.g. `vegan::diversity` vs `igraph::diversity`, H2′
  bound to [0,1]) → fixed in the analysis scripts; not a data problem.

## Needs the project manager / a protocol change (a script can't fix these)

These are gaps baked in at data-entry time; only a change to how data is collected/logged fixes them going forward.

- **`flower_flowering` column almost never filled** (~68 of 9,243 rows). We inferred
  "flowering" from survey protocol instead. Fix: either make the y/n field required, or
  formally document "a survey plant record = in flower" so the inference is official.
- **Intern training / practice photos not flagged.** Can't separate practice IDs from
  real records after the fact. Fix: a "training" flag at entry.
- **Un-attributed on-transect iNaturalist records** (~3,868 with no `surveyor_type`).
  Provenance/observer not captured. Fix: tie each iNat record to a surveyor + role at
  entry (project membership, not post-hoc).
- **Photos stalling above species** (Lasioglossum, Ceratina, etc.). A detection limit
  of photography, not a data-cleaning bug — needs vouchers/keying (see the Specimen Bee
  Bounty), not a script.
- **Specimen ID backlog** (204 specimens keyable at genus; Melissodes + Colletes the
  bulk). Needs a specialist to key them; can't be scripted.
- **Missing vouchers for photo-only taxa** (30 species + 8 genera in photos, no
  specimen — the Specimen Bee Bounty). A collecting task, not a data task.
- **Structural confounds to state, not fix:** all lethal records are intern-collected;
  OT is non-lethal only. Report these; they're design facts, not errors.
- **Casual iNat coordinate precision varies** (`positional_accuracy` up to hundreds of
  m). We drop >250 m for the spatial map; finer work would need better-located records.

## One-liner for the paper

> Volunteers generated an enormous, valuable dataset, but standardizing and cleaning it
> was the single biggest effort — and several of the hardest problems (flowering flags,
> observer attribution, training photos, species-level determination) can only be solved
> by protocol at collection time and by a dedicated data/results person, not after the fact.
