# Data issues — working notes (UNOFFICIAL)

> Informal scratch list, not a formal deliverable. Purpose: separate what a **script
> can fix now** from what only the **project manager / a protocol change** can fix.
> Feeds the "lessons learned" section of the Melittology paper down the line. Edit freely.

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
