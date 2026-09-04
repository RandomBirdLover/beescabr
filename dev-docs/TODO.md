# TODO

One line each. Done items are deleted, not archived — git remembers them.

## Paper & report

- [ ] Finish the analyses James needs
- [ ] Finish writing the report
- [ ] Literature review — community science projects

## Protocol

- [ ] **Make a CABR Native Bee Project on iNaturalist**, add every observation.
  One observation per call; needs the JWT auth in `engine/api/inat_auth.R`.
- [ ] **Change the field protocol** on what the analyses showed. Two things bear on it:
  `flower_flowering` is filled on 68 of 9,243 rows, so plant phenology cannot be
  computed; and before 2024 only interns netted and only beeple photographed.

## Specimens

- [ ] **Tobi's wasps — 162 specimens, none identified.** Tiphiidae 79, Crabronidae 41,
  Vespidae 12, Pompilidae 3, Scoliidae 1, family unknown 26. All are `needs_ID = Y` with
  `ask_ID_from = Tobi Hays`. Tags still need fixing against the plant records.
- [ ] **Joel Gardner is identifying the rest of the bees — 48 of them, as of V19:**
  24 stuck at tribe (all Halictini), and 24 at genus —
  *Lasioglossum* 19, *Perdita* 2, *Colletes* 1, *Dufourea* 1, *Hylaeus* 1.
- [ ] One duplicate SDNHM tag left: `218093` is on two *Lasioglossum* — ucsd_id 353
  (*perichlarus*, det. JL Mullins) and 383 (no species yet). Needs a new tag from Shahan.
  The 29 zeroed in V13 are already resolved: V19 has no blank or zero `sdnhm_id`.
- [ ] Formal deposit to SDNHM (Shahan Derkarabetian)
- [ ] Physical box audit — duplicates, error flags, unidentified

## Pipeline

- [ ] **Migrate the iNaturalist API v1 → v2.** Not a URL swap: v2 needs an explicit
  `fields` parameter and returns nothing else, so `inat_flatten.R` must be rewritten
  against a different response shape. Do it behind the existing injectable transport,
  test against recorded v2 responses, and keep v1 working until v2 is proven — the
  cache is the system of record and a half-migrated ingest writes malformed rows.
  Pairs with JWT: v2 is the cleaner route to private coordinates.

## Handoff

- [ ] **Delete `data/secrets/` from any copy that leaves this machine.** A key is
  personal; a pull runs as whoever signed in. The rest is already handled — `data/`
  is gitignored, no names in the README or DATA_ACCESS.
- [ ] Sweep the `.DS_Store` files under `data/`
- [ ] Cut `PIPELINE_GUIDE.md` back to architecture
