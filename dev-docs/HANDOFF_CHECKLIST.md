# Handoff checklist

Getting this repo ready to pass to Brandi's supervisor. Delete this file once the
handoff is done — it is a working list, not permanent documentation.

Started 2026-08-25.

---

## Blocked on you (I cannot do these)

- [ ] **Do NOT transfer `data/secrets/`.** It holds YOUR iNaturalist app credentials and
      YOUR IUCN key. The new operator creates their own (see SEASON_RUNBOOK section 1b);
      the pipeline asks for them on first run. Delete the folder from any copy you hand over.
- [ ] **Decide what happens to the `beescabr` OAuth app** owned by @randombirdlover. Taro
      should create his own on the park account (`officialbeescabr`). Yours can then be
      destroyed at <https://www.inaturalist.org/oauth/applications>.
- [ ] **Use the PARK account, not a new one.** Coordinate trust on iNaturalist is granted by
      each observer to a specific account. Surveyors trusted `@randombirdlover` and
      `@cabrillonationalmonument`. A new account starts with no trust and would return
      obscured coordinates until every observer granted it separately, which is not
      realistic. Sign in as `@cabrillonationalmonument`.

- [x] **Answer the scoping question.** What will the supervisor actually do with  
      answered 2026-08-26: Taro Katayama will BOTH run it each season and take over the code; comfortable with R and git.
- [x] **Roster affiliations** — Patricia, Sofia, Diego, Toby have blank  
      done -- all 16 identifier rows carry an affiliation.
- [x] **`@itazura` has no name** in `identifier_roster.csv` (first and last blank).  
      resolved -- affiliation reads 'Grade school student', so the name is deliberately withheld.
- [x] **Two placeholders in `DATA_ACCESS.md`** — the public site URL and a contact  
      done -- site URL verified live, contacts are Brandi Sanchez and Taro Katayama.
- [x] **Decide who the data contact is after you leave**, and say so in  
      done -- Taro Katayama holds the data; both contacts listed.
- [ ] **Rewrite `PIPELINE_GUIDE.md`.** It is numbered 1-10, but sections 6-10 came
      over from the README and it has never had a proper pass. Should read as one
      document written for a new maintainer.
- [ ] **Merge `VERIFICATION_DESIGN.md` + `verification_guide.md`** into one
      `VERIFICATION.md`. Two files, one topic: the design is the *what*, the guide
      is the *how*.
- [ ] **A season runbook** — what to run, in what order, at what point in the year,
      and what the interactive prompts will ask. Only worth writing if they will
      actually run it.
- [ ] **`TODO.md` triage** — decide which open items are real commitments to hand
      over vs. ideas that should be dropped before someone inherits them.

## Nice to have

- [x] **Install `testthat`** so the 278 tests can be run without me:  
      done -- installed; the suite runs at 993 passing.
- [ ] **Decide whether `data/analysis/` outputs get archived somewhere** the
      supervisor can reach, since `data/` never leaves your machine.

---

## Done

- [x] **Consolidated all documentation into `dev-docs/`** — no buried `.md` files
      anywhere; exactly one `README.md`, at the root. (2026-08-24)
- [x] **README.md: 684 -> 216 lines.** Moved 5 sections to their proper homes,
      deleted 2 obsolete ones, fixed every dead path, added a documentation map.
- [x] **Fixed 12 stale facts across dev-docs** — dead entrypoints, a done item
      marked open, wrong file paths, a retired Python script.
- [x] **Merged `PITFALLS.txt` + `data_issues_notes.md`** into `LIMITATIONS.md`.
- [x] **Created `ARCGIS_SPATIAL_MAPPING.md`** from the 111-line `spatial_utils.R` header;
      script trimmed to 33 lines of what a code reader needs.
- [x] **`.gitignore` documents WHY** each rule exists — people, credentials, park
      localities.
- [x] **`dev-docs/` is tracked**, so a clone actually gets the working knowledge.
- [x] **Removed 3 abandoned git worktrees** (17 MB of stale duplicate files).
      (2026-08-25)
