# Handoff checklist

Getting this repo ready to pass to Brandi's supervisor. Delete this file once the
handoff is done — it is a working list, not permanent documentation.

Started 2026-08-25.

---

## Blocked on you (I cannot do these)

- [ ] **Answer the scoping question.** What will the supervisor actually do with
      the repo, and how comfortable are they with R and git? This decides how much
      of everything below is worth writing.
      - Will they: run it each season · take over the code · review and archive it?
      - Are they: comfortable with both · knows R not git · mostly a biologist?
- [ ] **Roster affiliations** — Patricia, Sofia, Diego, Toby have blank
      `affiliation` in the rosters.
- [ ] **`@itazura` has no name** in `identifier_roster.csv` (first and last blank).
- [ ] **Two placeholders in `DATA_ACCESS.md`** — the public site URL and a contact
      person/email for someone requesting the data.
- [ ] **Decide who the data contact is after you leave**, and say so in
      `DATA_ACCESS.md`. Right now the answer is implicitly "Brandi."

## I can do these once scoped

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

- [ ] **Install `testthat`** so the 278 tests can be run without me:
      `Rscript -e 'install.packages("testthat", repos="https://cloud.r-project.org")'`
      then `Rscript -e 'library(testthat); test_dir("tests/testthat")'`
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
- [x] **Created `spatial_mapping.md`** from the 111-line `spatial_utils.R` header;
      script trimmed to 33 lines of what a code reader needs.
- [x] **`.gitignore` documents WHY** each rule exists — people, credentials, park
      localities.
- [x] **`dev-docs/` is tracked**, so a clone actually gets the working knowledge.
- [x] **Removed 3 abandoned git worktrees** (17 MB of stale duplicate files).
      (2026-08-25)
