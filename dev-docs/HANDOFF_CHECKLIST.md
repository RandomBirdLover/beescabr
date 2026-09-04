# Handoff checklist

Working list. Delete once the handoff is done.

## Only you can do these

| | Why |
|---|---|
| **Do NOT hand over `data/secrets/`** | Your iNaturalist and IUCN keys. Delete the folder from any copy. The next operator makes **their own OAuth app, under the park's iNaturalist account** — not a new account of their own, which would get obscured coordinates until every observer trusted it separately. The pipeline asks for the values on first run. |
| **Retire your OAuth app** | Taro creates his own on the park account, then destroy yours at [inaturalist.org/oauth/applications](https://www.inaturalist.org/oauth/applications). |
| **Use the PARK account** | Surveyors granted coordinate trust to `@randombirdlover` and `@cabrillonationalmonument`. A brand-new account gets obscured coordinates until every observer re-grants. Sign in as `@cabrillonationalmonument`. |
| **Hand over `data/`** | It is gitignored — a clone gets no data at all. Drive, zip, however. |

## Still open

- [ ] Triage `TODO.md` — which items are real commitments vs. ideas to drop
- [ ] Decide whether `data/analysis/` outputs get archived somewhere reachable

## Settled

| | |
|---|---|
| Who takes over | Taro Katayama — runs it each season *and* owns the code |
| Data contact | Taro; both contacts listed in `DATA_ACCESS.md` |
| Roster | All 16 identifier rows carry an affiliation |
| Docs | One README at root, everything else in `dev-docs/` |
| Tests | `testthat` installed; suite runs |
