# People roster redesign — problem brief

Status: **design discussion only. Do not write code, schemas, or tests yet.**
This document states a problem and a direction. The open decisions at the bottom
are not settled, and settling them is the next task, not implementing them.

## Repo context you need

- R pipeline, Cabrillo National Monument bee survey (`beescabr`).
- `scripts/config.R` holds `PATHS` and all constants.
- `scripts/utils/people.R` already turns a name string into a person, and
  `tests/testthat/test-people-keys.R` covers it. It handles three written forms
  of one person: `Sam O'Dell` (logs), `S O'Dell` (museum label), `Sam` (old data),
  and refuses a bare first name when two people share it (there are two Julias).
- `CLAUDE.md` rules that apply: test-first red/green for anything built; join on
  ids, never on assembled name strings; constants live in `config.R`; never
  commit or push.

## Current state: three hand-maintained roster files

| file | grain | rows / people |
|---|---|---|
| `data/project_info/rosters/surveyor_roster.csv` | one row per person-year | 70 rows, 34 people, 2021-2026 |
| `data/project_info/rosters/identifier_roster.csv` | one row per person | 16 |
| `data/project_info/rosters/research_team_roster/research_team_roster.csv` | one row per person | 7 |

48 distinct people. Six appear in more than one file, with identity fields
(email, iNat handle, affiliation) copy-pasted between them.

Consumers today: `build_content_pages.R` (acknowledgements page),
`nps_summary_tables.R` (headcounts), `specimen_bee_clean.R` (determiner code ->
person), `inat_plant_clean.R` (surveyor handles -> scope filter),
`qc_review_inat_location_maps.R`.

## Problem 1 — people have no id, so identity drifts

There is no person key. Every file re-states the same person by name, and two
have already diverged:

- **Sam O'Dell**: `sam.odell@gmail.com` in the surveyor roster,
  `sodell@ou.edu` in the identifier and research-team rosters.
- **Lydia Duran**: two rows inside `surveyor_roster.csv`, one with an email and
  one blank.

The taxonomy layer already solved this exact problem with `taxon_id` and a rule
that a name is for display only. People never got the same treatment.

## Problem 2 — one `role` column is answering three different questions

The vocabulary in use (beeple, intern, surveyor, collector, photographer,
identifier, researcher) is not one list. It is three axes that have been
flattened into one column:

| axis | question it answers | values seen |
|---|---|---|
| capacity | how is this person engaged with the project? | beeple (volunteer), intern, staff, partner, PI |
| activity | what did they actually do? | photographed, netted, identified, coordinated |
| qualifier | in what mode? | net / photo, lethal / non-lethal, bee / plant / both |

`beeple` is not a peer of `collector`. It is a bundle: volunteer capacity plus
photo activity, wrapped in a project word.

The data disproves any attempt to treat capacity as activity:

```
         net  photo
beeple    0     46      <- capacity beeple is always photo
intern   18      6      <- capacity intern is BOTH
```

`collector_code` is filled for only 8 of 24 intern person-years, so "netted" and
"has a museum label code" are not the same fact either.

## Problem 3 — the shape cannot record a person who did two things

`surveyor_roster.csv` stores one `technique` per person-year. An intern who both
netted and photographed in the same season is structurally unrepresentable.
This is confirmed to happen. So the fix cannot be "add more values to `role`" —
the grain itself is wrong.

## Problem 4 — a capacity change erases the person

`role` accepts only `beeple` or `intern`. There is no `staff`. A real case:
Brandi Sanchez has a single `2024 / intern / non-lethal / photo` row, and now
appears in the research-team roster as "Natural Resource Management Research
Assistant." Her participation after the internship has nowhere to live in the
file that is the authority for who participated. Anyone who is hired, or who
moves from volunteer to intern, hits the same wall.

## Problem 5 — derived numbers are stored by hand

`identifier_roster.csv` carries `id_count` (e.g. 8258, 3526). That is a computed
statistic living in a hand-edited file. It is stale the moment it is written.

## What we want to try

**A. Give people a stable id.** One `person_id` per human, assigned once and
then opaque — never re-derived from a name, so a surname change does not break
every join. Identity facts (name, handle, email, affiliation, `collector_code`,
`determiner_code`) live in exactly one row, in one file.

**B. Split the three axes instead of picking better role words.** Capacity,
activity, and qualifier get recorded separately. Project words like "beeple"
become a *display label* rendered from capacity + activity for the website, not
a stored key. Store the atoms, print the bundle.

**C. Treat activity as a property of records, not of people.** Nobody "is" a
collector. A specimen records who collected it, an observation records who
photographed it, a determination records who identified it. Once a crosswalk can
resolve `S O'Dell` and `wranglebees` to one `person_id`, those roles are
countable rather than maintained, and `id_count` stops being hand-typed.

**D. Generate a people crosswalk.** One table mapping every string form the data
actually writes (full name, museum label form, iNat handle, declared code) to a
`person_id`, plus a QC file listing ambiguous keys that were dropped and name
strings found in the data that resolve to nobody. `person_name_keys()` in
`scripts/utils/people.R` already builds this in memory and is tested; the work is
persisting it keyed on `person_id`.

**E. Keep declared participation authoritative for headcount.** Participation is
not contribution. A volunteer who came out six times and photographed nothing
identifiable still participated, and a purely derived roster would erase them.
Declared capacity answers "who was on the project"; derived activity answers "who
did what." These are two different numbers and they need two different names, or
the wrong one will end up in an NPS report.

## Open decisions — settle these before designing tables

1. **Is capacity per-year, or per-person with a date range?** Cindy Pencek is six
   near-identical rows saying one thing. A date range is compact but harder to
   hand-edit and to count by year.
2. **Do identifiers get years?** They have none today, but identifications happen
   in time and the records know when.
3. **Can one person-year hold several activities?** Problem 3 says yes. Confirm,
   because it decides whether activity is a column or its own row.
4. **How much stays hand-edited vs derived?** This is the real fork; the others
   mostly follow from it. Fully derived is honest but erases the unproductive
   volunteer (Problem/point E). Fully declared is maintainable but drifts
   (Problem 1).
5. **How is a capacity change over time represented?** Nobody in the current data
   switches, so no existing shape has been tested against it, and Problem 4 shows
   it is already real.
6. **How many hand-edited files?** One long table repeats capacity within a
   person-year (a drift risk a validation check could catch); two tables avoid
   the repeat at the cost of a join when editing 48 people by hand.

## Constraints

- Do not build, migrate, or write tests yet.
- Any eventual migration must reproduce the known counts: 34 field participants,
  16 identifiers, 7 research team, 48 distinct people, 70 person-years.
- Conflicts found during migration (Sam's two emails) get surfaced for a human to
  resolve, never silently picked.
