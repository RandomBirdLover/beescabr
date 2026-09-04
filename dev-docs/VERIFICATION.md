# Verification

The pipeline stops and asks you to confirm a bee it has not seen before:

```
Andrena cerasifolii (id 1538175, complex).
Real ID / genuine SD record?  [y verify / r reject / Enter skip / x stop]
```

**The question:** does this bee genuinely occur in San Diego County?

## Answer it in 4 steps

```
1.  Take the id from the prompt                      1538175
2.  Open  inaturalist.org/observations?taxon_id=1538175
3.  Filters:  Show -> Verifiable
              More Filters -> Place -> "San Diego County, CA, US"
4.  Count what comes back
```

| Result | Answer |
|---|---|
| **1 or more observations** | `y` — real SD bee |
| **none** | `Enter` to skip (returns next run), or `r` to reject |

## Two traps

| | |
|---|---|
| **Use `Place`, not the top `Location` box** | Location attaches a pin + radius and leaks in Imperial, Orange, Riverside. Place uses the real county boundary. |
| **`Verifiable`, not `Any`** | Verifiable = Research Grade + Needs ID. It excludes Casual — drawer photos, no date, captive. That is exactly what shouldn't count. |

## How it works underneath

```
iNat observation
      ↓
  is its genus / subgenus / complex / species / subspecies
  in the Holway San Diego reference?
      ↓ no
  flag it  →  prompt you  →  save the answer
      ↓
  verified_taxa.csv   or   rejected_taxa.csv
      (data/reference/hand_curated/)
```

| | |
|---|---|
| Your answers are **remembered** | You are never asked about the same taxon twice |
| Rejecting is **not deleting** | The record stays in the data, just off the checklist |
| Run it when? | Part of the cleaning pipeline, interactively. An unattended run skips the prompts. |
