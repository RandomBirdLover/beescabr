# Pass‑2 Verification — how to check a flagged bee on iNaturalist

**When:** the pipeline's pass‑2 prompt shows a taxon that is *new to the Holway San Diego
baseline* and asks:

```
Andrena cerasifolii (id 1538175, complex).  Real ID / genuine SD record?  [y verify / r reject / Enter skip / x stop]:
```

**The question you're answering:** *does this bee genuinely occur in San Diego County?*

**The rule:** a taxon counts as a real SD record if it has **at least one *verifiable*
observation inside San Diego County** — the county boundary itself, **not** a mileage
radius around it.

---

## Steps

1. **Grab the `id` from the prompt.** That number is the iNaturalist **taxon_id**
   (e.g. `1538175`). Using the id is more reliable than typing the name — it avoids
   name/spelling ambiguity, especially for *complex* taxa.

2. **Open that taxon's observations on iNaturalist.** Easiest:
   paste this into your browser, swapping in the id —
   `https://www.inaturalist.org/observations?taxon_id=1538175`
   (or search the name under **Explore**).

3. **Open `Filters` and set two things:**
   - Under **Show**, check **Verifiable**. (*Verifiable* = **Research Grade + Needs ID**;
     it **excludes Casual** records — single specimen‑drawer photos, no date/location,
     captive/cultivated, out‑of‑range junk. That casual stuff is exactly what we don't
     want to count.)
   - Click **More Filters**, and under **Place** type `San Diego County` and select
     **“San Diego County, CA, US”**. Then hit **Update Search**.

4. **Use the `Place` filter — NOT the top `Location` box.** The **Place** field uses the
   real **county boundary**; the top Location box can attach a pin + mileage radius
   (e.g. “25 mi”), which leaks in neighboring counties (Imperial, Orange, Riverside).
   County boundary only.

5. **Read the result and answer the prompt:**

   | What you see in SD County (Verifiable) | Answer |
   |---|---|
   | **≥ 1 observation shows up** | **`y` verify** — real SD bee, add to the checklist |
   | **0 observations** | **`Enter` skip** now (comes back next run), or **`r` reject‑for‑now** once that option is live in your session |

---

## Notes & edge cases

- **Complex / subspecies flags** (e.g. *Bombus fervidus* complex, *Andrena cerasifolii*
  complex) are auto‑flagged because Holway has **no complex/subspecies concept** — not
  because they're suspicious. If the complex (or its common member — e.g. the *B. fervidus*
  complex's *B. californicus*, the California Bumble Bee) has verifiable SD‑County records → **`y`**.

- **Wrong‑county leaks** (e.g. *Perdita larreae*, whose only "record" was a **casual photo
  of an Imperial County museum specimen**) → nothing verifiable shows up in SD County → **reject**.

- **When unsure, skip.** A skipped taxon returns next run, so deferring costs nothing.

- **`r` = reject‑for‑now is remembered, not deleted.** A rejected taxon is re‑shown next
  run flagged *"you rejected this before — verified now?"*, so if it's ever confirmed later
  you just hit `y`. To forget a rejection entirely, delete its row from
  `data/reference/hand_curated/rejected_taxa.csv`.

- **Specimen vs photo:** Chris's **specimen** determinations you can trust directly. The
  iNaturalist check above is mainly for the **photo (iNat)** taxa — like *Perdita larreae*,
  which turned out to be an out‑of‑county casual photo.

---

*This is pass 2 of two. Pass 1 (the taxon_id prompt) answers "which iNaturalist species is
this?"; pass 2 (here) answers "is that ID actually right for San Diego?" Your `y` / `r` /
skip calls are what curate the confirmed San Diego bee checklist.*
