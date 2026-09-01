# ArcGIS spatial layers and mapping

> This file is about the **geodata**: where each layer came from and what is known to be
> wrong with it. How the interactive map *pages* are built and published is in
> `WEBSITE_GUIDE.md`.

Everything about the boundary and transect geometry this project uses: where each
shapefile came from, how it was built, which one is the real inclusion geometry,
and the known coastal discrepancy between them.

Written 2026-08-24 by consolidating two sources that covered the same ground: the
provenance block from `scripts/spatial/spatial_utils.R` and the "Spatial analysis"
section of README.md. The script now carries only what a code reader needs and
points here for the rest.

**Working CRS: EPSG:26946** (NAD83 / California zone 6, meters). Every layer is
reprojected to it on load. As of 2026-06-24 the shapefiles on disk are already
saved natively in EPSG:26946, so the `st_transform()` calls in `spatial_utils.R`
are a defensive no-op kept in case that changes.

---

## 1. Where each layer comes from

============================================================
cabr_boundary       : NPS Land Resources Division tract/boundary data,
                      UNIT_CODE == "CABR". Single dissolved polygon,
                      ~160 acres, matches NPS-published CABR acreage.
                      Unmodified authoritative source. File lives in
                      boundaries/cabr/.

cabr_survey_box     : CUSTOM hand-drawn rectangle (2026-06-22), built
                      in ArcGIS Pro to extend past cabr_boundary on
                      the north, south, and southeast -- generous on
                      every side, not a uniform/formula-based buffer.
                      This is the actual CABR-tier inclusion geometry
                      for spatial joins (NOT cabr_boundary itself).
                      Verified via st_contains() below to fully
                      contain cabr_boundary. File lives in
                      boundaries/cabr/ (alongside cabr_boundary).

point_loma_boundary : City of San Diego Community Plan district
                      "PENINSULA" (CPCODE 30), re-downloaded fresh
                      from the City's open data portal on 2026-06-24.
                      Unmodified authoritative source -- this REPLACES
                      an earlier hand-edited + 5m-buffered version
                      used in prior sessions (see superseded note
                      below). File lives in boundaries/point_loma/.

                      SUPERSEDED 2026-06-22 approach: that version
                      hand-edited the city's PENINSULA polygon and
                      added a 5m seam buffer to force full containment
                      of cabr_boundary. As of 2026-06-24 we no longer
                      do this -- see "Known issue" section below for
                      why, and for the current, non-destructive
                      handling of the same underlying discrepancy.

sd_county_boundary  : NOTE -- this is NOT the raw County of San Diego
                      boundary on its own. As of 2026-06-24 this file
                      is a DISSOLVED UNION of the original SD County
                      boundary + point_loma_boundary, built in ArcGIS
                      Pro (Union, then Dissolve with no fields
                      selected, producing a single coverage polygon).
                      The original county-only polygon was not
                      preserved separately. A 1m buffer is applied on
                      load (see load section below) to absorb ~0.0001
                      acres of topological noise left by the dissolve
                      -- not a real gap, confirmed via
                      diagnose_sd_county_gap.R.

                      Practical effect: because point_loma_boundary
                      is now literally unioned into sd_county_boundary,
                      "point_loma_boundary completely within
                      sd_county_boundary" is true by construction, not
                      a fact about two independently-sourced
                      boundaries. Keep this in mind if this check is
                      ever used to validate something else -- it
                      won't catch a real future county/Point Loma
                      mismatch the way it would have when
                      sd_county_boundary was the raw county file.
                      File lives in boundaries/san_diego_county/.

---

## 2. Known issue — CABR boundary extends beyond Point Loma / SD County

Confirmed 2026-06-24 in ArcGIS Pro (Select By Location, "Completely
within", all layers standardized to EPSG:26946 first):

  point_loma_boundary WITHIN sd_county_boundary  -> PASS
    (true by construction -- see provenance note above)
  cabr_boundary       WITHIN point_loma_boundary -> FAIL
  cabr_boundary       WITHIN sd_county_boundary  -> FAIL

Both failures trace to the same cause: cabr_boundary (NPS authoritative
source) extends slightly into the water/coastline beyond where
point_loma_boundary and sd_county_boundary (City/County authoritative
sources) draw the coastline. This is treated as an EXPECTED feature of
independently-digitized boundary data, not an error.

Decision (2026-06-24): do NOT edit point_loma_boundary or
sd_county_boundary to force containment of cabr_boundary, and do NOT
clip cabr_boundary to fit inside them. All three are kept as
unmodified authoritative sources (sd_county_boundary's Point-Loma-union
status aside -- see provenance note). If acreage totals across
CABR/Point Loma/County tiers don't reconcile exactly, this coastal
discrepancy is the expected explanation, not a data error.

This supersedes the 2026-06-22 approach of hand-editing
point_loma_boundary + applying a 5m seam buffer to force containment.
The three checks below are informational (message(), not warning())
because failing is the known, correct state for two of them.

Separately, per Taro (2026-06-22): the BST transect begins on
Navy-owned land south of the official CABR (NPS) boundary, but this
area has historically been surveyed as part of CABR and should be
counted as such. cabr_survey_box (see provenance above) is the actual
CABR-tier inclusion geometry for this reason -- generous on every
side, not just south. cabr_boundary is layered on top purely as a
provenance label (inside_nps_boundary TRUE/FALSE), never as a filter.

---

## 3. How the layers are used in analysis

Transect buffers are generated in R via `scripts/spatial/spatial_utils.R`. Default = 10m. To change:

```r
buffer_dist_m <- 5  # change this one line in spatial_utils.R
```

CRS: EPSG:26946 (NAD83 / California zone 6, meters).

### Boundary layers

| File | Location | Source | Notes |
|------|----------|--------|-------|
| `cabr_boundary_nps_official.shp` | `boundaries/cabr/nps_official/` | NPS Land Resources Division (UNIT_CODE = CABR) | Official monument boundary (~160 acres). Used as a provenance label only — not a spatial filter. |
| `cabr_tracts_nps_official.shp` | `boundaries/cabr/nps_official/` | NPS Land Resources Division | Official NPS tract boundaries. |
| `cabr_survey_box.shp` | `boundaries/cabr/` | Hand-drawn in ArcGIS Pro | The actual CABR-tier inclusion geometry. See below. |
| `point_loma_boundary.shp` | `boundaries/point_loma/` | City of San Diego "PENINSULA" community plan district (CPCODE 30) | Unmodified authoritative source. |
| `sd_county_boundary.shp` | `boundaries/san_diego_county/` | County of San Diego Open Data Portal, unioned with `point_loma_boundary` then dissolved | **Not the raw county boundary alone** — a single dissolved polygon covering County + Point Loma combined. Original county-only file not preserved. |

All shapefiles are in EPSG:26946. `spatial_utils.R` calls `st_transform()` on load as a defensive no-op.

**`sd_county_boundary.shp` note:** because this is a Union+Dissolve of county + Point Loma, "Point Loma within SD County" is true by construction. If you ever need the unmodified county boundary, re-download it separately.

**1m noise buffer:** the Union+Dissolve left microscopic slivers along the Point Loma coastline (~4 sq ft total) — floating-point noise, not a real gap, but enough to make `st_contains()` return `FALSE`. `spatial_utils.R` applies a 1m buffer to `sd_county_boundary` on load to absorb this. See `diagnose_county_gap.R` for details.

### CABR survey area vs. official NPS boundary

The BST transect begins on Navy-owned land south of the official CABR (NPS) boundary, but this area is surveyed as part of CABR (per Taro, 2026-06-22).

`cabr_survey_box` is a hand-drawn polygon (not a formula buffer) extending past `cabr_boundary` on the north, south, and southeast. It is the actual CABR-tier inclusion geometry for spatial joins — not `cabr_boundary`. `cabr_boundary` is a provenance label only: every point gets an `inside_nps_boundary` TRUE/FALSE flag, but this never excludes a point from being counted as CABR.

`spatial_utils.R` runs `st_contains(cabr_survey_box, cabr_boundary)` on every load to confirm the box still fully contains the official boundary.

### Known coastal discrepancy

All boundary layers are standardized to EPSG:26946. Three containment checks run in ArcGIS Pro:

| Check | Result |
|---|---|
| `point_loma_boundary` within `sd_county_boundary` | **PASS** (true by construction) |
| `cabr_boundary` within `point_loma_boundary` | **FAIL** |
| `cabr_boundary` within `sd_county_boundary` | **FAIL** |

Both failures occur because `cabr_boundary` (NPS source) extends slightly into the water beyond where the city and county draw the coastline. This is an expected feature of independently-digitized boundaries, not an error. None of the three boundaries have been edited to force containment. If acreage totals don't reconcile exactly across tiers, this coastal overlap is the expected explanation.

`spatial_utils.R` reports all three checks via `message()` (not `warning()`) on every load, since FAIL is the known correct state for two of them.

---
