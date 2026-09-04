# Spatial layers

Where each shapefile came from, and what is known to be wrong with it.
*(How map pages are built and published: `WEBSITE_GUIDE.md`.)*

**Working CRS: EPSG:26946** — NAD83 / California zone 6, metres. Everything is
reprojected on load; the files on disk are already saved in it, so the transform
is a defensive no-op.

## The layers

| Layer | Source | Note |
|---|---|---|
| `cabr_boundary` | NPS Land Resources Division, `UNIT_CODE == "CABR"` | Authoritative, unmodified. ~160 acres. |
| **`cabr_survey_box`** | **Hand-drawn in ArcGIS Pro (2026-06-22)** | **This is the real CABR inclusion geometry** — not `cabr_boundary`. A generous rectangle extending north, south and southeast. |
| `point_loma_boundary` | City of San Diego Community Plan district "PENINSULA" (CPCODE 30) | |
| `sd_county_boundary` | County + Point Loma, Union + Dissolve | So "Point Loma within SD County" is always true |
| `cabr_bee_transects` | Drawn in ArcGIS Pro | The four survey lines |
| `cabr_survey_access_routes` | Drawn in ArcGIS Pro | Humphreys Rd — used to mark walk-in observations as non-survey |

## The nesting

```
  San Diego County
    └── Point Loma
          └── cabr_survey_box     <-- inclusion geometry for CABR
                └── cabr_boundary <-- the official NPS line
                      └── transects
```

## Two known quirks

| | |
|---|---|
| **CABR reaches just past the Point Loma / County lines** | A coastal discrepancy between an NPS boundary and a city plan district. Expected, not an error — the pipeline prints a note about it. |
| **1 m noise buffer on the county layer** | The Union + Dissolve left slivers of a few square feet along the coastline. The buffer swallows them. |

## Buffers are computed, never stored

Every buffer this project uses is generated in R at the moment it is needed —
there is no `Buffer_10m` shapefile and there should not be. A stored buffer is a
derived layer that silently goes stale when the source it came from is edited.

**Do not commit `Buffer_10m.*` or any other derived shapefile.** Only the source
layers under `data/spatial/shapefiles/` belong in git.

## Where it is used

| Job | Layer |
|---|---|
| Is this record at CABR? | `cabr_survey_box` |
| Which transect is it on? | `cabr_bee_transects` — nearest line within **50 m** (`IBC_OFF_TRANSECT_M`). Casual iNaturalist coordinates drift to hundreds of metres, so a tighter buffer would drop real survey records. |
| Is it a walk-in, not a survey? | `cabr_survey_access_routes` |
| County / Point Loma tiers | the two boundary layers |
