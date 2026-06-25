# =============================================================
# SD County / Point Loma / CABR Native Bee Checklists
# Created: June 11, 2026
# Updated: June 24, 2026 — combined into ONE script covering BOTH
#          tiers: TIER 1 (iNat-only, taxon_id-keyed) AND TIER 2
#          (merged: iNat + CABR specimens, Holway-format columns,
#          plus a Holway cross-check for SD County). Previously these
#          were two separate scripts (native_bee_checklist.R and a
#          draft native_bee_checklist_v2.R); per project decision,
#          all checklist-building now lives in this one file, run
#          top to bottom.
# Author: Brandi Sanchez
# Data: iNaturalist export, San Diego County 25 Mile Buffer, all
#       years, all quality grades, Anthophila (excl. Apis mellifera)
# Description: PART A builds THREE Tier 1 checklists of unique native
#              bee taxa -- SD County, Point Loma, and CABR -- each
#              derived by spatially filtering the same underlying iNat
#              observations before deduplicating to unique taxa.
#              Subgenus, complex name, and complex taxon_id are
#              pulled from the iNaturalist API ONCE for the full set
#              of taxa across all three tiers (not once per tier),
#              since the API cost is identical either way and a
#              single county-wide pass is far cheaper than three.
#              taxon_id is the stable key throughout PART A.
#
#              PART B builds THREE Tier 2 checklists -- the merged,
#              Holway-format versions, folding in CABR specimen
#              evidence and a Holway cross-check for SD County. See
#              PART B header below for full details.
#
# GENUS-REQUIRED RULE (applies to BOTH tiers, 2026-06-24): a row only
# belongs in any checklist here if genus is populated and
# non-empty. An observation/specimen identified no further than a
# higher rank (family, tribe, etc.) with no genus at all should have
# been identified further by someone manually -- it does not get its
# own placeholder row in either tier's checklist. This replaces an
# earlier version of PART A that incorrectly kept a row if EITHER
# genus OR species was present.
#
# Geographic tier definitions (see spatial_utils.R for full
# provenance/history on each boundary):
#   SD County : sd_county_boundary -- as of 2026-06-24 this is a
#               dissolved Union of the original county boundary +
#               point_loma_boundary, NOT the raw county extent alone.
#               See spatial_utils.R provenance notes before assuming
#               this tier is "no spatial filter."
#   Point Loma: point_loma_boundary -- unmodified City of San Diego
#               "PENINSULA" district (CPCODE 30), re-downloaded fresh
#               2026-06-24. No buffer applied (an earlier version used
#               a 5m seam buffer; that approach was dropped -- see
#               spatial_utils.R provenance notes).
#   CABR      : cabr_survey_box (NOT cabr_boundary -- the survey box
#               is the actual CABR-tier inclusion geometry; see
#               spatial_utils.R header notes)
#
# Column naming convention (PART A / Tier 1):
#   taxon_*_name columns follow iNat rank naming throughout.
#   scientific_name is the ONLY column with full "Genus species" format.
#   All other rank columns contain the epithet or rank name only.
#
# Complex handling (PART A / Tier 1):
#   - complex : name of the complex (e.g. "Andrena osmioides")
#   - complex_taxon_id   : iNat taxon_id of the complex itself
#   - Complexes are NOT excluded from richness counts
#   - complex is the join key for matching against specimen data
#   - A taxon IS a complex when taxon_id == complex_taxon_id
# =============================================================

# =============================================================
# PART A: TIER 1 (iNat-only) checklists
# =============================================================

# Run once to install, then leave commented:
# install.packages(c("tidyverse", "httr2", "stringr", "sf"))
library(tidyverse)
library(httr2)
library(stringr)
library(sf)

source("scripts/utils.R")        # read_latest()
source("scripts/spatial_utils.R") # cabr_survey_box, point_loma_boundary, sd_county_boundary

# ------------------------------------------------------------
# STEP 1: Load iNaturalist export (auto-detects newest file)
# ------------------------------------------------------------
bees_path <- read_latest(
  "data/reference_exports/native_bees",
  "^inat_native_bees_sdcounty"
)
cat("Loading:", basename(bees_path), "\n")
bees <- read.csv(bees_path)
cat("Loaded", nrow(bees), "observations\n")

# ------------------------------------------------------------
# STEP 2: Spatially split observations into the three tiers.
#
# Uses public latitude/longitude (not private_*) -- same fields the
# rest of the pipeline treats as authoritative. Rows with missing
# coordinates can't be spatially placed and are dropped from ALL
# three tiers (reported below), not silently kept in one.
# ------------------------------------------------------------
n_missing_coords <- sum(is.na(bees$latitude) | is.na(bees$longitude))
if (n_missing_coords > 0) {
  cat(sprintf("\nNOTE: %d observation(s) missing lat/long -- excluded from all spatial tiers.\n",
              n_missing_coords))
}

bees_sf <- bees %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(PROJECT_CRS)  # PROJECT_CRS (26946) comes from spatial_utils.R

filter_to_boundary <- function(points_sf, boundary, label) {
  inside <- st_within(points_sf, boundary, sparse = FALSE)[, 1]
  result <- points_sf %>% filter(inside) %>% st_drop_geometry()
  cat(sprintf("%-12s: %d of %d observations fall inside the boundary\n",
              label, nrow(result), nrow(points_sf)))
  result
}

cat("\n--- Spatial split ---\n")
bees_sd_county  <- filter_to_boundary(bees_sf, sd_county_boundary,  "SD County")
bees_point_loma <- filter_to_boundary(bees_sf, point_loma_boundary, "Point Loma")
bees_cabr       <- filter_to_boundary(bees_sf, cabr_survey_box,     "CABR")

# ------------------------------------------------------------
# STEP 3: Build initial checklist of unique taxa (by taxon_id),
# generically, so the same logic runs once per tier.
# ------------------------------------------------------------
build_checklist <- function(obs_df, label) {
  before_genus_filter <- obs_df %>%
    select(
      taxon_id,
      scientific_name,
      common_name,
      kingdom,
      phylum,
      class,
      order,
      superfamily,
      family,
      subfamily,
      tribe,
      subtribe,
      genus,
      species,
      subspecies
    ) %>%
    distinct(taxon_id, .keep_all = TRUE)

  n_before <- nrow(before_genus_filter)

  # GENUS REQUIRED (2026-06-24 fix): a row only belongs in the
  # checklist if genus is populated. Previously this used
  # !is.na(genus) | !is.na(species) (genus OR
  # species), which incorrectly kept rows identified only to a
  # higher rank (family, tribe, etc.) with no genus at all. Per
  # project decision: an observation/specimen identified no further
  # than genus or above should have been identified further by
  # someone manually before counting as a checklist record, so it's
  # excluded here rather than appearing as its own placeholder row.
  result <- before_genus_filter %>%
    filter(!is.na(genus), genus != "") %>%
    arrange(family, genus, species)

  n_dropped <- n_before - nrow(result)
  cat(sprintf("%-12s: %d unique taxa before genus filter, %d dropped (no genus -- identified no further than family/tribe/order/etc.), %d remain\n",
              label, n_before, n_dropped, nrow(result)))

  result
}

cat("\n--- Genus-required filter (PART A) ---\n")
checklist_sd_county  <- build_checklist(bees_sd_county,  "SD County")
checklist_point_loma <- build_checklist(bees_point_loma, "Point Loma")
checklist_cabr       <- build_checklist(bees_cabr,       "CABR")


cat("\n--- Unique taxa per tier ---\n")
cat("SD County :", nrow(checklist_sd_county), "\n")
cat("Point Loma:", nrow(checklist_point_loma), "\n")
cat("CABR      :", nrow(checklist_cabr), "\n")

# ------------------------------------------------------------
# STEP 4: Pull subgenus, complex, and
#         complex_taxon_id from the iNaturalist API -- ONCE, for
#         the union of taxon_ids across all three tiers. Point Loma
#         and CABR taxa are subsets of the SD County taxon set (since
#         both are geographic subsets of the county), so in practice
#         this just means SD County's unique IDs, but the union is
#         computed explicitly in case that assumption ever breaks
#         (e.g. boundary edits, future county boundary changes).
#
# Uses /v1/taxa/{id} — returns full ranked ancestors.
# Complex handling:
#   - If the taxon ITSELF has rank "complex", it is flagged directly
#     (taxon_id == complex_taxon_id)
#   - If a complex appears in ancestors, it is captured there
#   - subgenus and complex are kept in separate columns
# ------------------------------------------------------------
get_subgenus_and_complex <- function(taxon_id, max_retries = 3) {
  for (attempt in 1:max_retries) {
    Sys.sleep(0.5)  # be polite to the API

    result <- tryCatch({
      resp <- request(paste0("https://api.inaturalist.org/v1/taxa/", taxon_id)) %>%
        req_timeout(10) %>%  # if the API doesn't respond within 10s, fail and retry
        req_perform() %>%
        resp_body_json()

      taxon     <- resp$results[[1]]
      ancestors <- taxon$ancestors

      subgenus <- NA_character_
      complex  <- NA_character_
      complex_taxon_id    <- NA_integer_

      # Check if the taxon ITSELF is a complex rank
      if (!is.null(taxon$rank) && taxon$rank == "complex") {
        complex <- taxon$name
        complex_taxon_id   <- as.integer(taxon$id)
      }

      # Walk ancestors for subgenus and complex
      for (a in ancestors) {
        if (!is.null(a$rank)) {
          if (a$rank == "subgenus" && is.na(subgenus)) {
            subgenus <- a$name
          }
          if (a$rank == "complex" && is.na(complex)) {
            complex <- a$name
            complex_taxon_id   <- as.integer(a$id)
          }
        }
      }

      tibble(
        taxon_id            = taxon_id,
        subgenus = subgenus,
        complex  = complex,
        complex_taxon_id    = complex_taxon_id,
        fetch_failed        = FALSE
      )

    }, error = function(e) NULL)  # NULL signals failure, triggers retry

    if (!is.null(result)) return(result)

    if (attempt < max_retries) {
      Sys.sleep(2)  # back off longer before retrying
    }
  }

  # All retries exhausted — log this taxon_id as a real failure
  cat(sprintf("\n  WARNING: taxon_id %s failed after %d attempts\n", taxon_id, max_retries))
  tibble(
    taxon_id            = taxon_id,
    subgenus = NA_character_,
    complex  = NA_character_,
    complex_taxon_id    = NA_integer_,
    fetch_failed        = TRUE
  )
}

unique_ids <- unique(c(
  checklist_sd_county$taxon_id,
  checklist_point_loma$taxon_id,
  checklist_cabr$taxon_id
))
unique_ids <- unique_ids[!is.na(unique_ids)]

cat("\nFetching subgenus and complex from iNaturalist API for",
    length(unique_ids), "taxa (union across all three tiers)...\n")
cat("Estimated time:", round(length(unique_ids) * 0.5 / 60, 1), "minutes\n\n")

# Run with progress indicator
taxonomy_lookup <- map_dfr(
  seq_along(unique_ids),
  function(i) {
    cat(sprintf("\r  Progress: %d / %d taxa (%.0f%%)",
                i, length(unique_ids),
                i / length(unique_ids) * 100))
    flush.console()
    get_subgenus_and_complex(unique_ids[[i]])
  }
)

cat("\n\nDone fetching taxonomy data!\n")

failed_fetches <- taxonomy_lookup %>% filter(fetch_failed == TRUE)
if (nrow(failed_fetches) > 0) {
  cat("\n*** WARNING:", nrow(failed_fetches),
      "taxa failed to fetch from the API after retries. ***\n")
  cat("*** Results below are INCOMPLETE for these taxa — rerun or investigate. ***\n")
  print(failed_fetches %>% select(taxon_id))
} else {
  cat("\nAll", nrow(taxonomy_lookup), "taxa fetched successfully — no API failures.\n")
}

# ------------------------------------------------------------
# STEP 5: Join taxonomy_lookup onto each tier, parse species and
#         subspecies to epithet only, and finalize column order.
#         Same logic per tier, applied generically.
# ------------------------------------------------------------
finalize_checklist <- function(checklist) {
  checklist %>%
    left_join(taxonomy_lookup, by = "taxon_id") %>%
    mutate(
      species    = word(species, -1),
      subspecies = word(subspecies, -1)
    ) %>%
    select(
      taxon_id,
      scientific_name,
      common_name,
      kingdom,
      phylum,
      class,
      order,
      superfamily,
      family,
      subfamily,
      tribe,
      subtribe,
      genus,
      subgenus,
      complex,   # join key for specimen matching
      complex_taxon_id,     # stable iNat reference for the complex
      species,
      subspecies
    )
}

checklist_sd_county  <- finalize_checklist(checklist_sd_county)
checklist_point_loma <- finalize_checklist(checklist_point_loma)
checklist_cabr       <- finalize_checklist(checklist_cabr)

# ------------------------------------------------------------
# STEP 6: Quality control checks (run per tier)
# ------------------------------------------------------------
run_qc <- function(checklist, label) {
  cat(sprintf("\n--- QUALITY CONTROL: %s ---\n", label))

  families <- checklist %>%
    filter(!is.na(family), family != "") %>%
    distinct(family) %>%
    arrange(family)

  cat("Families found:", nrow(families), "\n")
  print(families$family)

  complex_taxa <- checklist %>%
    filter(!is.na(complex_taxon_id) & taxon_id == complex_taxon_id)

  distinct_complexes <- checklist %>%
    filter(!is.na(complex)) %>%
    distinct(complex, complex_taxon_id)

  cat("Total unique taxa:             ", nrow(checklist), "\n")
  cat("Taxa with subgenus: ", sum(!is.na(checklist$subgenus)), "\n")
  cat("Taxa with complex:  ", sum(!is.na(checklist$complex)), "\n")
  cat("Distinct complexes represented:", nrow(distinct_complexes), "\n")
  cat("Genera represented:            ", n_distinct(checklist$genus), "\n")
}

run_qc(checklist_sd_county,  "SD County")
run_qc(checklist_point_loma, "Point Loma")
run_qc(checklist_cabr,       "CABR")

# Note on the SD County family-count check that used to live here:
# the old script asserted "expected 6 for San Diego." That assumption
# no longer auto-checks itself here since the SD County 25 Mile
# Buffer place filter (see README iNat export instructions) can in
# principle catch additional families just outside the literal county
# line. Eyeball the printed family list above instead of relying on a
# hardcoded count.

# ------------------------------------------------------------
# STEP 7: Save all three checklists as CSV.
#
# These are TIER 1 (iNat-only) outputs. The old single-file
# inat_bee_checklist / SD_inat_bee_checklist.csv from before
# 2026-06-23 no longer exists -- anything downstream referencing
# that name needs to point at one of the three files below instead.
# ------------------------------------------------------------
write.csv(checklist_sd_county,
          "data/outputs/SD_county_inat_native_bee_checklist.csv",
          row.names = FALSE)
write.csv(checklist_point_loma,
          "data/outputs/PL_inat_native_bee_checklist.csv",
          row.names = FALSE)
# RENAMED 2026-06-24 (was CABR_inat_native_bee_checklist.csv) -- part of
# a 3-file CABR-specific naming set (see PART B) so the iNat-only,
# specimen-only, and merged CABR checklists can be directly compared:
#   cabr_inat_bee_checklist_clean.csv     (this file -- iNat only)
#   cabr_specimen_bee_checklist_clean.csv (specimen only -- built in PART B)
#   cabr_full_bee_checklist_clean.csv     (merged -- built in PART B)
write.csv(checklist_cabr,
          "data/outputs/cabr_inat_bee_checklist_clean.csv",
          row.names = FALSE)

cat("\nThree checklists saved to data/outputs/:\n")
cat("  SD_county_inat_native_bee_checklist.csv\n")
cat("  PL_inat_native_bee_checklist.csv\n")
cat("  cabr_inat_bee_checklist_clean.csv (renamed 2026-06-24, was CABR_inat_native_bee_checklist.csv)\n")


# =============================================================
# PART B: TIER 2 (merged) checklists
# Created: 2026-06-24
# Description: Builds the TIER 2 (merged, no "inat" in the name)
#              checklists for all three geographic tiers, formatted
#              to LARGELY match Dr. Holway's "San Diego County Bee
#              Species Checklist v3" workbook column structure (Family /
#              Subfamily / Tribe / Genus / Subgenus / Species /
#              Authority / Recent survey / Museum Collection /
#              Literature / iNaturalist / Notes) -- WITH TWO ADDITIONS
#              Holway's sheet doesn't have: Complex (between Subgenus
#              and Species) and Subspecies (after Species). These carry
#              real taxonomic resolution we already have from PART A;
#              dropping them just to match Holway's exact column set
#              would throw away information for no reason. Plus one
#              additional column for SD County only: "Found in Holway
#              checklist?".
#
#              Complex values are prefixed "(Complex) " (e.g. "(Complex)
#              Diadasia australis") -- some complex names look exactly
#              like a binomial species name, so without a marker a
#              complex-level ID could be misread as a confirmed species.
#
#              Reuses checklist_sd_county / checklist_point_loma /
#              checklist_cabr built in PART A above -- does not
#              re-read those from disk, so PART A must run first in
#              this same session (it already has, since this is one
#              script run top to bottom).
#
# Evidence sources currently wired in:
#   iNaturalist   : from the PART A / Tier 1 checklists above.
#   Recent survey : CABR ONLY -- intern-collected specimens
#                   (cabr_bee_specimens_clean.csv). NOT museum
#                   specimens -- these have not been deposited at
#                   SDNHM yet (see README TODO: formal specimen
#                   deposit, Pam Horsley).
#
# NOT YET IMPLEMENTED (deliberately blank, not a confirmed negative):
#   Museum Collection : blank across ALL THREE tiers as of 2026-06-24.
#                        This does NOT mean "no museum specimens
#                        exist." SDNHM holds specimens for greater San
#                        Diego County generally, but that data is not
#                        yet downloaded/integrated into this pipeline
#                        (pending Dorey/GBIF integration -- see
#                        README). For Point Loma and CABR specifically,
#                        whether SDNHM holds any digitized records at
#                        all is an open question (pending Shahan,
#                        SDNHM, as of 2026-06-24). Treat blank here as
#                        "not yet checked," not "checked and absent."
#   Literature         : blank across all three tiers -- out of scope,
#                        no literature-survey data source in this
#                        pipeline.
#
# GENUS-MINIMUM RULE (2026-06-24, fixed): an earlier version of PART B
# additionally required a species epithet, dropping genus-only rows
# (e.g. a "Colletes" record with no species ID) under the mistaken
# assumption that matching Holway's species-level column LAYOUT meant
# Tier 2 itself had to be species-level only. That was wrong -- the
# genus-minimum rule (genus required, nothing more) applies everywhere
# in this pipeline, including PART B. Genus-only rows are kept here;
# Species/Subspecies/Complex are simply blank for those rows. The same
# fix was applied to the CABR specimen evidence join below (a
# genus-only specimen now counts as Recent survey evidence for a
# genus-only checklist row, instead of being silently excluded).
#
# Per-tier scope note: Point Loma and SD County have NO specimen data
# of any kind in this pipeline (the museum holds none beyond the CABR
# intern specimens, which are CABR-only). So "Recent survey" is blank
# for Point Loma and SD County -- only CABR gets X's in that column.
#
# Holway cross-check ("Found in Holway checklist?"):
#   Per project decision (2026-06-24), Holway's three sheets (Described
#   n=688, Tentative n=11, Unpublished n=18) are flattened into ONE
#   combined 717-name reference list (holway_v3_combined.csv) for this
#   check. We do NOT attempt to replicate Holway's Described/Tentative/
#   Unpublished split in our own output -- that taxonomic-certainty
#   judgment is out of scope for this project. A species is marked
#   "No" only if it's absent from ALL 717 combined names -- this is
#   intentionally permissive. Matching is done on genus + species
#   epithet text (the only key both systems share), with Holway's
#   "CF " (tentative) and "MSN ... sp. nov." (unpublished manuscript
#   name) qualifiers stripped before matching, so e.g. our "annectens"
#   correctly matches his "CF annectens".
#
#   This check ONLY runs for the SD County tier, per project decision
#   -- Holway's workbook is SD-County-wide with no Point Loma/CABR
#   breakdown, so a Point-Loma- or CABR-only absence wouldn't be a
#   meaningful gap to flag at that geographic scale.
#
# Output: data/outputs/cabr_full_bee_checklist_clean.csv      (renamed 2026-06-24, was CABR_native_bee_checklist.csv)
#         data/outputs/cabr_specimen_bee_checklist_clean.csv  (NEW 2026-06-24, specimen-only, see STEP 3b)
#         data/outputs/PL_native_bee_checklist.csv
#         data/outputs/SD_county_native_bee_checklist.csv
#         (SD County only) includes "Found in Holway checklist?" column
#
# Related CABR-specific file, built in PART A, renamed alongside these:
#         data/outputs/cabr_inat_bee_checklist_clean.csv (was CABR_inat_native_bee_checklist.csv)
# =============================================================

# ------------------------------------------------------------
# STEP 1: Load CABR intern specimens (CABR ONLY -- no specimen data
# exists for Point Loma or SD County in this pipeline).
# RENAMED 2026-06-24 (was data/outputs/CABR_bee_specimens_clean.csv,
# capital CABR) -- clean_specimens.R now saves lowercase, matching the
# other renamed cabr_*_clean.csv files.
# ------------------------------------------------------------
specimens_path <- "data/outputs/cabr_bee_specimens_clean.csv"
if (!file.exists(specimens_path)) {
  message("cabr_bee_specimens_clean.csv not found -- running clean_specimens.R to build it...")
  source("scripts/clean_specimens.R")
}

cabr_specimens <- read.csv(specimens_path)
require_columns(cabr_specimens,
                 c("order", "family", "subfamily",
                   "tribe", "genus", "subgenus",
                   "complex", "complex_taxon_id", "species", "subspecies"),
                 "cabr_specimens")

# GENUS-MINIMUM (2026-06-24): only genus is required here, matching the
# same rule applied throughout PART A/B -- a specimen identified only to
# genus (no species) still counts as Recent survey evidence for that
# genus-only checklist row. Previously this also required species,
# which would have silently excluded genus-only specimens from the
# Recent survey evidence join below.
cabr_specimen_species <- cabr_specimens %>%
  filter(!is.na(genus), genus != "") %>%
  distinct(genus, species) %>%
  mutate(has_cabr_specimen = TRUE)

cat(sprintf("\nCABR specimens: %d unique genus+species combinations with at least one specimen.\n",
            nrow(cabr_specimen_species)))

# ------------------------------------------------------------
# STEP 2: Load and flatten Holway's v3 checklist for the cross-check.
# Strip "CF " (tentative) and "MSN ... sp. nov." (unpublished
# manuscript name) qualifiers so species_raw becomes a clean epithet
# comparable to our species.
# ------------------------------------------------------------
holway_path <- "data/reference_exports/holway_2026/holway_v3_combined.csv"
holway_raw <- read.csv(holway_path)
require_columns(holway_raw, c("genus", "species_raw"), "holway_raw")

cat(sprintf("Holway v3 combined reference: %d total names (Described + Tentative + Unpublished).\n",
            nrow(holway_raw)))

holway_clean <- holway_raw %>%
  mutate(
    species_clean = species_raw %>%
      str_remove("^CF\\s+") %>%
      str_remove("^MSN\\s+") %>%
      str_remove("\\s+sp\\.\\s*nov\\.$") %>%
      str_trim(),
    match_key = paste(str_to_lower(genus), str_to_lower(species_clean), sep = "_")
  ) %>%
  filter(match_key != "_", !is.na(genus), genus != "")

holway_match_keys <- unique(holway_clean$match_key)

cat(sprintf("Holway reference: %d distinct genus+species match keys after qualifier stripping.\n",
            length(holway_match_keys)))

# ------------------------------------------------------------
# STEP 3: Build one merged Tier 2 checklist, given a Tier 1 checklist
# (from PART A above) and (optionally) a specimen species table. Same
# logic applied to all three tiers; specimen_species and
# run_holway_check are the only things that vary by tier.
# ------------------------------------------------------------
build_tier2_checklist <- function(tier1_checklist, specimen_species, run_holway_check, label) {

  # GENUS-ONLY ROWS KEPT (2026-06-24 fix): an earlier version of this
  # function required species too, dropping genus-only rows
  # (e.g. a "Colletes" record with no species ID) under the assumption
  # that Holway's checklist is species-level only. Per project decision,
  # the genus-required-minimum rule applies everywhere in this pipeline
  # -- genus-only rows are real, valid checklist entries (an
  # observation/specimen identified to Colletes and no further is still
  # evidence of Colletes at this site) and must NOT be silently dropped
  # just because Tier 2 mirrors Holway's column layout. PART A already
  # enforces genus-required; PART B adds no further row-level filtering.
  checklist <- tier1_checklist %>%
    mutate(
      match_key = paste(str_to_lower(genus), str_to_lower(species), sep = "_")
    )

  # Recent survey: CABR-specific specimen evidence. For tiers with no
  # specimen data, specimen_species is NULL and this column is all NA.
  if (!is.null(specimen_species)) {
    checklist <- checklist %>%
      left_join(
        specimen_species %>%
          mutate(match_key = paste(str_to_lower(genus), str_to_lower(species), sep = "_")) %>%
          select(match_key, has_cabr_specimen),
        by = "match_key"
      )
  } else {
    checklist$has_cabr_specimen <- NA
  }

  checklist <- checklist %>%
    mutate(
      Family            = family,
      Subfamily         = subfamily,
      Tribe             = tribe,
      Genus             = genus,
      Subgenus          = subgenus,
      # Complex (2026-06-24): prefixed "(Complex) " per project decision
      # -- some complex names are themselves valid-looking binomials
      # (e.g. "Diadasia australis"), so without a marker a complex-level
      # ID could be misread as a confirmed species ID. The prefix makes
      # that distinction visible directly in the cell.
      Complex           = ifelse(!is.na(complex) & complex != "",
                                  paste0("(Complex) ", complex),
                                  NA_character_),
      Species           = species,
      Subspecies        = subspecies,
      Authority         = NA_character_,  # not available from iNat/specimen data
      `Recent survey`   = ifelse(!is.na(has_cabr_specimen) & has_cabr_specimen, "X", NA_character_),
      `Museum Collection` = NA_character_,  # NOT YET IMPLEMENTED -- see PART B header notes
      Literature        = NA_character_,    # out of scope -- no literature source in this pipeline
      iNaturalist       = "X",  # every row here came from the PART A / Tier 1 checklist, so always present
      Notes             = NA_character_
    )

  # Holway cross-check -- SD County only, per project decision.
  #
  # Genus-only rows (no species) are now KEPT (see note above), but a
  # genus-only row has nothing meaningful to compare against Holway's
  # species-level list -- it's not "missing," the comparison just isn't
  # applicable at that resolution. Left BLANK (NA) for those rows rather
  # than "No", so a blank here doesn't get misread as a real gap.
  if (run_holway_check) {
    checklist <- checklist %>%
      mutate(`Found in Holway checklist?` = case_when(
        is.na(species) | species == "" ~ NA_character_,
        match_key %in% holway_match_keys                      ~ "Yes",
        TRUE                                                   ~ "No"
      ))

    n_missing <- sum(checklist$`Found in Holway checklist?` == "No", na.rm = TRUE)
    n_not_applicable <- sum(is.na(checklist$`Found in Holway checklist?`))
    cat(sprintf("%s: %d of %d species NOT found in Holway's combined v3 list (flagged for investigation); %d genus-only row(s) left blank (not applicable).\n",
                label, n_missing, nrow(checklist), n_not_applicable))
  }

  output_cols <- c("Family", "Subfamily", "Tribe", "Genus", "Subgenus", "Complex", "Species", "Subspecies",
                    "Authority", "Recent survey", "Museum Collection", "Literature", "iNaturalist", "Notes")
  if (run_holway_check) output_cols <- c(output_cols, "Found in Holway checklist?")

  checklist %>%
    select(all_of(output_cols)) %>%
    arrange(Family, Genus, Subgenus, Species)
}

# ------------------------------------------------------------
# STEP 3b: Build a SPECIMEN-only checklist (unique taxa, not raw
# specimens) -- cabr_specimen_bee_checklist_clean.csv. Added 2026-06-24 so
# the iNat-derived and specimen-derived checklists can be directly
# diffed against each other (e.g. "what species do we have a specimen
# for that iNat hasn't recorded, or vice versa"). Previously specimen
# data only existed as cabr_specimen_species, an in-memory 2-column
# helper table used solely for the Recent survey join below -- never
# saved as its own usable checklist.
#
# Column set is DIFFERENT from the Tier 1 iNat checklist on purpose --
# the specimen sheet itself doesn't track kingdom, phylum, class,
# superfamily, or subtribe (confirmed directly against the V10 specimen
# workbook column headers, 2026-06-24), so those columns are not
# fabricated here. complex_taxon_id IS included as of 2026-06-24 --
# clean_specimens.R now populates this via a match against the iNat
# checklist (previously the specimen sheet had no ID column at all,
# only the complex name). Kept in the native taxon_*_name convention
# (not Holway/Title Case) so it lines up cleanly, column-for-column,
# against cabr_inat_bee_checklist_clean.csv for comparison.
#
# Same genus-minimum rule as everywhere else: only genus is
# required; rows with no species survive as genus-only entries.
# ------------------------------------------------------------
build_specimen_checklist <- function(specimens_df) {
  specimen_cols <- c("order", "family", "subfamily",
                      "tribe", "genus", "subgenus",
                      "complex", "complex_taxon_id", "species", "subspecies")

  specimens_df %>%
    select(all_of(specimen_cols)) %>%
    distinct() %>%
    filter(!is.na(genus), genus != "") %>%
    arrange(family, genus, subgenus, species)
}

cabr_specimen_checklist <- build_specimen_checklist(cabr_specimens)
cat(sprintf("\ncabr_specimen_bee_checklist_clean: %d unique taxa (genus required, species optional) from CABR specimens.\n",
            nrow(cabr_specimen_checklist)))

write.csv(cabr_specimen_checklist,
          "data/outputs/cabr_specimen_bee_checklist_clean.csv",
          row.names = FALSE, na = "")

# ------------------------------------------------------------
# STEP 4: Build all three tiers.
# ------------------------------------------------------------
cat("\n--- Building Tier 2 merged checklists ---\n")

checklist_cabr_v2 <- build_tier2_checklist(
  checklist_cabr, cabr_specimen_species, run_holway_check = FALSE, label = "CABR"
)

checklist_point_loma_v2 <- build_tier2_checklist(
  checklist_point_loma, NULL, run_holway_check = FALSE, label = "Point Loma"
)

checklist_sd_county_v2 <- build_tier2_checklist(
  checklist_sd_county, NULL, run_holway_check = TRUE, label = "SD County"
)

# ------------------------------------------------------------
# STEP 5: Save.
# ------------------------------------------------------------
# RENAMED 2026-06-24 (was CABR_native_bee_checklist.csv) -- part of the
# same 3-file CABR naming set as cabr_inat_bee_checklist_clean.csv and
# cabr_specimen_bee_checklist_clean.csv above. "full" here means merged
# (iNat + specimen evidence combined), not "complete"/"final" in any
# other sense.
write.csv(checklist_cabr_v2,
          "data/outputs/cabr_full_bee_checklist_clean.csv",
          row.names = FALSE, na = "")
write.csv(checklist_point_loma_v2,
          "data/outputs/PL_native_bee_checklist.csv",
          row.names = FALSE, na = "")
write.csv(checklist_sd_county_v2,
          "data/outputs/SD_county_native_bee_checklist.csv",
          row.names = FALSE, na = "")

cat("\nTier 2 + specimen checklists saved to data/outputs/:\n")
cat("  cabr_inat_bee_checklist_clean.csv     (", nrow(checklist_cabr), "rows -- iNat only, see PART A )\n")
cat("  cabr_specimen_bee_checklist_clean.csv (", nrow(cabr_specimen_checklist), "rows -- specimen only )\n")
cat("  cabr_full_bee_checklist_clean.csv     (", nrow(checklist_cabr_v2), "rows -- merged, renamed 2026-06-24, was CABR_native_bee_checklist.csv )\n")
cat("  PL_native_bee_checklist.csv        (", nrow(checklist_point_loma_v2), "rows, includes any genus-only entries )\n")
cat("  SD_county_native_bee_checklist.csv (", nrow(checklist_sd_county_v2), "rows, includes any genus-only entries; includes Holway cross-check )\n")

cat("\nREMINDER: Museum Collection is blank across all Tier 2 outputs --\n")
cat("this is 'not yet checked', not a confirmed absence of specimens.\n")
cat("See PART B header notes before treating blank as a negative result.\n")
