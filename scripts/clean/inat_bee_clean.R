# =============================================================
# inat_bee_clean.R
# beescabr pipeline — non-lethal iNat bee data cleaning
# Author: Brandi Sanchez  |  Rewritten: 2026-07-10
#
# PRIMARY SOURCE: iNat export CSV (data/cabr_surveys/nonlethal/inat_bee/)
#   Has tag_list (comma-separated), all obs_field columns, lat/lng, and
#   full taxon_*_name hierarchy. Script filters to roster surveyors and
#   clips to the CABR survey box.
#
# LIVE SUPPLEMENT: iNat API
#   Fetches obs_ids NOT yet in the export (posted since last download).
#   Gives tags + basic obs data only. Obs_fields for taxon-type fields
#   (e.g. flower visited) are unreliable from the API and stay blank
#   until the next export. The export is the ground truth for obs_fields.
#
# TAXONOMY: data/outputs/reference/bee_taxonomy_lookup.csv
#   Built by native_bee_checklist.R. One row per taxon_id, all ranks
#   from epifamily through subspecies (including subgenus and complex
#   which the iNat export does not include). Run the checklist first.
#
# RUN ORDER (full pipeline):
#   1. native_bee_checklist.R  →  bee_taxonomy_lookup.csv
#   2. inat_bee_clean.R        →  cabr_inat_bee_clean.csv  (triage pass)
#   3. survey_dates.R          →  beeple/intern official date files
#   4. inat_bee_clean.R        →  cabr_inat_bee_clean.csv  (date recovery)
#
# Outputs:
#   data/outputs/inat_clean/cabr_inat_bee_clean.csv
#   data/outputs/inat_clean/qc/cabr_inat_bee_unknown_tags.csv
# =============================================================

library(tidyverse)
library(httr2)
library(sf)

source("scripts/utils/utils.R")
source("scripts/spatial/spatial_utils.R")

# ---- Config -----------------------------------------------------------------
info_dir       <- "data/project_info"
roster_path    <- list.files(info_dir, pattern = "^surveyors_by_year.*\\.csv$",   full.names = TRUE)[1]
crosswalk_path <- list.files(info_dir, pattern = "^project_tags_fields.*\\.csv$", full.names = TRUE)[1]
export_dir     <- "data/cabr_surveys/nonlethal/inat_bee"

taxonomy_path    <- "data/outputs/reference/bee_taxonomy_lookup.csv"
out_clean        <- "data/outputs/inat_clean/cabr_inat_bee_clean.csv"
out_unknown_tags <- "data/outputs/inat_clean/qc/cabr_inat_bee_unknown_tags.csv"

TAXON_BEES <- 630955   # Anthophila (all bees)
UA         <- "beescabr inat_bee_clean (brandirenesanchez16@gmail.com)"

# ---- Helpers ----------------------------------------------------------------

# Normalize a tag for matching: strip leading #, trim whitespace, lowercase.
norm_key <- function(x) tolower(gsub("^#", "", trimws(x)))

# write_csv version of write_fresh (overrides utils.R's write.csv version).
write_fresh_csv <- function(x, path, ...) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(path))       unlink(path, recursive = TRUE, force = TRUE)
  else if (file.exists(path)) unlink(path, force = TRUE)
  write_csv(x, path, ...)
}

if (!exists("cabr_survey_box"))
  stop("cabr_survey_box not found after sourcing spatial_utils.R")


# =============================================================
# 1. ROSTER + CROSSWALK
# =============================================================
roster <- read_csv(roster_path, show_col_types = FALSE)

# All non-lethal surveyors with an iNat username.
usernames <- roster |>
  filter(method == "non-lethal",
         !is.na(inaturalist_username), trimws(inaturalist_username) != "") |>
  pull(inaturalist_username) |>
  unique()

# Role lookup: intern / beeple / staff per username. Used as data_source.
role_lookup <- roster |>
  filter(inaturalist_username %in% usernames) |>
  distinct(inaturalist_username, role) |>
  group_by(inaturalist_username) |>
  summarise(data_source = paste(sort(unique(role)), collapse = "/"), .groups = "drop")

crosswalk <- read_csv(crosswalk_path, show_col_types = FALSE)

message(length(usernames), " surveyors in roster | ", nrow(crosswalk), " crosswalk rows")


# =============================================================
# 2. BUILD TAG MAP FROM CROSSWALK
# =============================================================
# Maps every known tag variant (including misspellings, case variants,
# # prefix variants) to its canonical name and category.
# Categories used in triage: Beeple, Intern, General → has_survey = TRUE
#                            Exclude                  → is_exclude = TRUE
#                            Transect                 → populates transect column
#                            Location                 → location hint only (not survey)
tag_map <- crosswalk |>
  filter(type == "tag") |>
  transmute(
    canonical = name,
    category,
    variants  = if_else(
      is.na(inat_variants) | trimws(inat_variants) == "",
      name,
      paste(name, inat_variants, sep = "; ")
    )
  ) |>
  separate_rows(variants, sep = ";\\s*") |>
  mutate(key = norm_key(variants)) |>
  # Drop empty, placeholder, and NA-string variants
  filter(
    nchar(key) > 0,
    !grepl("^\\(", key),          # "(none found in data)"
    !(key %in% c("n/a", "na"))    # literal "n/a" or NA coerced to string
  ) |>
  distinct(key, canonical, category)


# =============================================================
# 3. LOAD EXPORT → FILTER TO ROSTER → CLIP TO SURVEY BOX
# =============================================================
export_path <- read_latest(export_dir, "^inat_native_bees_sdcounty_25_mi_buffer")
message("\nLoading export: ", basename(export_path))

export_raw <- read_csv(export_path, show_col_types = FALSE, na = c("", "NA"))
message(sprintf("  %d total obs in export", nrow(export_raw)))

# Filter to roster surveyors only.
export_roster <- export_raw |> filter(user_login %in% usernames)
message(sprintf("  %d obs from roster surveyors", nrow(export_roster)))

# Clip to the precise CABR survey box polygon (see spatial_utils.R for details).
pts <- export_roster |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(st_crs(cabr_survey_box))

in_box <- lengths(st_within(pts, cabr_survey_box)) > 0

export_cabr <- pts |>
  filter(in_box) |>
  st_drop_geometry() |>
  rename(obs_id = id, observer = user_login) |>
  mutate(
    observed_date = as.Date(observed_on),
    # Fold all plant-association obs_fields → flower_visited (primary first).
    # field:interaction->visited flower of is the authoritative source;
    # older/alternative plant fields used as fallback only.
    flower_visited  = coalesce(
      `field:interaction->visited flower of`,
      `field:name of associated plant`,
      `field:nectar / pollen delivering plant`,
      `field:nectar plant`
    ),
    bee_behavior    = `field:bee behavior`,
    mating_behavior = `field:behavior: mating`,
    on_ground       = `field:on ground?`,
    # Fold nest/nesting into one column (field_ids 4837 and 988).
    nesting         = coalesce(`field:nest`, `field:nesting`),
    nesting_bee     = `field:nesting bee`,
    # Tags obs_field (field_id 20521): manual correction tool.
    # When Brandi needs to fix an obs with the wrong survey hashtag
    # (e.g. intern who used the beeple tag), she adds the correct tag
    # as the Tags obs_field value to override triage. Not the standard
    # method -- standard is always the hashtag.
    tags_override   = `field:tags`,
    data_from       = "export"
  )

message(sprintf("  %d obs inside CABR survey box", nrow(export_cabr)))


# =============================================================
# 4. TRIAGE FROM tag_list
# =============================================================
# tag_list in the export is comma-separated, e.g.:
#   "CabrilloBee10MinuteSurvey, Cabrillo2021BeeSurvey, UPMON"
# Split, normalize each tag, look up in tag_map.

# Reusable triage function used for both export and API obs.
# Input:  data frame with columns obs_id (integer) and tag_list (character)
# Output: list(triage = per-obs summary, unknown = unrecognized tags)
triage_from_tag_list <- function(id_tags_df, tmap) {

  # Expand comma-separated tag_list into one row per tag
  tags_long <- id_tags_df |>
    filter(!is.na(tag_list), nchar(trimws(tag_list)) > 0) |>
    mutate(tag = str_split(tag_list, ",\\s*")) |>
    unnest(tag) |>
    mutate(
      tag = trimws(tag),
      key = norm_key(tag)
    ) |>
    filter(nchar(key) > 0) |>
    left_join(tmap, by = "key")

  # Per-obs summary: survey membership, transect, year, type
  triage_summary <- tags_long |>
    group_by(obs_id) |>
    summarise(
      is_exclude  = any(category == "Exclude", na.rm = TRUE),
      exclude_tag = paste(sort(unique(na.omit(canonical[category == "Exclude"]))), collapse = "; "),

      has_survey  = any(category %in% c("Beeple", "Intern", "General"), na.rm = TRUE),
      survey_tags = paste(sort(unique(na.omit(canonical[category %in% c("Beeple","Intern","General")]))), collapse = "; "),

      # survey_year: extract 4-digit year from Beeple/Intern canonical names
      survey_year = {
        yr_names <- na.omit(canonical[category %in% c("Beeple", "Intern")])
        yr <- str_extract(yr_names, "\\d{4}")
        yr <- yr[!is.na(yr)]
        if (length(yr)) paste(sort(unique(yr)), collapse = "; ") else NA_character_
      },

      # survey_type: use category column (Intern -> "intern", Beeple -> "beeple")
      survey_type = {
        cats <- na.omit(category[category %in% c("Beeple", "Intern")])
        if (length(cats)) paste(sort(unique(tolower(cats))), collapse = "/") else NA_character_
      },

      # transect: canonical names for Transect-category tags
      transect = {
        tr <- sort(unique(na.omit(canonical[category == "Transect"])))
        if (length(tr)) paste(tr, collapse = "; ") else NA_character_
      },

      # 10-minute survey flag
      is_10min = any(canonical == "CabrilloBee10MinuteSurvey", na.rm = TRUE),

      .groups = "drop"
    )

  # Obs with empty/NA tag_list get no row in triage_summary.
  # Add those back in as "flag" so every obs gets a triage value.
  all_ids <- tibble(obs_id = id_tags_df$obs_id)
  triage_complete <- all_ids |>
    left_join(triage_summary, by = "obs_id") |>
    mutate(
      is_exclude  = coalesce(is_exclude, FALSE),
      has_survey  = coalesce(has_survey, FALSE),
      is_10min    = coalesce(is_10min,   FALSE)
    )

  # Tags not recognized by the crosswalk -- QC file to scan for typos/new years
  unknown <- tags_long |>
    filter(is.na(canonical), !is.na(tag), nchar(tag) > 0) |>
    count(tag, sort = TRUE, name = "n_obs")

  list(triage = triage_complete, unknown = unknown)
}

# Run triage on export
export_triage_result <- triage_from_tag_list(
  export_cabr |> select(obs_id, tag_list),
  tag_map
)
obs_triage   <- export_triage_result$triage
unknown_tags <- export_triage_result$unknown

# Write unknown tags QC file for review
dir.create(dirname(out_unknown_tags), recursive = TRUE, showWarnings = FALSE)
if (nrow(unknown_tags) > 0) {
  message(sprintf("\n%d tag(s) not in crosswalk -> %s", nrow(unknown_tags), out_unknown_tags))
  message("Scan for misspelled Cabrillo survey tags. Add to crosswalk inat_variants, then re-run.")
  write_fresh_csv(unknown_tags, out_unknown_tags, na = "")
} else {
  message("All tags recognized by crosswalk.")
}


# =============================================================
# 4a. TAGS OBS_FIELD OVERRIDE (field_id 20521)
# =============================================================
# When someone used the wrong survey hashtag (e.g. Jillian's beeple tag
# on what should be an intern obs), Brandi adds the correct tag as the
# "Tags" obs_field value on iNat. This block reads that field and uses
# it to override survey_type / survey_year / has_survey for the affected
# obs. Manual correction tool only -- NOT the standard survey method.
tags_override_triage <- export_cabr |>
  filter(!is.na(tags_override), nchar(trimws(tags_override)) > 0) |>
  select(obs_id, tags_override) |>
  mutate(key = norm_key(tags_override)) |>
  left_join(
    tag_map |> filter(category %in% c("Beeple", "Intern", "General")),
    by = "key"
  ) |>
  filter(!is.na(canonical)) |>
  transmute(
    obs_id,
    ov_has_survey  = TRUE,
    ov_survey_type = tolower(category),   # "beeple" or "intern" or "general"
    ov_survey_year = str_extract(canonical, "\\d{4}"),
    ov_survey_tags = canonical
  )

# Apply overrides: Tags obs_field wins for survey_type and survey_year.
obs_triage <- obs_triage |>
  left_join(tags_override_triage, by = "obs_id") |>
  mutate(
    has_survey_from_tags_field = coalesce(ov_has_survey, FALSE),
    has_survey  = has_survey | has_survey_from_tags_field,
    survey_type = if_else(!is.na(ov_survey_type), ov_survey_type, survey_type),
    survey_year = if_else(!is.na(ov_survey_year), ov_survey_year, survey_year),
    survey_tags = case_when(
      !is.na(ov_survey_tags) & (is.na(survey_tags) | survey_tags == "") ~
        ov_survey_tags,
      !is.na(ov_survey_tags) ~
        paste(survey_tags, ov_survey_tags, sep = "; "),
      TRUE ~ survey_tags
    )
  ) |>
  select(-starts_with("ov_"))

# Assign triage values now that overrides are folded in.
obs_triage <- obs_triage |>
  mutate(
    triage = case_when(
      is_exclude                 ~ "exclude",
      has_survey                 ~ "keep",
      TRUE                       ~ "flag"
    ),
    triage_reason = case_when(
      is_exclude                 ~ paste0("exclude tag: ", exclude_tag),
      has_survey_from_tags_field ~ paste0(
                                      "valid survey tag via Tags obs_field override: ",
                                      coalesce(survey_tags, "")
                                    ),
      has_survey                 ~ "valid Cabrillo survey tag",
      TRUE                       ~ "no recognized survey tag -- review"
    )
  )


# =============================================================
# 5. iNAT API: LIVE SUPPLEMENT
# =============================================================
# Fetch all non-lethal roster obs within the CABR bounding box, then
# keep only obs_ids NOT already in the export. These are observations
# posted since the last export download -- typically very few or none
# if the export is recent.
message("\n--- API fetch (supplement to export) ---")

bb <- cabr_survey_box |> st_transform(4326) |> st_bbox()

api_params <- list(
  user_id  = paste(usernames, collapse = ","),
  swlat    = unname(bb["ymin"]),
  swlng    = unname(bb["xmin"]),
  nelat    = unname(bb["ymax"]),
  nelng    = unname(bb["xmax"]),
  taxon_id = TAXON_BEES,
  per_page = 200,
  order_by = "id",
  order    = "asc"
)

api_raw_obs  <- list()
api_raw_tags <- list()
id_above <- 0L
page     <- 0L

repeat {
  page <- page + 1L
  resp <- request("https://api.inaturalist.org/v1/observations") |>
    req_user_agent(UA) |>
    req_retry(
      max_tries    = 6,
      is_transient = \(r) resp_status(r) %in% c(429L, 500L, 502L, 503L, 504L),
      backoff      = \(i) min(60, 5 * 2^(i - 1))
    ) |>
    req_url_query(!!!api_params, id_above = id_above) |>
    req_perform() |>
    resp_body_json()

  results <- resp$results
  if (length(results) == 0L) break

  api_raw_obs[[page]] <- map_dfr(results, function(o) {
    coord <- o$geojson$coordinates
    tibble(
      obs_id               = as.integer(o$id),
      observer             = o$user$login %||% NA_character_,
      url                  = paste0("https://www.inaturalist.org/observations/", o$id),
      observed_on          = o$observed_on %||% NA_character_,
      latitude             = if (is.null(coord)) NA_real_ else coord[[2L]],
      longitude            = if (is.null(coord)) NA_real_ else coord[[1L]],
      quality_grade        = o$quality_grade %||% NA_character_,
      captive_cultivated   = isTRUE(o$captive),
      coordinates_obscured = isTRUE(o$obscured),
      taxon_id             = as.integer(o$taxon$id %||% NA_integer_),
      scientific_name      = o$taxon$name %||% NA_character_,
      common_name          = o$taxon$preferred_common_name %||% NA_character_
    )
  })

  api_raw_tags[[page]] <- map_dfr(results, function(o) {
    tg <- o$tags %||% list()
    if (length(tg) == 0L) return(NULL)
    tibble(
      obs_id = as.integer(o$id),
      # API tags can be plain strings OR list objects with a $name field
      tag = map_chr(tg, \(t) if (is.list(t)) (t$name %||% "") else as.character(t))
    )
  })

  last_id <- as.integer(results[[length(results)]]$id)
  message(sprintf("  page %d: %d obs (id up to %d)", page, length(results), last_id))
  id_above <- last_id
  if (length(results) < api_params$per_page) break
  Sys.sleep(1)
}

api_all_obs  <- if (length(api_raw_obs))  bind_rows(api_raw_obs)  else tibble()
api_all_tags <- if (length(api_raw_tags)) bind_rows(api_raw_tags) else tibble()

# Keep only obs NOT already in the export
new_ids <- setdiff(api_all_obs$obs_id, export_cabr$obs_id)

if (length(new_ids) > 0L) {
  message(sprintf("  %d new obs (not in export) -- clipping to survey box", length(new_ids)))

  api_new_sf <- api_all_obs |>
    filter(obs_id %in% new_ids, !is.na(latitude), !is.na(longitude)) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326L) |>
    st_transform(st_crs(cabr_survey_box))

  api_in_box  <- lengths(st_within(api_new_sf, cabr_survey_box)) > 0L
  api_new_obs <- api_new_sf |>
    filter(api_in_box) |>
    st_drop_geometry() |>
    mutate(observed_date = as.Date(observed_on), data_from = "api_only")

  message(sprintf("  %d new obs inside CABR survey box added from API", nrow(api_new_obs)))

  # Reconstruct tag_list string for API obs (for reference + triage)
  api_tag_strings <- api_all_tags |>
    filter(obs_id %in% api_new_obs$obs_id) |>
    group_by(obs_id) |>
    summarise(tag_list = paste(tag, collapse = ", "), .groups = "drop")

  # Triage API obs from their tags (no Tags obs_field available for API)
  api_triage_input <- tibble(obs_id = api_new_obs$obs_id) |>
    left_join(api_tag_strings, by = "obs_id")

  api_triage_result <- triage_from_tag_list(api_triage_input, tag_map)

  api_obs_triage <- api_triage_result$triage |>
    mutate(
      has_survey_from_tags_field = FALSE,
      triage = case_when(
        is_exclude ~ "exclude",
        has_survey ~ "keep",
        TRUE       ~ "flag"
      ),
      triage_reason = case_when(
        is_exclude ~ paste0("exclude tag: ", exclude_tag),
        has_survey ~ "valid Cabrillo survey tag",
        TRUE       ~ "no recognized survey tag -- review"
      )
    )

  api_new_obs <- api_new_obs |>
    left_join(role_lookup,     by = c("observer" = "inaturalist_username")) |>
    left_join(api_tag_strings, by = "obs_id") |>
    left_join(api_obs_triage,  by = "obs_id")

} else {
  message("  No new obs from API -- export is fully current")
  api_new_obs <- tibble()
}


# =============================================================
# 6. MERGE EXPORT + API
# =============================================================
# Export is the ground truth for obs_fields. API-only obs get NA for
# all obs_field columns (flower_visited, bee_behavior, etc.) -- they
# will fill in on the next export refresh.
export_core <- export_cabr |>
  left_join(role_lookup, by = c("observer" = "inaturalist_username")) |>
  left_join(obs_triage,  by = "obs_id") |>
  mutate(
    # Coalesce triage defaults for obs that had no tags at all
    is_exclude  = coalesce(is_exclude,  FALSE),
    has_survey  = coalesce(has_survey,  FALSE),
    is_10min    = coalesce(is_10min,    FALSE),
    triage      = coalesce(triage,      "flag"),
    triage_reason = coalesce(triage_reason, "no tags on observation -- review"),
    has_survey_from_tags_field = coalesce(has_survey_from_tags_field, FALSE)
  )

# bind_rows fills in NA for columns present in one source but not the other
all_obs <- bind_rows(export_core, api_new_obs)

message(sprintf(
  "\nMerged: %d obs total (%d export + %d API-only)",
  nrow(all_obs), nrow(export_core), nrow(api_new_obs)
))


# =============================================================
# 7. TAXONOMY JOIN
# =============================================================
# bee_taxonomy_lookup.csv is built by native_bee_checklist.R.
# It covers all ranks from epifamily through subspecies, and includes
# subgenus and complex (which the iNat CSV export does NOT include).
# Join key: taxon_id (integer).
# For obs the lookup doesn't cover, fall back to the export's
# taxon_*_name columns.
if (!file.exists(taxonomy_path)) {
  message("\nWARNING: bee_taxonomy_lookup.csv not found at '", taxonomy_path, "'")
  message("Run native_bee_checklist.R first to build it.")
  message("Proceeding with export taxon_*_name columns only (no subgenus/complex).")
  tax <- tibble(
    taxon_id   = integer(),
    rank       = character(),
    family     = character(), subfamily  = character(),
    tribe      = character(),
    genus      = character(), subgenus   = character(),
    complex    = character(), species    = character(),
    subspecies = character()
  )
} else {
  tax <- read_csv(taxonomy_path, show_col_types = FALSE) |>
    select(taxon_id, rank, family, subfamily, tribe,
           genus, subgenus, complex, species, subspecies)
  message(sprintf("Taxonomy lookup: %d taxa loaded", nrow(tax)))
}

all_obs <- all_obs |>
  left_join(tax, by = "taxon_id") |>
  mutate(
    # Lookup wins (has subgenus/complex/correct tribe); export taxon_*_name
    # columns fill in for obs the lookup doesn't yet cover (higher-rank obs
    # added since the last checklist run, or if checklist hasn't been run yet).
    family     = coalesce(family,     taxon_family_name),
    subfamily  = coalesce(subfamily,  taxon_subfamily_name),
    tribe      = coalesce(tribe,      taxon_tribe_name),
    genus      = coalesce(genus,      taxon_genus_name),
    species    = coalesce(species,    taxon_species_name),
    subspecies = coalesce(subspecies, taxon_subspecies_name)
    # subgenus and complex: lookup only (not in iNat export)
  )


# =============================================================
# 8. ASSEMBLE FINAL CLEAN OUTPUT
# =============================================================
clean <- all_obs |>
  mutate(missing_coords = is.na(latitude) | is.na(longitude)) |>
  select(
    # --- Identity ---
    obs_id,
    url,
    observer,
    data_source,          # intern / beeple / staff (from roster role)
    observed_date,
    latitude,
    longitude,
    quality_grade,
    captive_cultivated,
    coordinates_obscured,
    missing_coords,

    # --- Triage ---
    triage,               # keep / flag / exclude / recovered_by_date (Step 9)
    triage_reason,
    has_survey_from_tags_field,  # TRUE = rescued via Tags obs_field override
    survey_year,
    survey_type,          # beeple / intern / general
    transect,             # TP / UPMON / BST / OT (from tags)
    is_10min,             # CabrilloBee10MinuteSurvey flag

    # --- Taxonomy (from lookup + export fallback) ---
    taxon_id,
    scientific_name,
    common_name,
    rank,
    family,
    subfamily,
    tribe,
    genus,
    subgenus,             # lookup only (not in export)
    complex,              # lookup only (not in export)
    species,
    subspecies,

    # --- Observation fields (export only; NA for API-only obs) ---
    flower_visited,       # coalesced from all plant-interaction fields
    bee_behavior,
    on_ground,
    nesting,              # coalesced from field:nest + field:nesting
    nesting_bee,
    mating_behavior,

    # --- Raw fields ---
    tag_list,
    description,
    data_from             # "export" or "api_only"
  )

write_fresh_csv(clean, out_clean, na = "")

message("\n========== CLEAN SUMMARY ==========")
message(sprintf("Total obs: %d", nrow(clean)))
message("\nBy triage:"); print(count(clean, triage, sort = TRUE))
message("\nBy data_source:"); print(count(clean, data_source, sort = TRUE))
message("\nBy survey_year:"); print(count(clean, survey_year, sort = TRUE))
message(sprintf("\nSaved -> %s", out_clean))

if (nrow(unknown_tags) > 0) {
  message(sprintf(
    "\nACTION: %d unrecognized tag(s) -- check:\n  %s",
    nrow(unknown_tags), out_unknown_tags
  ))
  message("Scan for misspelled Cabrillo survey tags or new survey years.")
  message("Add to crosswalk inat_variants, then re-run this script.")
}


# =============================================================
# 9. DATE-BASED RECOVERY (optional second pass)
# =============================================================
# Only runs after survey_dates.R has produced the official date files.
# Adds on_survey_date to all obs and recovers "flag" obs that occurred
# on a confirmed survey date (most likely forgot the survey hashtag).
#
# To trigger: run survey_dates.R first, then re-run inat_bee_clean.R.
beeple_official <- "data/project_info/beeple_survey_dates_official.csv"
intern_official  <- "data/project_info/intern_survey_dates_official.csv"

if (file.exists(beeple_official) && file.exists(intern_official)) {
  message("\n--- Step 9: Date-based recovery ---")

  # Beeple confirmed dates: each confirmed/manual row has usernames per transect column
  TRANSECT_COLS <- c("OT", "TP1", "TP2", "UPMON", "BST")

  beeple_dates <- read_csv(beeple_official, show_col_types = FALSE) |>
    filter(status %in% c("confirmed", "manual")) |>
    select(date, any_of(TRANSECT_COLS)) |>
    pivot_longer(-date, names_to = "t", values_to = "username") |>
    filter(!is.na(username), !is.na(date)) |>
    transmute(username, date = as.Date(date))

  # Intern confirmed dates: each row has one or two usernames
  intern_dates <- read_csv(intern_official, show_col_types = FALSE) |>
    filter(status == "confirmed") |>
    select(date, any_of(c("username_1", "username_2"))) |>
    pivot_longer(-date, names_to = NULL, values_to = "username") |>
    filter(!is.na(username), !is.na(date)) |>
    transmute(username, date = as.Date(date))

  confirmed_events <- bind_rows(beeple_dates, intern_dates) |>
    distinct() |>
    mutate(on_survey_date = TRUE)

  clean <- clean |>
    left_join(confirmed_events,
              by = c("observer" = "username", "observed_date" = "date")) |>
    mutate(
      on_survey_date = coalesce(on_survey_date, FALSE),
      triage = if_else(
        triage == "flag" & on_survey_date,
        "recovered_by_date",
        triage
      ),
      triage_reason = if_else(
        triage == "recovered_by_date",
        "no survey tag but obs date matches confirmed survey date",
        triage_reason
      )
    )

  n_recovered    <- sum(clean$triage == "recovered_by_date", na.rm = TRUE)
  n_keep_offdate <- sum(clean$triage == "keep" & !clean$on_survey_date, na.rm = TRUE)

  message(sprintf("  %d obs recovered (on confirmed survey date, lacked tag)", n_recovered))
  if (n_keep_offdate > 0)
    message(sprintf(
      "  NOTE: %d 'keep' obs fall outside confirmed survey dates -- possible personal visits",
      n_keep_offdate
    ))

  write_fresh_csv(clean, out_clean, na = "")
  message(sprintf("  Updated with on_survey_date + recovery -> %s", out_clean))

} else {
  message("\nSkipping date recovery -- run survey_dates.R first, then re-run this script.")
  message("(Re-run adds on_survey_date column and recovers untagged survey obs.)")
}

# --- helpers used by the QC worklists below (write_fresh clears a stale file/dir first) ---
if (!exists("write_fresh")) write_fresh <- function(x, path, ...) {
  if (dir.exists(path)) unlink(path, recursive = TRUE, force = TRUE)
  else if (file.exists(path)) unlink(path, force = TRUE)
  write_csv(x, path, ...)
}
if (!exists("col_or_na")) col_or_na <- function(df, name, n = nrow(df)) if (name %in% names(df)) df[[name]] else rep(NA_character_, n)

# ============================================================================
# QC: SURVEY bee observations MISSING a flower-type (visited-plant) association.
# Spits out a URL worklist so they can be filled in on iNaturalist -- but ONLY for
# obs that plausibly SHOULD have a flower. It SKIPS records that already say the bee
# wasn't flower-visiting: "insect on flower? = No", on the ground, nesting, or a
# non-flower behaviour (mating / sleeping / etc.). A record counts as HAVING a flower
# if ANY of the ~20 visited-plant fields (not just the 4 authoritative ones) is filled.
#
# Standalone by design: reads the flattened export + the brain's membership
# (project_unclean_bee_observations.csv, for survey status), so it runs on its own.
#   source("scripts/clean/inat_bee_clean.R"); qc_missing_flower()
# ============================================================================

# obs-fields that record WHICH plant/flower the bee was on (plant identity).
FLOWER_PLANT_FIELDS <- c(
  "field:interaction->visited flower of", "field:interaction->visited flower of (2):",
  "field:interaction->flower visited by", "field:interaction->visited plant",
  "field:interaction->visited extrafloral nectaries of",
  "field:nectar / pollen delivering plant", "field:insect nectar/pollen plant",
  "field:name of associated plant", "field:nectar plant",
  "field:flower/plant name (what plant was the pollinator visiting?)",
  "field:what flower is the bee pollinating?", "field:plant species visited",
  "field:name of visited flowering plant", "field:visited flower of",
  "field:visiting flower of:", "field:visiting a flower of: (interaction)",
  "field:flowers visited", "field:visiting",
  "field:familia de planta visitada (xicotlidata)",
  "field:género de planta visitada (xicotlidata)",
  "field:especie de planta visitada (xicotlidata)")
# fields whose presence/value means the bee WASN'T flower-visiting -> don't flag.
FLOWER_NEST_FIELDS   <- c("field:nesting", "field:nesting bee", "field:nest",
  "field:nesting site", "field:nest construction", "field:nesting behaviour",
  "field:number of nests in site")
FLOWER_GROUND_FIELDS <- c("field:on ground", "field:on ground?")
FLOWER_BEHAVIOR_FIELDS <- c("field:bee behavior", "field:insect behavior",
  "field:animal behavior", "field:behavior observed", "field:behavior: mating",
  "field:mating behavior observed?")
# non-flower behaviour/value keywords (a behaviour field carrying one of these = not
# flower-visiting; a flower-y value like "pollinating"/"foraging" simply won't match).
FLOWER_NONFLOWER_RX <- "nest|ground|mat(e|ing)|copulat|sleep|rest|dead|prey|predat|\\bfl(y|ight|ying)\\b|emerg|dig|burrow|perch|stuck"

qc_missing_flower <- function(
    export_path     = if (exists("EXPORT_FLAT_CACHE")) EXPORT_FLAT_CACHE else "data/cache/export_flat.rds",
    membership_path = "data/project_info/project_unclean_bee_observations.csv",
    out_path        = "data/outputs/inat_clean/qc/cabr_inat_bee_missing_flower.csv",
    statuses        = "keep",   # which membership statuses to check (survey obs)
    write           = TRUE) {
  x <- readRDS(export_path)
  nonempty <- function(v) !is.na(v) & trimws(as.character(v)) != ""
  any_present <- function(cols) {
    cols <- intersect(cols, names(x))
    if (!length(cols)) return(rep(FALSE, nrow(x)))
    Reduce(`|`, lapply(cols, function(c) nonempty(x[[c]])))
  }

  has_flower <- any_present(FLOWER_PLANT_FIELDS)
  iof <- if ("field:insect on flower" %in% names(x)) tolower(trimws(x[["field:insect on flower"]])) else rep(NA_character_, nrow(x))
  not_on_flower <- (iof %in% "no") | any_present(FLOWER_NEST_FIELDS) | any_present(FLOWER_GROUND_FIELDS)
  for (c in intersect(FLOWER_BEHAVIOR_FIELDS, names(x)))
    not_on_flower <- not_on_flower | (nonempty(x[[c]]) & grepl(FLOWER_NONFLOWER_RX, tolower(x[[c]])))

  if (!file.exists(membership_path))
    stop("Need the brain's membership first: ", membership_path, " (run finding_project_info()).")
  mem <- readr::read_csv(membership_path, show_col_types = FALSE)
  survey <- mem |> filter(if ("kind" %in% names(mem)) kind == "bee" else TRUE, status %in% statuses)
  is_survey <- as.character(x$id) %in% as.character(survey$obs_id)

  flag <- is_survey & !has_flower & !not_on_flower
  tr <- survey |> transmute(id = as.character(obs_id), transect = if ("transect" %in% names(survey)) transect else NA_character_)
  out <- x[flag, , drop = FALSE] |>
    transmute(obs_id = as.character(id), url, observer = user_login, observed_on,
              scientific_name, common_name,
              insect_on_flower = col_or_na(x[flag, , drop = FALSE], "field:insect on flower"),
              tag_list, description) |>
    left_join(tr, by = c("obs_id" = "id")) |>
    relocate(transect, .after = common_name) |>
    arrange(observer, observed_on)

  if (write) {
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    write_fresh(out, out_path, na = "")
    message(sprintf("QC missing-flower: %d survey bee obs missing a flower association (of %d checked) -> %s",
                    nrow(out), sum(is_survey), out_path))
  }
  invisible(out)
}

# ============================================================
# QC WORKLISTS -- run standalone (skipped when sourced by run_pipeline.R). Both are
# advisory and wrapped so a missing input can't halt the script. The transect QC is a
# ROUGH placeholder: OT overlaps TP (~1m) / UPMON (~21m) so OT flags are unreliable --
# the fix lives in qc_misplaced_transect.R (see its header).
# ============================================================
if (!exists("BEESCABR_SOURCED_BY_RUNNER")) {
  tryCatch(qc_missing_flower(),
           error = function(e) message("  (missing-flower QC skipped: ", conditionMessage(e), ")"))
  tryCatch({
    source("scripts/clean/qc_misplaced_transect.R")
    qc_misplaced_transect(
      export_path     = "data/cache/export_flat.rds",
      membership_path = "data/project_info/project_unclean_bee_observations.csv",
      kind            = "bee",
      out_path        = "data/outputs/inat_clean/qc/cabr_inat_bee_misplaced_transect.csv")
  }, error = function(e) message("  (transect QC skipped: ", conditionMessage(e), ")"))
}
