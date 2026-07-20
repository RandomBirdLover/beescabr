# =============================================================
# specimens/specimen_bee_clean.R
# beescabr -- turn the lethal-survey specimen record into an ANALYSIS-ready table
# that MIRRORS inat_bee_clean.R's schema (so lethal vs non-lethal line up column-
# for-column), swapping obs_id -> ucsd_id + sdnhm_id and keeping `sex`.
#
# WHAT IT'S FOR
#   Reads the newest specimen .xlsx (data/specimens/records/), cleans names/dates,
#   attaches taxon_id + full taxonomy from the taxonomy lookup (by name), resolves
#   the transect from the `plot` text (via the master_crosswalk specimen_label_variants,
#   with a lat/long spatial fallback for plain "Cabrillo NM"), pulls the visited
#   plant out of method_or_plant, and writes cabr_specimen_bee_clean.csv plus
#   QC side files (taxonomy flags, missing, duplicates).
#
# COLUMN MAP (iNat -> specimen)
#   obs_id            -> ucsd_id + sdnhm_id
#   observer          -> collector          observed_on -> date       survey_year -> year
#   is_survey         -> TRUE (all)         survey_type -> "intern"   survey_note -> blank
#   survey_method     -> "lethal" (all)     (lethal/non-lethal axis; survey_type is the surveyor)
#   transect          -> from `plot` (crosswalk variants) + spatial fallback
#   flower_visited    -> the "ex. <plant>" value of method_or_plant (methods -> blank)
#   cabr_bee_lethal_collection -> TRUE      the 8 other behavior flags -> blank
#   location_needs_fix-> TRUE if lat/long missing (none should be)
#   taxon_id/taxon_rank + taxonomy -> from the lookup by name       sex -> kept
#   is_10min / is_metadata / quality_grade / positional_accuracy / url -> blank
#
# Runs AFTER the taxonomy lookup (stage 5) -- it reads the lookup + complex map.
# In the pipeline it's non-interactive: flags are logged and the run continues.
#
# Run: source("scripts/specimens/specimen_bee_clean.R"); clean_specimens()
# =============================================================
suppressWarnings(suppressMessages({ library(dplyr); library(stringr); library(readr); library(sf); library(readxl) }))

local({
  need <- function(sym, file) if (!exists(sym)) source(file.path("scripts", file))
  need("PATHS",                     "config.R")
  need("read_latest",               "utils/utils.R")
  need("require_columns",           "utils/utils.R")
  need("write_fresh",               "utils/utils.R")
  need("standardize_specimen_names","specimens/specimen_clean.R")
  need("keep_bee_specimens",        "specimens/specimen_clean.R")
})

SBC_RECORDS_DIR     <- "data/specimens/records"
SBC_RECORDS_PATTERN <- "^cabr_bee_specimens_record_V"
SBC_TRANSECTS       <- "data/spatial/transects/cabr_bee_transects.shp"
SBC_CROSSWALK       <- "data/project_info/master_crosswalk.csv"
SBC_OFF_TRANSECT_M  <- 50   # a plot-less specimen within this of a transect line is assigned to it

# blank behavior flags (iNat-only annotations); cabr_bee_lethal_collection is set TRUE
SBC_BLANK_BOOL <- c("bee_on_flower", "pollen_on_bee", "feeding", "mating",
                    "bee_on_ground", "bee_nest", "bee_in_nest", "mark_recapture")
SBC_TAXONOMY_COLS <- c("scientific_name", "common_name",
                       "kingdom", "phylum", "subphylum", "class", "subclass", "order",
                       "suborder", "infraorder", "superfamily", "family", "epifamily",
                       "subfamily", "tribe", "subtribe", "genus", "subgenus", "complex",
                       "species", "subspecies")
# final column order -- inat_bee_clean's IBC_COLUMN_ORDER, obs_id -> ucsd_id+sdnhm_id, + sex
SBC_COLUMN_ORDER <- c("ucsd_id", "sdnhm_id", "observer", "observed_on", "is_survey", "survey_note",
                      "survey_type", "survey_method", "survey_year", "transect", "is_10min", "is_metadata",
                      "flower_visited", SBC_BLANK_BOOL, "cabr_bee_lethal_collection",
                      "location_needs_fix", "taxon_id", "taxon_rank", "quality_grade",
                      SBC_TAXONOMY_COLS, "latitude", "longitude", "positional_accuracy",
                      "url", "sex")

# TP/TP1 -> TP, etc. (same normalization the iNat side uses).
.sbc_norm_transect <- function(x) {
  u <- toupper(gsub("^#", "", trimws(as.character(x))))
  dplyr::case_when(u %in% c("", "NA", "N/A") ~ NA_character_,
                   startsWith(u, "TP") ~ "TP", startsWith(u, "UPMON") ~ "UPMON",
                   startsWith(u, "BST") ~ "BST", u == "OT" ~ "OT", TRUE ~ u)
}

# the plant out of method_or_plant: "ex. Encelia californica" -> "Encelia californica";
# a method ("Aerial Net", "ground") -> NA.
.sbc_flower_from_method <- function(mop) {
  x <- trimws(as.character(mop))
  ifelse(grepl("^ex\\.?\\s+", x, ignore.case = TRUE),
         trimws(sub("^ex\\.?\\s+", "", x, ignore.case = TRUE)), NA_character_)
}

# spatial transect: for rows still missing a transect, the nearest transect line
# within SBC_OFF_TRANSECT_M of the specimen's coordinates (else NA). Impure (reads
# the shapefile); returns a character vector aligned to df's rows.
sbc_transect_spatial <- function(df, transect_path = SBC_TRANSECTS, off_m = SBC_OFF_TRANSECT_M) {
  out <- rep(NA_character_, nrow(df))
  if (!file.exists(transect_path)) {
    message("  (transect spatial: no shapefile -- plot-less rows left blank)"); return(out)
  }
  tl <- suppressWarnings(sf::st_read(transect_path, quiet = TRUE))
  names(tl)[tolower(names(tl)) == "name"] <- "Name"
  tl$tr <- .sbc_norm_transect(tl$Name)
  groups <- tl |> dplyr::filter(!is.na(tr)) |> dplyr::group_by(tr) |>
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")
  if (!nrow(groups)) return(out)
  idx <- which(!is.na(df$latitude) & !is.na(df$longitude))
  if (!length(idx)) return(out)
  pts <- sf::st_as_sf(df[idx, ], coords = c("longitude", "latitude"), crs = 4326) |>
    sf::st_transform(sf::st_crs(tl))
  dmat <- sf::st_distance(pts, groups)
  for (k in seq_along(idx)) {
    d <- as.numeric(dmat[k, ]); m <- which.min(d)
    if (length(m) && is.finite(d[m]) && d[m] <= off_m) out[idx[k]] <- as.character(groups$tr[m])
  }
  out
}

# ------------------------------------------------------------
# clean_specimens(): read -> clean -> write. Non-interactive in the pipeline
# (interactive_ok = FALSE logs flags and continues; never hard-stops the run).
# ------------------------------------------------------------
clean_specimens <- function(interactive_ok = (Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1"),
                            prompt_fn = readline, verbose = TRUE) {
  cleaned_dir <- "data/specimens/cleaned"
  flags_out   <- file.path(cleaned_dir, "cabr_specimen_bee_taxonomy_flags.csv")
  missing_out <- file.path(cleaned_dir, "cabr_specimen_bee_missing.csv")
  dupes_out   <- file.path(cleaned_dir, "cabr_specimen_bee_duplicates.csv")
  clean_out   <- PATHS$specimen_clean

  specimens_path <- read_latest(SBC_RECORDS_DIR, SBC_RECORDS_PATTERN)
  message("Loading specimens: ", basename(specimens_path))
  raw <- suppressMessages(readxl::read_excel(specimens_path))
  require_columns(raw, c("date", "latitude", "longitude", "sdnhm_id", "ucsd_id", "collector",
                         "plot", "method_or_plant", "genus", "subgenus", "complex", "species",
                         "subspecies", "sex", "missing_specimen"), "raw specimens")
  message("Loaded ", nrow(raw), " specimen rows")

  df <- raw |> parse_specimen_dates() |> standardize_specimen_names()

  # BEES ONLY (per Brandi: "only include bees, not wasps or dipterans, or anything
  # not bee"): drop wasp/fly bycatch AND fully-unidentified rows up front, so the
  # entire downstream -- taxonomy attach, spell-check flags, transect, complex match,
  # QC side files, output -- is bee-scoped. FAMILY test, not superfamily (apoid wasps
  # like Crabronidae share Apoidea with bees). The record carries `family` on every
  # ID'd row, so no real bee is lost.
  n_pre_bee <- nrow(df)
  df <- keep_bee_specimens(df)
  message(sprintf("  bee filter: kept %d bee-family rows, dropped %d non-bee / unidentified",
                  nrow(df), n_pre_bee - nrow(df)))

  # --- taxon_id + full taxonomy + spell-check (needs the lookup) ---
  if (file.exists(PATHS$taxonomy_lookup)) {
    lookup <- suppressMessages(read_csv(PATHS$taxonomy_lookup, show_col_types = FALSE))
    df <- attach_lookup_taxonomy(df, lookup)

    tax_check <- lookup |> filter(!is.na(genus), genus != "") |>
      mutate(genus = str_to_title(genus), species = str_to_lower(species))
    inat_species <- if (file.exists(PATHS$checklist_sd_county_inat)) {
      suppressMessages(read_csv(PATHS$checklist_sd_county_inat, show_col_types = FALSE)) |>
        filter(!is.na(genus), genus != "", !is.na(species), species != "") |>
        mutate(genus = str_to_title(genus), species = str_to_lower(species)) |> distinct(genus, species)
    } else tibble(genus = character(), species = character())
    known <- build_known_names(tax_check, inat_species)
    flags <- compute_taxonomy_flags(df, known$genera, known$genus_species)
    dir.create(cleaned_dir, recursive = TRUE, showWarnings = FALSE)
    write_fresh(flags, flags_out, row.names = FALSE)
    message(sprintf("  taxonomy spell-check: %d flag(s) -> %s", nrow(flags), basename(flags_out)))
    if (nrow(flags) > 0 && verbose) print(as.data.frame(flags))
    if (resolve_flag_gate(nrow(flags), interactive_ok, prompt_fn) == "stop")
      stop("Stopping: fix the flagged names in the source .xlsx, then re-run.")
  } else {
    message("WARNING: taxonomy lookup not found (", PATHS$taxonomy_lookup,
            ") -- taxon_id + taxonomy left blank. Run the lookup (stage 5) first.")
    for (col in SBC_TAXONOMY_COLS) if (!col %in% names(df)) df[[col]] <- NA_character_
    df$taxon_id <- NA; df$taxon_rank <- NA_character_
  }

  # --- transect: crosswalk plot-variant match, spatial fallback for the rest ---
  cw <- if (file.exists(SBC_CROSSWALK)) suppressMessages(read_csv(SBC_CROSSWALK, show_col_types = FALSE)) else NULL
  by_plot <- match_plot_transect(df$plot, transect_variant_map(cw))
  need_sp <- is.na(by_plot)
  by_spatial <- if (any(need_sp)) sbc_transect_spatial(df) else rep(NA_character_, nrow(df))
  df$transect <- ifelse(is.na(by_plot), by_spatial, by_plot)
  message(sprintf("  transect: %d from plot, %d from lat/long, %d unresolved",
                  sum(!is.na(by_plot)), sum(need_sp & !is.na(by_spatial)), sum(is.na(df$transect))))

  # --- complex match (needs the internal complex map from stage 5) ---
  if (file.exists(PATHS$complex_map)) {
    cmap <- read.csv(PATHS$complex_map)
    require_columns(cmap, c("genus", "species", "complex", "complex_taxon_id"), "complex_map")
    df <- match_specimen_complex(df, build_complex_lookup(cmap))
  } else if (!"complex" %in% names(df)) df$complex <- NA_character_

  # --- schema columns ---
  df$observer   <- df$collector
  df$observed_on <- df$date_clean
  df$is_survey  <- TRUE
  df$survey_note <- NA_character_
  df$survey_type <- "intern"     # surveyor category (interns collect the specimens)
  df$survey_method <- "lethal"   # specimens are the LETHAL survey
  df$survey_year <- df$year
  df$is_10min <- NA; df$is_metadata <- NA
  df$flower_visited <- .sbc_flower_from_method(df$method_or_plant)
  for (b in SBC_BLANK_BOOL) df[[b]] <- NA
  df$cabr_bee_lethal_collection <- TRUE
  df$location_needs_fix <- is.na(df$latitude) | is.na(df$longitude)
  df$quality_grade <- NA_character_
  df$positional_accuracy <- NA
  df$url <- NA_character_
  for (col in SBC_TAXONOMY_COLS) if (!col %in% names(df)) df[[col]] <- NA_character_

  # --- QC side files ---
  qc <- add_qc_flags(df)
  write_fresh(qc |> filter(!is.na(missing_specimen) & missing_specimen == "Y"), missing_out, row.names = FALSE)
  write_fresh(detect_duplicate_ids(df), dupes_out, row.names = FALSE)

  clean <- df |> strip_control_chars() |> select(any_of(SBC_COLUMN_ORDER))
  dir.create(dirname(clean_out), recursive = TRUE, showWarnings = FALSE)
  write.csv(clean, clean_out, row.names = FALSE, na = "")

  message(sprintf("specimen_bee_clean: %d specimen rows -> %s", nrow(clean), clean_out))
  message(sprintf("               %d with taxon_id | %d with flower_visited | %d missing lat/long (flagged)",
                  sum(!is.na(clean$taxon_id)), sum(!is.na(clean$flower_visited)), sum(clean$location_needs_fix)))
  invisible(list(clean = clean))
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0)
  message("Sourced specimen_bee_clean.R -- clean_specimens() writes the labeled CABR specimen table (mirrors inat_bee_clean).")
