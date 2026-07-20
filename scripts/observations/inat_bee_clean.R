# =============================================================
# observations/inat_bee_clean.R
# beescabr -- turn the brain's per-obs answer key into an ANALYSIS-ready iNaturalist BEE table.
#
# WHAT IT'S FOR
#   The brain (finding_project_info.R) writes data/observations/cabr_inat_raw.csv: one row per
#   iNat bee obs, already stamped (by the master_crosswalk) with a status, whose survey it is, the
#   resolved transect, and an in_cabr label -- but taxonomy-blind (no taxon_id / names) and with no
#   coordinates. This script pulls every bee obs INSIDE the CABR survey box (in_cabr == TRUE), labels
#   each survey-or-not, joins on identity + coordinates + crosswalk ANNOTATIONS from the iNat export.
#
#   "In CABR" uses cabr_survey_box.shp (the generous hand-drawn box), on purpose: it keeps ocean /
#   misplaced pins IN so they aren't lost.
#
# CROSSWALK ANNOTATIONS (per obs, from its iNat obs-fields + tags via master_crosswalk.csv)
#   * flower_visited -- the PLANT SPECIES the bee visited (a real value, e.g. "Encelia californica"),
#     coalesced from the visited-plant obs-fields (most-populated first).
#   * yes/no flags: bee_on_flower, pollen_on_bee, feeding, mating, bee_on_ground, bee_nest,
#     bee_in_nest, mark_recapture, cabr_bee_lethal_collection.
#   (flower_flowering is a PLANT concept -- excluded here; cabr_bee_lethal_collection is specimen-side,
#   so it's blank in the iNat bee table but kept for schema consistency.)
#
# SPATIAL SURVEY REFINEMENT (Humphreys Rd walk-in) + location_needs_fix
#   A tagged (keep) obs whose PIN sits off EVERY transect yet ON the access road to BST (Humphreys Rd)
#   is a WALK-IN: logged on the way to the transect, not a transect survey -> is_survey = FALSE (with a
#   survey_note). A tagged obs off every transect AND off the road stays a survey (likely a bad GPS pin)
#   and gets location_needs_fix = TRUE -- an extra-precaution "go fix this pin" marker. Shapefiles are
#   optional; absent either one, nothing is re-marked. (The pin-map visualising this is kept as a
#   reference artifact next to the road layer, not run in the pipeline.)
#
# SCAFFOLD NOTE (2026-07-20) -- columns exist now, values land later:
#   * TAXONOMY (scientific_name, common_name, kingdom..subspecies) is BLANK -- filled from the
#     taxonomy lookup (by taxon_id) once that table is built.
#   * is_10min / is_metadata are BLANK -- the crosswalk has no NOTE variants yet.
#   taxon_id is the real identity we compare bees on; taxon_rank rides along.
#
# INPUTS   data/observations/cabr_inat_raw.csv                         (the brain's per-obs lookup)
#          data/observations/cache/export_flat.rds                     (taxon_id + coords + fields/tags)
#          data/project_info/master_crosswalk.csv                      (field/tag -> annotation concept)
#          data/spatial/transects/cabr_bee_transects.shp               (off-transect test)
#          data/spatial/access_routes_to_transects/cabr_survey_access_routes.shp (Humphreys Rd walk-in)
# OUTPUT   data/observations/inat_clean/cabr_inat_bee_clean.csv        (one labeled CABR table)
#
# Run: source("scripts/observations/inat_bee_clean.R"); inat_bee_clean()
# =============================================================
suppressWarnings(suppressMessages({library(dplyr); library(readr); library(sf)}))

IBC_MEMBERSHIP     <- "data/observations/cabr_inat_raw.csv"
IBC_EXPORT         <- "data/observations/cache/export_flat.rds"
IBC_CROSSWALK      <- "data/project_info/master_crosswalk.csv"
IBC_TRANSECTS      <- "data/spatial/transects/cabr_bee_transects.shp"   # Name: TP/UPMON/BST/OT
IBC_ROAD           <- "data/spatial/access_routes_to_transects/cabr_survey_access_routes.shp"  # Humphreys Rd
IBC_OUT_CLEAN      <- "data/observations/inat_clean/cabr_inat_bee_clean.csv"
IBC_OFF_TRANSECT_M <- 50   # a pin farther than this from EVERY transect line is "off transect"
IBC_ROAD_BUFFER_M  <- 10   # off-transect AND within this of the access road = walk-in (not a survey)

# crosswalk annotation concepts carried onto each obs
IBC_BOOL_ANNOT <- c("bee_on_flower", "pollen_on_bee", "feeding", "mating",
                    "bee_on_ground", "bee_nest", "bee_in_nest", "mark_recapture",
                    "cabr_bee_lethal_collection")             # yes/no
IBC_ANNOT_COLS <- c("flower_visited", IBC_BOOL_ANNOT)          # flower_visited (value) first

# taxonomy columns -- BLANK placeholders now, filled from the taxonomy lookup (by taxon_id) later
IBC_TAXONOMY_COLS <- c("scientific_name", "common_name",
                       "kingdom", "phylum", "subphylum", "class", "subclass", "order",
                       "suborder", "infraorder", "superfamily", "family", "epifamily",
                       "subfamily", "tribe", "subtribe", "genus", "subgenus", "complex",
                       "species", "subspecies")

# final column order for the clean table
IBC_COLUMN_ORDER <- c("obs_id", "observer", "observed_on", "is_survey", "survey_note",
                      "survey_type", "survey_year", "transect", "is_10min", "is_metadata",
                      IBC_ANNOT_COLS, "location_needs_fix",
                      "taxon_id", "taxon_rank", "quality_grade",
                      IBC_TAXONOMY_COLS,
                      "latitude", "longitude", "positional_accuracy", "url")

# TP / TP1 / TP2 -> TP, etc. (same rule the brain + resolver use)
ibc_norm_transect <- function(x) {
  u <- toupper(gsub("^#", "", trimws(as.character(x))))
  dplyr::case_when(
    u %in% c("", "NA", "N/A") ~ NA_character_,
    startsWith(u, "TP")    ~ "TP",
    startsWith(u, "UPMON") ~ "UPMON",
    startsWith(u, "BST")   ~ "BST",
    u == "OT"              ~ "OT",
    TRUE                   ~ u)
}

# ---- crosswalk annotations -------------------------------------------------
# From each obs's iNat obs-fields (field:* columns) + tags, per master_crosswalk.csv:
#   flower_visited -> the plant value (coalesced, most-populated field first)
#   the IBC_BOOL_ANNOT concepts -> TRUE if any of the concept's field variants is populated or any
#   of its tag variants is present. Returns tibble(obs_id, flower_visited, <bool annot...>).
ibc_annotations <- function(ex_full, crosswalk_path) {
  ids <- as.character(ex_full$id)
  out <- tibble(obs_id = ids, flower_visited = NA_character_)
  for (cc in IBC_BOOL_ANNOT) out[[cc]] <- FALSE
  if (!file.exists(crosswalk_path)) { message("  (annotations: no crosswalk -- skipped)"); return(out) }
  cw <- suppressWarnings(read_csv(crosswalk_path, show_col_types = FALSE))
  splitv <- function(s) { s <- s[!is.na(s)]; if (!length(s)) return(character(0))
                          tolower(trimws(unlist(strsplit(s, "[;,]")))) }
  fcols <- grep("^field:", names(ex_full), value = TRUE); names(fcols) <- tolower(sub("^field:", "", fcols))
  tags  <- lapply(strsplit(tolower(ifelse(is.na(ex_full$tag_list), "", as.character(ex_full$tag_list))), "[;,|]"), trimws)
  nonempty <- function(x) !is.na(x) & trimws(as.character(x)) != ""

  for (cc in IBC_BOOL_ANNOT) {
    fv <- splitv(cw$inat_field_variants[cw$name == cc]); tv <- splitv(cw$inat_tag_variants[cw$name == cc])
    fc <- fcols[intersect(fv, names(fcols))]
    fhit <- if (length(fc)) Reduce(`|`, lapply(as.data.frame(ex_full[, fc, drop = FALSE]), nonempty)) else rep(FALSE, length(ids))
    thit <- if (length(tv)) vapply(tags, function(t) any(t %in% tv), logical(1)) else rep(FALSE, length(ids))
    out[[cc]] <- fhit | thit
  }

  # flower_visited value: coalesce the visited-plant fields, most-populated field first
  fvf <- splitv(cw$inat_field_variants[cw$name == "flower_visited"])
  fvc <- fcols[intersect(fvf, names(fcols))]
  if (length(fvc)) {
    fvc <- fvc[order(-vapply(fvc, function(c) sum(nonempty(ex_full[[c]])), integer(1)))]
    val <- rep(NA_character_, length(ids))
    for (c in fvc) { v <- trimws(as.character(ex_full[[c]])); take <- is.na(val) & nonempty(v); val[take] <- v[take] }
    out$flower_visited <- val
  }
  out
}

# ---- spatial survey flags --------------------------------------------------
# on_transect / walk_in (off transect but on Humphreys Rd) / bad_coord (off transect AND off road).
# Missing shapefile / coords -> "on_transect" (nothing re-marked).
ibc_spatial_flags <- function(keep_rows, transect_path, road_path,
                              off_m = IBC_OFF_TRANSECT_M, road_m = IBC_ROAD_BUFFER_M) {
  base <- tibble(obs_id = as.character(keep_rows$obs_id), spatial_cat = "on_transect")
  if (!file.exists(transect_path)) {
    message("  (spatial flags: no transect shapefile -- keep rows left as survey)"); return(base)
  }
  tl <- suppressWarnings(sf::st_read(transect_path, quiet = TRUE))
  names(tl)[tolower(names(tl)) == "name"] <- "Name"
  tl$tr <- ibc_norm_transect(tl$Name)
  lines <- tl |> dplyr::filter(!is.na(tr)) |> dplyr::summarise(geometry = sf::st_union(geometry))
  if (!nrow(lines)) return(base)

  cand <- keep_rows |>
    transmute(obs_id = as.character(obs_id), latitude, longitude,
              obsc = coalesce(as.logical(coordinates_obscured), FALSE)) |>
    filter(!is.na(latitude), !is.na(longitude), !obsc)
  if (!nrow(cand)) return(base)

  pts <- sf::st_as_sf(cand, coords = c("longitude", "latitude"), crs = 4326) |>
    sf::st_transform(sf::st_crs(tl))
  d_near <- as.numeric(sf::st_distance(pts, lines))

  if (file.exists(road_path)) {
    rd <- suppressWarnings(sf::st_read(road_path, quiet = TRUE)) |> sf::st_transform(sf::st_crs(tl))
    d_road <- as.numeric(sf::st_distance(pts, sf::st_union(rd)))
  } else {
    message("  (spatial flags: no access-road shapefile -- walk-in not identified)")
    d_road <- rep(Inf, nrow(pts))
  }

  cand$spatial_cat <- dplyr::case_when(
    d_near <= off_m  ~ "on_transect",
    d_road <= road_m ~ "walk_in",
    TRUE             ~ "bad_coord")

  base |>
    dplyr::left_join(cand |> dplyr::select(obs_id, sc = spatial_cat), by = "obs_id") |>
    dplyr::mutate(spatial_cat = dplyr::coalesce(sc, spatial_cat)) |>
    dplyr::select(obs_id, spatial_cat)
}

# ---- main ------------------------------------------------------------------
inat_bee_clean <- function(membership_path = IBC_MEMBERSHIP,
                           export_path     = IBC_EXPORT,
                           crosswalk_path  = IBC_CROSSWALK,
                           transect_path   = IBC_TRANSECTS,
                           road_path       = IBC_ROAD,
                           out_clean       = IBC_OUT_CLEAN,
                           write = TRUE) {
  stopifnot(file.exists(membership_path), file.exists(export_path))

  mem <- suppressWarnings(read_csv(membership_path, show_col_types = FALSE))
  if ("kind" %in% names(mem)) mem <- mem |> filter(kind == "bee")
  # whole-CABR = everything in the survey box that isn't a hard exclude (keep + flag)
  mem <- mem |> filter(status %in% c("keep", "flag"))
  mem$obs_id <- as.character(mem$obs_id)

  ex_full <- readRDS(export_path)
  ex <- tibble(
    obs_id               = as.character(ex_full$id),
    taxon_id             = as.character(ex_full$taxon_id),
    taxon_rank           = if ("rank" %in% names(ex_full)) as.character(ex_full$rank) else NA_character_,
    quality_grade        = ex_full$quality_grade,
    latitude             = ex_full$latitude,
    longitude            = ex_full$longitude,
    positional_accuracy  = ex_full$positional_accuracy,
    coordinates_obscured = if ("coordinates_obscured" %in% names(ex_full)) ex_full$coordinates_obscured else NA,
    url                  = ex_full$url) |>
    distinct(obs_id, .keep_all = TRUE)

  annot <- ibc_annotations(ex_full, crosswalk_path) |> distinct(obs_id, .keep_all = TRUE)

  df <- mem |> left_join(ex, by = "obs_id") |> left_join(annot, by = "obs_id")
  for (cc in IBC_BOOL_ANNOT) df[[cc]] <- coalesce(df[[cc]], FALSE)

  # spatial refinement: re-mark walk-in (off-transect but on Humphreys Rd) keep rows as NOT survey
  keep_full <- df |> filter(status == "keep")
  flags <- ibc_spatial_flags(keep_full, transect_path, road_path)
  df <- df |> left_join(flags, by = "obs_id")
  df$spatial_cat <- coalesce(df$spatial_cat, "on_transect")

  df$is_survey <- df$status == "keep" & df$spatial_cat != "walk_in"
  df$survey_note <- dplyr::case_when(
    df$status == "keep" & df$spatial_cat == "walk_in"   ~ "walk-in: off-transect on Humphreys Rd (not a transect survey)",
    df$status == "keep" & df$spatial_cat == "bad_coord" ~ "off-transect: GPS pin far from any transect -- check pin",
    TRUE ~ NA_character_)
  df$location_needs_fix <- df$is_survey & df$spatial_cat == "bad_coord"

  # blank scaffolding: taxonomy (from lookup later) + notes-derived flags (after note variants)
  for (col in IBC_TAXONOMY_COLS) df[[col]] <- NA_character_
  df$is_10min    <- NA
  df$is_metadata <- NA

  clean <- df |> select(any_of(IBC_COLUMN_ORDER))

  if (write) {
    dir.create(dirname(out_clean), recursive = TRUE, showWarnings = FALSE)
    write.csv(clean, out_clean, row.names = FALSE, na = "")
  }
  n_walk <- sum(df$status == "keep" & df$spatial_cat == "walk_in")
  n_bad  <- sum(df$location_needs_fix)
  n_fv   <- sum(!is.na(clean$flower_visited))
  message(sprintf("inat_bee_clean: %d CABR bee rows | %d survey / %d not-survey -> %s",
                  nrow(clean), sum(clean$is_survey), sum(!clean$is_survey), out_clean))
  message(sprintf("               spatial: %d walk-in re-marked NOT survey | %d location_needs_fix", n_walk, n_bad))
  message(sprintf("               annotations: %d obs with flower_visited", n_fv))
  invisible(list(clean = clean))
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0)
  message("Sourced inat_bee_clean.R -- inat_bee_clean() writes the labeled CABR bee table (annotations + walk-in marking).")
