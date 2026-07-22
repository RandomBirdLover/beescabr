# =============================================================
# observations/inat_plant_clean.R
# beescabr -- turn the brain's per-obs answer key into an ANALYSIS-ready iNaturalist PLANT table.
#
# Parallel to inat_bee_clean.R, scoped to the SURVEYORS' Plantae observations. Same brain
# membership file (cabr_inat_raw.csv, filtered kind=="plant"), same spatial survey refinement,
# same is_survey marking -- but PLANT-shaped:
#   * SCOPE: only the surveyors' plant obs -- observer is on the surveyor roster
#     (surveyor_roster.csv). Non-surveyor / public plant obs in the box are dropped.
#   * ANNOTATION: the ONE plant concept -- flower_flowering (the phenology value, e.g. "Flowering"),
#     coalesced from the crosswalk's "flowering?"/"flowering" obs-fields. (No bee annotations.)
#   * TAXONOMY: filled straight from the plant iNat export (scientific_name / common_name + the
#     ranked taxon_*_name columns) -- plants have NO Holway lookup. taxon_rank rides along as the
#     rank each obs was IDENTIFIED to (from the export), untouched by the taxonomy fill.
#   * is_survey marks whether each obs was part of a survey (tagged keep, on-transect) vs the
#     surveyor's non-survey plant obs; walk-in / bad-coord handled exactly as in the bee cleaner.
#
# INPUTS   data/observations/cabr_inat_raw.csv                 (brain per-obs lookup; kind=="plant")
#          data/observations/cache/export_flat_plant.rds       (taxonomy + coords + fields/tags)
#          data/project_info/master_crosswalk.csv              (flowering field variants)
#          data/project_info/surveyor_roster.csv               (surveyor usernames -> scope)
#          data/spatial/transects/cabr_bee_transects.shp       (off-transect test)
#          data/spatial/access_routes_to_transects/cabr_survey_access_routes.shp (walk-in)
# OUTPUT   data/observations/inat_clean/cabr_inat_plant_clean.csv
#
# Run: source("scripts/observations/inat_plant_clean.R"); inat_plant_clean()
# =============================================================
suppressWarnings(suppressMessages({library(dplyr); library(readr); library(sf); library(stringr)}))

IPC_MEMBERSHIP     <- "data/observations/cabr_inat_raw.csv"
IPC_EXPORT         <- "data/observations/cache/export_flat_plant.rds"
IPC_CROSSWALK      <- "data/project_info/master_crosswalk.csv"
IPC_ROSTER         <- "data/project_info/surveyor_roster.csv"
IPC_TRANSECTS      <- "data/spatial/transects/cabr_bee_transects.shp"
IPC_ROAD           <- "data/spatial/access_routes_to_transects/cabr_survey_access_routes.shp"
IPC_OUT_CLEAN      <- "data/observations/inat_clean/cabr_inat_plant_clean.csv"
IPC_ALL_TAXA       <- "data/observations/reference/cabr_inat_plant_all_taxa.csv"  # ALL in-box plant taxa, ANY observer -- in-park truth for the plant lookup
IPC_OFF_TRANSECT_M <- 50
IPC_ROAD_BUFFER_M  <- 10

# the ONE plant annotation concept (the flowering-phenology value)
IPC_ANNOT_COLS <- c("flower_flowering")

# taxonomy columns -- filled from the plant iNat export (no Holway lookup for plants)
IPC_TAXONOMY_COLS <- c("scientific_name", "common_name",
                       "kingdom", "phylum", "subphylum", "class", "subclass", "order",
                       "suborder", "infraorder", "superfamily", "family", "epifamily",
                       "subfamily", "tribe", "subtribe", "genus", "subgenus", "complex",
                       "species", "subspecies")

# schema rank column  <-  the export's taxon_*_name column it is filled from
IPC_EXPORT_RANKMAP <- c(
  kingdom = "taxon_kingdom_name",   phylum      = "taxon_phylum_name",   subphylum  = "taxon_subphylum_name",
  class   = "taxon_class_name",     subclass    = "taxon_subclass_name", order      = "taxon_order_name",
  suborder = "taxon_suborder_name", infraorder  = "taxon_infraorder_name",
  superfamily = "taxon_superfamily_name", family = "taxon_family_name",  epifamily  = "taxon_epifamily_name",
  subfamily = "taxon_subfamily_name", tribe    = "taxon_tribe_name",    subtribe    = "taxon_subtribe_name",
  genus   = "taxon_genus_name",     species     = "taxon_species_name",  subspecies = "taxon_subspecies_name")

# final column order for the clean plant table (mirrors inat_bee_clean; flower_flowering is the
# only annotation, and there is no lethal-collection column -- plants are non-lethal by nature)
IPC_COLUMN_ORDER <- c("obs_id", "observer", "observed_on", "is_survey", "survey_note", "survey_source",
                      "surveyor_type", "survey_method", "survey_year", "transect", "is_10min", "is_metadata",
                      IPC_ANNOT_COLS, "location_needs_fix",
                      "taxon_id", "taxon_rank", "quality_grade",
                      IPC_TAXONOMY_COLS,
                      "plant_genus", "plant_species",
                      "latitude", "longitude", "positional_accuracy", "url")

# TP / TP1 / TP2 -> TP, etc. (same rule the brain + bee cleaner use)
ipc_norm_transect <- function(x) {
  u <- toupper(gsub("^#", "", trimws(as.character(x))))
  dplyr::case_when(
    u %in% c("", "NA", "N/A") ~ NA_character_,
    startsWith(u, "TP")    ~ "TP",
    startsWith(u, "UPMON") ~ "UPMON",
    startsWith(u, "BST")   ~ "BST",
    u == "OT"              ~ "OT",
    TRUE                   ~ u)
}

# ---- flowering annotation --------------------------------------------------
# ipc_flowering(): PURE. From each plant obs's iNat obs-fields (field:* columns), per
# master_crosswalk.csv's flower_flowering row, return tibble(obs_id, flower_flowering) where
# flower_flowering is the phenology VALUE (e.g. "Flowering", "Fruiting"), coalesced from the
# flowering fields most-populated-first. Missing crosswalk / no flowering field -> NA column.
ipc_flowering <- function(ex_full, crosswalk_path) {
  ids <- as.character(ex_full$id)
  out <- tibble(obs_id = ids, flower_flowering = NA_character_)
  if (!file.exists(crosswalk_path)) return(out)
  cw <- suppressWarnings(suppressMessages(read_csv(crosswalk_path, show_col_types = FALSE)))
  splitv   <- function(s) { s <- s[!is.na(s)]; if (!length(s)) return(character(0))
                            tolower(trimws(unlist(strsplit(s, "[;,]")))) }
  nonempty <- function(x) !is.na(x) & trimws(as.character(x)) != ""
  fcols <- grep("^field:", names(ex_full), value = TRUE); names(fcols) <- tolower(sub("^field:", "", fcols))
  fv <- if (all(c("name", "inat_field_variants") %in% names(cw)))
          splitv(cw$inat_field_variants[cw$name == "flower_flowering"]) else character(0)
  fc <- fcols[intersect(fv, names(fcols))]
  if (length(fc)) {
    fc  <- fc[order(-vapply(fc, function(c) sum(nonempty(ex_full[[c]])), integer(1)))]   # most-populated first
    val <- rep(NA_character_, length(ids))
    for (c in fc) { v <- trimws(as.character(ex_full[[c]])); take <- is.na(val) & nonempty(v); val[take] <- v[take] }
    out$flower_flowering <- val
  }
  out
}

# ---- taxonomy from the plant export ----------------------------------------
# ipc_taxonomy_from_export(): PURE. Map the plant export's iNat taxonomy onto the shared schema
# columns: scientific_name / common_name direct, the ranked taxon_*_name columns onto
# kingdom..subspecies, and species/subspecies reduced to the bare epithet (the export carries the
# full binomial/trinomial). Every IPC_TAXONOMY_COLS column is guaranteed present (blank if absent).
ipc_taxonomy_from_export <- function(ex_full) {
  ids <- as.character(ex_full$id)
  g   <- function(nm) if (nm %in% names(ex_full)) as.character(ex_full[[nm]]) else rep(NA_character_, length(ids))
  ep  <- function(x) { x <- trimws(as.character(x)); ifelse(is.na(x) | x == "", NA_character_, word(x, -1)) }
  out <- tibble(obs_id = ids, scientific_name = g("scientific_name"), common_name = g("common_name"))
  for (rk in names(IPC_EXPORT_RANKMAP)) out[[rk]] <- g(IPC_EXPORT_RANKMAP[[rk]])
  out$subgenus   <- g("subgenus")
  out$complex    <- g("complex")
  out$species    <- ep(out$species)       # "Encelia californica" -> "californica"
  out$subspecies <- ep(out$subspecies)
  for (col in IPC_TAXONOMY_COLS) if (!col %in% names(out)) out[[col]] <- NA_character_
  out |> select(obs_id, all_of(IPC_TAXONOMY_COLS))
}

# ---- spatial survey flags (mirror of the bee cleaner) ----------------------
# on_transect / walk_in (off transect but on Humphreys Rd) / bad_coord (off transect AND off road).
# Missing shapefile / coords -> "on_transect" (nothing re-marked).
ipc_spatial_flags <- function(keep_rows, transect_path, road_path,
                              off_m = IPC_OFF_TRANSECT_M, road_m = IPC_ROAD_BUFFER_M) {
  base <- tibble(obs_id = as.character(keep_rows$obs_id), spatial_cat = "on_transect")
  if (!file.exists(transect_path)) {
    message("  (spatial flags: no transect shapefile -- keep rows left as survey)"); return(base)
  }
  tl <- suppressWarnings(sf::st_read(transect_path, quiet = TRUE))
  names(tl)[tolower(names(tl)) == "name"] <- "Name"
  tl$tr <- ipc_norm_transect(tl$Name)
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

# ---- surveyor scope --------------------------------------------------------
# ipc_scope_to_surveyors(): PURE. Keep only obs whose observer is on the roster. Missing roster
# (NULL/empty) -> passthrough (nothing dropped), so an absent roster never silently empties output.
ipc_scope_to_surveyors <- function(mem, surveyors) {
  if (is.null(surveyors) || !length(surveyors)) return(mem)
  su <- unique(trimws(as.character(surveyors))); su <- su[!is.na(su) & su != ""]
  if (!length(su)) return(mem)
  mem |> filter(trimws(as.character(observer)) %in% su)
}

# ---- main ------------------------------------------------------------------
inat_plant_clean <- function(membership_path = IPC_MEMBERSHIP,
                             export_path     = IPC_EXPORT,
                             crosswalk_path  = IPC_CROSSWALK,
                             roster_path     = IPC_ROSTER,
                             transect_path   = IPC_TRANSECTS,
                             road_path       = IPC_ROAD,
                             out_clean       = IPC_OUT_CLEAN,
                             write = TRUE) {
  stopifnot(file.exists(membership_path), file.exists(export_path))

  mem <- suppressWarnings(suppressMessages(read_csv(membership_path, show_col_types = FALSE)))
  if ("kind" %in% names(mem)) mem <- mem |> filter(kind == "plant")
  mem <- mem |> filter(status %in% c("keep", "flag"))   # in the CABR box, not a hard exclude
  mem$obs_id <- as.character(mem$obs_id)
  mem_all <- mem   # BEFORE surveyor scoping: every in-box plant obs, ANY observer (in-park truth)

  # SCOPE: surveyors' plant obs only (observer on the roster)
  if (file.exists(roster_path)) {
    roster    <- suppressWarnings(suppressMessages(read_csv(roster_path, show_col_types = FALSE)))
    surveyors <- if ("inaturalist_username" %in% names(roster)) roster$inaturalist_username else character(0)
    mem <- ipc_scope_to_surveyors(mem, surveyors)
  } else {
    message("  (scope: no roster at ", roster_path, " -- keeping all in-box plant obs)")
  }

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

  tax  <- ipc_taxonomy_from_export(ex_full) |> distinct(obs_id, .keep_all = TRUE)
  flow <- ipc_flowering(ex_full, crosswalk_path) |> distinct(obs_id, .keep_all = TRUE)

  # BROAD in-park truth: every distinct plant taxon observed anywhere in the CABR
  # box by ANY observer (mem_all = pre-surveyor-scope). The plant taxonomy lookup
  # reads this to decide in_cabr_park_at_all. Written here because only this stage
  # has the RDS export (taxon names) loaded.
  if (write) {
    tcols <- intersect(c("scientific_name", "common_name", "kingdom", "phylum", "class",
                         "order", "family", "genus", "species"), names(tax))
    all_taxa <- ex |> select(obs_id, taxon_id, taxon_rank) |>
      inner_join(tax |> select(obs_id, all_of(tcols)), by = "obs_id") |>
      filter(obs_id %in% mem_all$obs_id, !is.na(scientific_name), scientific_name != "") |>
      distinct(scientific_name, .keep_all = TRUE) |>
      select(taxon_id, taxon_rank, all_of(tcols)) |>
      arrange(scientific_name)
    dir.create(dirname(IPC_ALL_TAXA), recursive = TRUE, showWarnings = FALSE)
    write.csv(all_taxa, IPC_ALL_TAXA, row.names = FALSE, na = "")
    message(sprintf("               all-observer in-park plant taxa: %d -> %s", nrow(all_taxa), IPC_ALL_TAXA))
  }

  df <- mem |> left_join(ex, by = "obs_id") |> left_join(tax, by = "obs_id") |> left_join(flow, by = "obs_id")

  # spatial refinement: re-mark walk-in (off-transect but on Humphreys Rd) keep rows as NOT survey
  keep_full <- df |> filter(status == "keep")
  flags <- ipc_spatial_flags(keep_full, transect_path, road_path)
  df <- df |> left_join(flags, by = "obs_id")
  df$spatial_cat <- coalesce(df$spatial_cat, "on_transect")

  df$is_survey <- df$status == "keep" & df$spatial_cat != "walk_in"
  df$survey_note <- dplyr::case_when(
    df$status == "keep" & df$spatial_cat == "walk_in"   ~ "walk-in: off-transect on Humphreys Rd (not a transect survey)",
    df$status == "keep" & df$spatial_cat == "bad_coord" ~ "off-transect: GPS pin far from any transect -- check pin",
    TRUE ~ NA_character_)
  df$location_needs_fix <- df$is_survey & df$spatial_cat == "bad_coord"

  for (col in IPC_TAXONOMY_COLS) if (!col %in% names(df)) df[[col]] <- NA_character_
  if (!"flower_flowering" %in% names(df)) df$flower_flowering <- NA_character_
  if (!"survey_source" %in% names(df)) df$survey_source <- NA_character_   # tag / inferred_on_transect
  df$survey_method <- "nonlethal"   # iNaturalist observations are the NON-lethal survey

  # plant_genus + full-binomial plant_species (the plant IS the taxon here) -- uniform
  # with the bee + specimen tables so analysis can group on the same two columns everywhere.
  if (!exists("plant_name_parts")) source("scripts/reference/plant_lookup_join.R")
  .pp <- plant_name_parts(df$scientific_name)
  df$plant_genus <- .pp$plant_genus; df$plant_species <- .pp$plant_species

  clean <- df |> select(any_of(IPC_COLUMN_ORDER))

  if (write) {
    dir.create(dirname(out_clean), recursive = TRUE, showWarnings = FALSE)
    write.csv(clean, out_clean, row.names = FALSE, na = "")
  }
  n_walk <- sum(df$status == "keep" & df$spatial_cat == "walk_in")
  n_flow <- sum(!is.na(clean$flower_flowering))
  message(sprintf("inat_plant_clean: %d surveyor plant rows | %d survey / %d not-survey -> %s",
                  nrow(clean), sum(clean$is_survey), sum(!clean$is_survey), out_clean))
  message(sprintf("               spatial: %d walk-in re-marked NOT survey | annotations: %d with flower_flowering",
                  n_walk, n_flow))
  invisible(list(clean = clean))
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0)
  message("Sourced inat_plant_clean.R -- inat_plant_clean() writes the surveyors' labeled CABR plant table.")
