# =============================================================
# clean/qc_misplaced_transect.R
# beescabr -- SHARED transect-coordinate QC (used by inat_bee_clean.R + inat_plant_clean.R)
#
# Flags TAGGED observations whose GPS coordinate sits farther than `buffer_m` from the
# transect line named by the obs's own transect tag, so a human can @-message the observer
# to fix the pin on iNaturalist. It MEASURES ONLY -- it never moves a point.
#
# >>> ROUGH / PLACEHOLDER (2026-07-18) -- THIS IS WHERE THE OVERLAP FIX HAS TO HAPPEN <<<
#   TRANSECT LINES OVERLAP, so which transect a point belongs to CANNOT be decided from
#   its coordinate where two lines sit within GPS error (5-25 m) of each other -- the
#   transect TAG has to be trusted in those zones. Overlaps to fix:
#       * OT overlaps with TP and UPMON
#       * BST overlaps with UPMON
#   Measured min distance between the current shapefile lines:
#       OT-TP 1m | OT-UPMON 21m | OT-BST 40m | BST-UPMON 122m | UPMON-TP 233m | BST-TP 316m
#   DISCREPANCY: the shapefile has BST-UPMON 122 m APART (no overlap), which CONTRADICTS
#   the "BST overlaps UPMON" report -- so either the shapefile is wrong for BST/UPMON, or
#   the BST<->UPMON tag swaps we found are genuine mislabels. RESOLVE before trusting either.
#   How to fix (HERE):
#     * repair the transect shapefile so lines match the real routes (esp. BST vs UPMON).
#     * measure distance to the NEAREST transect (off EVERY line = a bad coordinate) rather
#       than the tagged line; never call OT (or any overlapping pair) a "wrong transect".
#     * buffer_m below is a placeholder (50 m).
#
# Run (standalone):
#   source("scripts/clean/qc_misplaced_transect.R")
#   qc_misplaced_transect("data/cache/export_flat.rds",
#                         "data/project_info/project_unclean_bee_observations.csv",
#                         kind = "bee",
#                         out_path = "data/outputs/inat_clean/qc/cabr_inat_bee_misplaced_transect.csv")
# =============================================================

suppressWarnings(suppressMessages({library(sf); library(dplyr); library(readr)}))

QC_TRANSECTS_PATH     <- "data/spatial/transects/cabr_bee_transects.shp"  # Name: TP/UPMON/BST/OT
QC_MISPLACED_BUFFER_M <- 50   # placeholder -- see header note

qc_norm_transect <- function(x) {
  u <- toupper(gsub("^#", "", trimws(as.character(x))))
  dplyr::case_when(
    u %in% c("", "NA", "N/A") ~ NA_character_,
    startsWith(u, "TP")    ~ "TP",     # TP / TP1 / TP2 -> TP
    startsWith(u, "UPMON") ~ "UPMON",
    startsWith(u, "BST")   ~ "BST",
    u == "OT"              ~ "OT",
    TRUE                   ~ u)
}

# export_path      -- data/cache/export_flat.rds (bee) or export_flat_plant.rds (plant); has coords
# membership_path  -- project_unclean_bee_observations.csv (the shared per-obs lookup)
# kind             -- "bee" / "plant" to filter the membership (NULL = all); tolerated if absent
qc_misplaced_transect <- function(export_path, membership_path,
                                  kind          = NULL,
                                  transect_path = QC_TRANSECTS_PATH,
                                  buffer_m      = QC_MISPLACED_BUFFER_M,
                                  out_path      = NULL, write = TRUE) {
  empty <- tibble(obs_id = character(), observer = character(), transect = character(),
                  distance_m = numeric(), url = character())
  if (!file.exists(transect_path) || !file.exists(export_path) || !file.exists(membership_path)) {
    message("  (transect QC: missing input -- skipped)"); return(empty)
  }
  tl <- sf::st_read(transect_path, quiet = TRUE)
  names(tl)[tolower(names(tl)) == "name"] <- "Name"
  tl$tr <- qc_norm_transect(tl$Name)

  mem <- suppressWarnings(read_csv(membership_path, show_col_types = FALSE))
  if (!"obscured" %in% names(mem)) mem$obscured <- FALSE          # old membership had no obscured col
  if (!is.null(kind) && "kind" %in% names(mem)) mem <- mem |> filter(kind == !!kind)

  ex <- readRDS(export_path); ex$obs_id <- ex$id
  cand <- mem |>
    filter(status == "keep", !is.na(transect), !coalesce(as.logical(obscured), FALSE)) |>
    transmute(obs_id = as.character(obs_id), observer, tr = qc_norm_transect(transect)) |>
    filter(!is.na(tr), tr %in% tl$tr) |>
    inner_join(ex |> transmute(obs_id = as.character(obs_id), latitude, longitude, url), by = "obs_id") |>
    filter(!is.na(latitude), !is.na(longitude))
  if (!nrow(cand)) {
    if (write && !is.null(out_path)) {
      dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
      write.csv(empty, out_path, row.names = FALSE, na = "")
    }
    message("  transect QC: 0 obs to flag"); return(empty)
  }

  pts <- sf::st_as_sf(cand, coords = c("longitude", "latitude"), crs = 4326) |>
    sf::st_transform(sf::st_crs(tl))
  pts$distance_m <- NA_real_
  for (code in unique(pts$tr)) {
    i <- which(pts$tr == code)
    pts$distance_m[i] <- as.numeric(sf::st_distance(pts[i, ], sf::st_union(tl[tl$tr == code, ])))
  }
  res <- sf::st_drop_geometry(pts) |>
    filter(distance_m > buffer_m) |>
    transmute(obs_id, observer, transect = tr, distance_m = round(distance_m, 1), url) |>
    arrange(desc(distance_m))
  if (write && !is.null(out_path)) {
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    write.csv(res, out_path, row.names = FALSE, na = "")
    message(sprintf("  transect QC: %d obs >%dm off their tagged transect -> %s", nrow(res), buffer_m, out_path))
  }
  res
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0)
  message("Sourced qc_misplaced_transect.R -- call qc_misplaced_transect(export_path, membership_path, kind=, out_path=).")
