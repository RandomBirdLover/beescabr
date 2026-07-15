# =============================================================
# clean/specimen_bee_clean.R
# beescabr pipeline -- CABR bee specimen cleaning (orchestrator)
# Rewritten: 2026-07-13 (split into pure helpers + skippable review gate)
#
# Loads the newest specimen .xlsx, runs the QC/clean transforms from
# clean/specimen_clean.R, and writes the cleaned records + QC side files.
#
# The taxonomy spell-check keeps its MANUAL REVIEW GATE, but it is now
# skippable/loggable rather than a hard interactive stop:
#   - Interactive terminal (default): prompts to confirm you've fixed flagged
#     names in the source .xlsx; answering 'n' stops as before.
#   - Non-interactive (BEESCABR_NONINTERACTIVE=1): flags are written and
#     logged, and the run CONTINUES with the current data. This is what lets
#     it run unattended if you ever want it in an automated chain.
# The main pipeline no longer auto-invokes this script (specimen evidence is
# optional there); run it yourself when you're ready to refresh specimens.
#
# Run: Rscript scripts/clean/specimen_bee_clean.R
#      BEESCABR_NONINTERACTIVE=1 Rscript scripts/clean/specimen_bee_clean.R
# =============================================================

library(dplyr)
library(stringr)

local({
  need <- function(sym, file) if (!exists(sym)) source(file.path("scripts", file))
  need("PATHS",                      "config.R")
  need("write_fresh",                "utils/utils.R")
  need("standardize_specimen_names", "clean/specimen_clean.R")
})

clean_specimens <- function(interactive_ok = (Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1"),
                            prompt_fn = readline, verbose = TRUE) {
  lookup_path         <- PATHS$taxonomy_lookup
  inat_checklist_path <- PATHS$checklist_sd_county_inat
  flags_out   <- "data/outputs/specimens/cabr_specimen_bee_taxonomy_flags.csv"
  missing_out <- "data/outputs/specimens/cabr_specimen_bee_missing.csv"
  dupes_out   <- "data/outputs/specimens/cabr_specimen_bee_duplicates.csv"
  clean_out   <- PATHS$specimen_clean

  specimens_path <- read_latest("data/cabr_surveys/lethal", "^cabr_bee_specimens_record_V")
  message("Loading specimens: ", basename(specimens_path))
  raw <- readxl::read_excel(specimens_path)
  require_columns(raw,
                  c("date", "latitude", "longitude", "sdnhm_id", "ucsd_id", "accession",
                    "locality", "genus", "subgenus", "complex", "species", "subspecies",
                    "missing_specimen"),
                  "raw specimens")
  message("Loaded ", nrow(raw), " specimen rows")

  df <- raw |> parse_specimen_dates() |> standardize_specimen_names()

  # --- taxonomy fill + spell-check (needs bee_taxonomy_lookup.csv) ---
  if (file.exists(lookup_path)) {
    tax_lookup <- read.csv(lookup_path, na.strings = "") |>
      filter(!is.na(genus), genus != "") |>
      select(genus, species, subspecies, family, subfamily, tribe) |>
      distinct(genus, species, subspecies, .keep_all = TRUE)
    df <- fill_specimen_taxonomy(df, tax_lookup)

    tax_check <- read.csv(lookup_path, na.strings = "") |>
      filter(!is.na(genus), genus != "") |>
      mutate(genus = str_to_title(genus), species = str_to_lower(species))
    inat_species <- if (file.exists(inat_checklist_path)) {
      read.csv(inat_checklist_path, na.strings = "") |>
        filter(!is.na(genus), genus != "", !is.na(species), species != "") |>
        mutate(genus = str_to_title(genus), species = str_to_lower(species)) |>
        distinct(genus, species)
    } else tibble(genus = character(), species = character())

    known <- build_known_names(tax_check, inat_species)
    flags <- compute_taxonomy_flags(df, known$genera, known$genus_species)

    dir.create(dirname(flags_out), recursive = TRUE, showWarnings = FALSE)
    write_fresh(flags, flags_out, row.names = FALSE)
    message(sprintf("\n--- TAXONOMY SPELL-CHECK (%d flag(s)) -> %s ---", nrow(flags), flags_out))
    if (nrow(flags) > 0 && verbose) print(as.data.frame(flags))

    decision <- resolve_flag_gate(nrow(flags), interactive_ok, prompt_fn)
    if (decision == "stop")
      stop("Stopping. Fix the flagged names in the source .xlsx, then re-run specimen_bee_clean.R.")
    if (decision == "continue" && nrow(flags) > 0)
      message(sprintf("Continuing with %d unresolved taxonomy flag(s) (logged to %s).",
                      nrow(flags), flags_out))
  } else {
    message("WARNING: ", lookup_path, " not found -- taxonomy fill + spell-check skipped.")
    message("         Run taxonomy_lookup_build.R first to generate it.")
  }

  # --- QC flags + side lists ---
  df <- add_qc_flags(df)
  missing_specimens_list <- df |> filter(missing_specimen == "Y")
  write_fresh(missing_specimens_list, missing_out, row.names = FALSE)
  duplicates_list <- detect_duplicate_ids(df)
  write_fresh(duplicates_list, dupes_out, row.names = FALSE)

  # --- complex match (needs the internal complex map) ---
  # complex_taxon_id is stripped from the public iNat checklists, so the
  # species->complex_taxon_id map is read from the internal file that
  # taxonomy_lookup_build.R writes for exactly this purpose.
  complex_map_path <- PATHS$complex_map
  if (file.exists(complex_map_path)) {
    checklist <- read.csv(complex_map_path)
    require_columns(checklist, c("genus", "species", "complex", "complex_taxon_id"), "complex_map")
    df <- match_specimen_complex(df, build_complex_lookup(checklist))
  } else {
    message("WARNING: complex map not found -- complex not matched.")
    if (!"complex" %in% names(df))          df$complex <- NA_character_
    if (!"complex_taxon_id" %in% names(df)) df$complex_taxon_id <- NA
  }

  # --- column order, control-char strip, old-name tracking ---
  df <- df |>
    relocate(genus, subgenus, complex, complex_taxon_id, species, subspecies, .after = tribe) |>
    strip_control_chars() |>
    build_old_scientific_name() |>
    relocate(old_scientific_name, .after = old_species_name)

  dir.create(dirname(clean_out), recursive = TRUE, showWarnings = FALSE)
  write_fresh(df, clean_out, row.names = FALSE)

  message("\n--- SPECIMEN CLEANING SUMMARY ---")
  message("Total specimens:    ", nrow(df))
  message("Missing genus:      ", sum(df$missing_genus, na.rm = TRUE))
  message("Physically missing: ", nrow(missing_specimens_list))
  message("Potential dupes:    ", nrow(duplicates_list))
  message("Saved -> ", clean_out)
  invisible(df)
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER")) clean_specimens()
