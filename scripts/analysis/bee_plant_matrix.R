# =============================================================
# analysis/bee_plant_matrix.R
# beescabr -- WHICH PLANT GENERA DOES EACH BEE SPECIES VISIT?
#
# A presence matrix: one row per bee species, one column per plant genus, a check
# where that bee has been recorded on that plant. Both methods pooled (iNaturalist
# photos + netted specimens) because the question is what a bee USES, not how hard
# we looked for it with one method.
#
# Scope choice, stated on the page: SPECIES-level records only. A record identified
# only to genus cannot say which species did the visiting, so it is excluded -- which
# is why the matrix covers fewer records than the flower totals elsewhere. Subspecies
# roll up into their species.
#
# HONESTY: a check is PRESENCE, not preference. Common plants collect visits simply by
# being common, and a bee with one flower record gets a check as dark as one with five
# hundred. The per-bee record count is shown so a reader can weigh each row, and the
# availability-corrected question ("does this bee PREFER this plant?") is a different
# analysis -- see forage_selectivity.R and the field guide's Forage preference column.
#
# Pure builders (bpm_*) are unit-tested in tests/testthat/test-bee-plant-matrix.R.
# Run from the repo root:  Rscript scripts/analysis/bee_plant_matrix.R
# =============================================================
suppressPackageStartupMessages({ library(dplyr); library(stringr) })

# ---- pure: records -> long bee/plant pairs with a visit count ------------------
# Identity comes from taxon_id joined to the reference lookup, NOT from pasting the
# record's genus + species strings together. The lookup (sd_bee_taxonomy_lookup.csv)
# is the project's authority for what a taxon_id means, so:
#   * a subspecies rolls up to its species because its lookup row names the parent
#     species (Anthophora urbana ssp. clementina -> Anthophora urbana), and
#   * a stale or misspelled name on a record cannot invent a bee -- the id decides.
# An id absent from the lookup is DROPPED rather than guessed at.
bpm_pairs <- function(df, lookup, name_col = NULL) {
  empty <- data.frame(bee = character(0), plant = character(0), n = integer(0),
                      stringsAsFactors = FALSE)
  if (!all(c("taxon_id", "taxon_rank", "plant_genus") %in% names(df)) || !nrow(df)) return(empty)
  if (!all(c("taxon_id", "genus", "species") %in% names(lookup))) return(empty)

  rank  <- tolower(str_squish(df$taxon_rank))
  plant <- str_squish(df$plant_genus)
  # A record is usable if it is a species-level record on a named plant AND can be
  # resolved to a bee at all -- by id, or (for the unpublished taxa) by name.
  nm <- if (!is.null(name_col) && name_col %in% names(df)) str_squish(df[[name_col]]) else rep(NA_character_, nrow(df))
  nm[!is.na(nm) & nm == ""] <- NA_character_
  keep <- rank %in% c("species", "subspecies") & !is.na(plant) & plant != "" &
          (!is.na(df$taxon_id) | !is.na(nm))
  if (!any(keep)) return(empty)

  # id -> canonical "Genus species" from the lookup (blank species = not a species row)
  key <- ifelse(str_squish(lookup$species) == "" | is.na(lookup$species), NA_character_,
                paste(str_squish(lookup$genus), str_squish(lookup$species)))
  # Skip missing ids explicitly: match(NA, x) matches NA TO NA, and the lookup holds 17
  # id-less rows, so a plain match would relabel every id-less record as whichever of
  # those rows comes first in the file. An absent id resolves by name below, or not at all.
  tid <- df$taxon_id[keep]
  bee <- rep(NA_character_, length(tid))
  has <- !is.na(tid)
  if (any(has)) bee[has] <- key[match(tid[has], lookup$taxon_id)]

  # NAME FALLBACK -- narrow and deliberate. 17 checklist bees have no taxon_id because
  # iNaturalist has not published a taxon for them (dev-docs/LIMITATIONS.md), and one of
  # them (Lasioglossum turgiventre) carries flower records. An id-only join would drop
  # them silently. The fallback still resolves through the SAME reference table --
  # matching its scientific_name and returning ITS canonical key -- so an unknown name is
  # rejected rather than invented, and a subspecies still rolls up to its species.
  gap <- is.na(bee) & !is.na(nm[keep])
  if (any(gap) && "scientific_name" %in% names(lookup)) {
    lk_nm <- str_squish(lookup$scientific_name)
    lk_nm[is.na(lk_nm) | lk_nm == ""] <- NA_character_    # never match on a blank name
    bee[gap] <- key[match(nm[keep][gap], lk_nm)]
  }

  ok <- !is.na(bee)
  if (!any(ok)) return(empty)
  data.frame(bee = bee[ok], plant = plant[keep][ok], stringsAsFactors = FALSE) |>
    count(bee, plant, name = "n") |>
    as.data.frame()
}

# ---- pure: long pairs -> wide matrix (bee rows x plant columns, 0 = never) ------
# Ordered by plant breadth (most plants first), then name: the bees a reader is most
# likely to be looking for sit at the top, and the long tail of one-plant bees sinks.
bpm_matrix <- function(pairs) {
  if (!nrow(pairs)) return(data.frame(bee = character(0), stringsAsFactors = FALSE))
  bees   <- sort(unique(pairs$bee))
  plants <- sort(unique(pairs$plant))
  m <- matrix(0L, nrow = length(bees), ncol = length(plants), dimnames = list(bees, plants))
  m[cbind(match(pairs$bee, bees), match(pairs$plant, plants))] <- as.integer(pairs$n)
  out <- data.frame(bee = bees, m, check.names = FALSE, stringsAsFactors = FALSE,
                    row.names = NULL)
  breadth <- rowSums(m > 0)
  out[order(-breadth, out$bee), , drop = FALSE]
}

# ---- pure: per-bee summary that travels with the matrix ------------------------
bpm_summary <- function(pairs) {
  if (!nrow(pairs)) return(data.frame(bee = character(0), n_records = integer(0),
                                      n_plants = integer(0), stringsAsFactors = FALSE))
  pairs |> group_by(bee) |>
    summarise(n_records = sum(n), n_plants = dplyr::n(), .groups = "drop") |>
    arrange(desc(n_plants), bee) |> as.data.frame()
}

# ---- build (skipped when this file is merely sourced for its helpers) ----------
if (!exists("BPM_SOURCED_FOR_HELPERS")) {
  if (!exists("PATHS"))       source("scripts/config.R")
  if (!exists("scope_cap"))   source("scripts/analysis/theme_beescabr.R")
  if (!exists("plant_label")) source("scripts/analysis/plant_names.R")
  if (!exists("inat_photo_link")) source("scripts/analysis/inat_taxon_links.R")

  OUT_DIR <- file.path(DIR_REPORT, "reference/nps_summary")   # lives with the other NPS summary tables
  dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

  rd <- function(p) read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
  keep_cols <- c("taxon_id", "taxon_rank", "plant_genus", "scientific_name")
  recs  <- bind_rows(rd(PATHS$inat_clean)     %>% select(all_of(keep_cols)),
                     rd(PATHS$specimen_clean) %>% select(all_of(keep_cols)))
  pairs <- bpm_pairs(recs, read.csv(PATHS$taxonomy_lookup, stringsAsFactors = FALSE),
                     name_col = "scientific_name")
  mat   <- bpm_matrix(pairs)
  summ  <- bpm_summary(pairs)

  write.csv(pairs, file.path(OUT_DIR, "bee_plant_pairs.csv"), row.names = FALSE)
  write.csv(mat,   file.path(OUT_DIR, "bee_plant_matrix.csv"), row.names = FALSE)
  message(sprintf("Bee species x plant genus: %d species x %d plant genera, %d visited pairs",
                  nrow(mat), ncol(mat) - 1L, nrow(pairs)))
  # Records we could NOT place: no taxon_id, or an id absent from the reference lookup.
  # Report how each flower record resolved. A record with no taxon_id is EXPECTED for
  # the 17 checklist bees iNaturalist has not published a taxon for (dev-docs/LIMITATIONS.md);
  # those resolve by name against the same lookup and DO enter the matrix. Anything that
  # resolves neither way is a real gap -- named, never silently dropped.
  .sp <- recs[tolower(str_squish(recs$taxon_rank)) %in% c("species", "subspecies") &
              !is.na(recs$plant_genus) & recs$plant_genus != "", ]
  .lk  <- read.csv(PATHS$taxonomy_lookup, stringsAsFactors = FALSE)
  .key <- ifelse(str_squish(.lk$species) == "" | is.na(.lk$species), NA_character_,
                 paste(str_squish(.lk$genus), str_squish(.lk$species)))
  .b <- rep(NA_character_, nrow(.sp)); .h <- !is.na(.sp$taxon_id)
  if (any(.h)) .b[.h] <- .key[match(.sp$taxon_id[.h], .lk$taxon_id)]
  .g <- is.na(.b)
  if (any(.g)) .b[.g] <- .key[match(str_squish(.sp$scientific_name)[.g], str_squish(.lk$scientific_name))]
  .by_name <- sum(.g & !is.na(.b))
  .lost    <- sum(is.na(.b))
  if (.by_name)
    message(sprintf("  %d flower record(s) had no taxon_id and were matched by name via %s (expected: iNaturalist has no published taxon for these bees -- see dev-docs/LIMITATIONS.md)",
                    .by_name, basename(PATHS$taxonomy_lookup)))
  if (.lost)
    message(sprintf("  WARNING: %d flower record(s) resolved by NEITHER id nor name -- excluded. Names: %s",
                    .lost, paste(unique(str_squish(.sp$scientific_name)[is.na(.b)]), collapse = "; ")))
}
