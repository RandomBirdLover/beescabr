# =============================================================
# analysis/shared/forage_selectivity.R   (MODULE -- not a standalone figure)
#
# Single source of truth for bee-genus FORAGE SELECTIVITY: does a genus favor certain plants
# BEYOND what was actually available to it? For each bee genus a Monte-Carlo chi-square
# goodness-of-fit test compares its plant-visit counts to a MATCHED expectation -- what the
# rest of the community recorded in the SAME (month, year, survey-method) cells the genus
# appears in (leave-one-out, cell-weighted). So the availability baseline is corrected for
# phenology (which month), year (drought/rain/climate), AND method (net vs photo sample
# different plants). A genus that deviates (p<0.05) with enough records is "selective";
# otherwise it's a generalist. Its PREFERRED plant = the one most over-used relative to that
# matched availability (highest observed/expected) -- the honest "what it likes" vs "what it
# just gets." Observer identity is NOT controlled (well spread over many observers -> averages
# out). Plant DETECTABILITY (showy vs tiny flowers biasing photo probability) is NOT and cannot
# be controlled here -- there is no independent bloom census (see README limitations).
#
# Drives BOTH the interaction-web colors (interactions_network.R) AND the genus field
# guide's preference column (bee_field_guide_genus.R), so the two always agree. The older
# OVERALL-abundance p (ignores timing/year/method) is kept as chi_p_abundance for comparison.
#
# Remaining caveats: "availability" is the community's realized plant USE per cell (a strong
# proxy, not a bloom census); verdicts near p=0.05 (e.g. Dianthidium) are borderline.
#
# FINDINGS (data as of 2026-08-02; regenerate to refresh -- a snapshot, not hardcoded logic):
# 17 of 31 genera are selective; the set is STABLE across the abundance -> +month -> +year ->
# +method controls (i.e. the preferences are robust, not artifacts of when/how bees were
# sampled). Year matching did shift some FAVORITES: Bombus recorded most on Eriogonum but
# prefers Acmispon/deervetch (~46x); Diadasia prefers Opuntia (~108x, a cactus specialist);
# Andrena prefers Lasthenia (~23x); Habropoda Salvia (~34x); Hylaeus Baccharis (~19x). Halictus
# is weakly-but-significantly selective (Deinandra ~2.7x) once its flight timing is accounted
# for. Clear generalists (visit ~ availability): Megachile, Nomada.
#
# Loaded once at the top of run_all_analysis_pipeline.R and self-sourced by each consumer when run
# standalone. Defines functions only; writes nothing.
#
# Depends on: dplyr, stringr (+ config.R).
# =============================================================

suppressPackageStartupMessages({ library(dplyr); library(stringr) })
if (!exists("PATHS")) source("scripts/config.R")

SELECT_MIN_REC     <- 50                                            # REPORT floor: min plant-visit records to judge selectivity (& to state a field-guide preference)
SELECT_GENUS_RANKS <- c("species", "subspecies", "subgenus", "complex", "genus")
SELECT_B           <- 4999                                          # Monte-Carlo reps (higher = stabler p near the 0.05 cutoff)

# bee-genus x plant-genus visit matrix (plant rows, bee cols) from the two cleaned tables
.selectivity_matrix <- function() {
  cols <- c("genus", "taxon_rank", "plant_genus")
  grab <- function(p) { d <- read.csv(p, stringsAsFactors = FALSE, check.names = FALSE); d[cols] }
  inter <- bind_rows(grab(PATHS$specimen_clean), grab(PATHS$inat_clean))
  inter$genus       <- str_squish(inter$genus)
  inter$plant_genus <- str_squish(inter$plant_genus)
  inter$taxon_rank  <- tolower(str_squish(inter$taxon_rank))
  inter <- inter[!is.na(inter$plant_genus) & inter$plant_genus != "" &
                 !is.na(inter$genus) & inter$genus != "" &
                 inter$taxon_rank %in% SELECT_GENUS_RANKS, ]
  t <- table(inter$plant_genus, inter$genus)
  matrix(as.integer(t), nrow = nrow(t), dimnames = dimnames(t))
}

# long records (bee genus, plant genus, month, year, method) -- raw material for the
# season + year + method weighting. method = lethal (net/specimen) vs nonlethal (photo/iNat).
.selectivity_records <- function() {
  cols <- c("genus", "taxon_rank", "plant_genus", "observed_on", "survey_method")
  grab <- function(p) { d <- read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
                        if (!"survey_method" %in% names(d)) d$survey_method <- NA_character_; d[cols] }
  inter <- bind_rows(grab(PATHS$specimen_clean), grab(PATHS$inat_clean))
  inter$genus       <- str_squish(inter$genus)
  inter$plant_genus <- str_squish(inter$plant_genus)
  inter$taxon_rank  <- tolower(str_squish(inter$taxon_rank))
  inter$month       <- suppressWarnings(as.integer(substr(inter$observed_on, 6, 7)))
  inter$year        <- suppressWarnings(as.integer(substr(inter$observed_on, 1, 4)))
  inter$method      <- tolower(str_squish(inter$survey_method))
  inter$method[is.na(inter$method) | inter$method == ""] <- "unknown"
  inter[!is.na(inter$plant_genus) & inter$plant_genus != "" &
        !is.na(inter$genus) & inter$genus != "" &
        inter$taxon_rank %in% SELECT_GENUS_RANKS & !is.na(inter$month) & !is.na(inter$year),
        c("genus", "plant_genus", "month", "year", "method")]
}

# session cache (the Monte-Carlo chi-squares over ~31 genera are not free)
.sel_env <- new.env(parent = emptyenv())

# SEASON + YEAR + METHOD-AWARE selectivity. For each bee genus we ask: given the exact
# YEAR-MONTH-and-METHOD combinations it was actually recorded in (e.g. May-2021 by photo,
# Apr-2024 by net ...), did it visit plants differently from what the rest of the community
# recorded in those SAME cells? Expected plant use = sum over the genus's (year, month, method)
# cells of (its share of records in that cell) x (the community's plant-use shares in that same
# cell, leave-one-out). This controls for phenology (month), year (drought / rain / climate),
# AND survey method (net specimens sample different plants than iNat photos). A preference only
# counts if the genus over-used a plant relative to what was available in the same year-months
# and by the same method. Thin cells fall back method-preserving: (year,month,method) ->
# (month,method) -> (month) -> overall. Observer identity is NOT controlled (it is well spread
# across many observers, so it averages out); the old overall-abundance p is kept for comparison.
selectivity_table <- function(min_rec = SELECT_MIN_REC) {
  key <- as.character(min_rec)
  if (!is.null(.sel_env[[key]])) return(.sel_env[[key]])
  rec    <- .selectivity_records()
  plants <- sort(unique(rec$plant_genus)); P <- length(plants)
  bees   <- sort(unique(rec$genus))
  gmarg  <- as.numeric(table(factor(rec$plant_genus, plants))); gmarg <- gmarg / sum(gmarg)  # overall availability
  MIN_CELL <- 10                                                  # min community records to trust a cell
  REG      <- 0.05                                                # blend 5% uniform so no expected share is exactly 0
  rec$ym   <- rec$year * 100L + rec$month                         # year-month key
  share_of <- function(pg) { t <- as.numeric(table(factor(pg, plants))); if (sum(t) == 0) NULL else t / sum(t) }

  rows <- lapply(bees, function(b) {
    isb <- rec$genus == b
    rb  <- rec[isb, , drop = FALSE]
    x   <- as.numeric(table(factor(rb$plant_genus, plants))); names(x) <- plants
    n   <- sum(x)
    rc  <- rec[!isb, , drop = FALSE]                              # leave-one-out community

    # weight each (year, month, method) cell the genus appears in by the community's plant use there
    cw    <- table(paste(rb$ym, rb$method, sep = "|")) / n
    Epref <- numeric(P)
    for (k in names(cw)) {
      parts <- strsplit(k, "\\|", fixed = FALSE)[[1]]
      ymk   <- as.integer(parts[1]); mth <- parts[2]; mm <- ymk %% 100L
      m1 <- rc$ym == ymk & rc$method == mth                        # (year, month, method)
      sh <- if (sum(m1) >= MIN_CELL) share_of(rc$plant_genus[m1]) else NULL
      if (is.null(sh)) {                                           # -> (month, method)
        m2 <- rc$month == mm & rc$method == mth
        sh <- if (sum(m2) >= MIN_CELL) share_of(rc$plant_genus[m2]) else NULL
      }
      if (is.null(sh)) {                                           # -> (month) -> overall
        m3 <- rc$month == mm
        sh <- if (sum(m3) >= MIN_CELL) share_of(rc$plant_genus[m3]) else gmarg
      }
      if (is.null(sh)) sh <- gmarg
      Epref <- Epref + as.numeric(cw[k]) * sh
    }
    if (sum(Epref) <= 0) Epref <- gmarg
    Epref <- Epref / sum(Epref)
    Epref <- (1 - REG) * Epref + REG / P                          # regularise: strictly positive expected
    names(Epref) <- plants

    p_ym    <- if (n < 1) NA_real_ else tryCatch(
      suppressWarnings(chisq.test(x, p = Epref, simulate.p.value = TRUE, B = SELECT_B)$p.value),
      error = function(e) NA_real_)
    p_abund <- if (n < 1) NA_real_ else tryCatch(
      suppressWarnings(chisq.test(x, p = gmarg, simulate.p.value = TRUE, B = SELECT_B)$p.value),
      error = function(e) NA_real_)

    ratio <- ifelse(Epref > 0, (x / n) / Epref, NA_real_); names(ratio) <- plants   # observed vs season+year-expected
    elig  <- x >= pmax(3, 0.05 * n)                               # ignore 1-2 record blips when picking the favorite
    pref  <- if (any(elig)) names(which.max(ifelse(elig, ratio, -Inf))) else NA_character_
    yc    <- table(rb$year)

    data.frame(
      genus           = b,
      n_records       = n,
      n_plants        = sum(x > 0),
      n_years         = length(yc),                                # how many distinct years the records span
      top_year_pct    = if (n > 0) round(100 * max(yc) / n) else NA_integer_,  # % in its single biggest year
      chi_p           = p_ym,                                      # PRIMARY = season + year controlled
      chi_p_abundance = p_abund,                                   # reference = overall-abundance test
      selective       = !is.na(p_ym) & p_ym < 0.05 & n >= min_rec,
      top_plant       = names(sort(x, decreasing = TRUE))[1],      # raw most-visited (availability-blended)
      preferred_plant = pref,                                      # most-visited vs season+year-expected availability
      preferred_ratio = if (is.na(pref)) NA_real_ else round(unname(ratio[pref]), 1),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out <- out[order(-out$n_records), ]
  .sel_env[[key]] <- out
  out
}

# ---- vectorised lookups -------------------------------------------------------
selective_genera <- function(min_rec = SELECT_MIN_REC) {
  t <- selectivity_table(min_rec); t$genus[t$selective]            # ordered most-recorded first
}
is_selective <- function(genus, min_rec = SELECT_MIN_REC) {
  t <- selectivity_table(min_rec); unname(setNames(t$selective, t$genus)[genus]) %in% TRUE
}
preferred_plant_of <- function(genus, min_rec = SELECT_MIN_REC) {
  t <- selectivity_table(min_rec); unname(setNames(t$preferred_plant, t$genus)[genus])
}

# One-line forage-preference verdict for a genus, for the field guide.
# plant_fmt: a function to render a plant genus (defaults to identity; the guide passes
# plant_label so it reads as a common name).
forage_preference_label <- function(genus, plant_fmt = function(x) x, min_rec = SELECT_MIN_REC) {
  t   <- selectivity_table(min_rec)
  idx <- match(genus, t$genus)
  vapply(seq_along(genus), function(i) {
    j <- idx[i]
    if (is.na(j)) return("-")
    r <- t[j, ]
    if (isTRUE(r$selective) && !is.na(r$preferred_plant))
      sprintf("Selective -> %s (%.1fx vs available)", plant_fmt(r$preferred_plant), r$preferred_ratio)
    else if (!is.na(r$n_records) && r$n_records >= min_rec)
      "Generalist (visits ~ availability)"
    else
      "not enough records"
  }, character(1))
}

# =============================================================
# SPECIES-level forage preference -- the SAME matched test, but the focal taxon is a SPECIES
# (Genus epithet) and the community is every other record (leave-one-out). Populated only for
# species with >= SELECT_MIN_REC plant-visit records (a handful today, more as sampling grows);
# "too few records to judge" below that. Used by the SPECIES field guide. The genus path above is
# left untouched (it drives the web colors); .forage_core mirrors selectivity_table()'s maths on
# a generic `taxon` column -- keep the two in sync if the test ever changes.
# =============================================================
.selectivity_records_species <- function() {
  cols <- c("genus", "species", "taxon_rank", "plant_genus", "observed_on", "survey_method")
  grab <- function(p) { d <- read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
                        if (!"survey_method" %in% names(d)) d$survey_method <- NA_character_; d[cols] }
  inter <- bind_rows(grab(PATHS$specimen_clean), grab(PATHS$inat_clean))
  inter$genus       <- str_squish(inter$genus)
  inter$plant_genus <- str_squish(inter$plant_genus)
  inter$taxon_rank  <- tolower(str_squish(inter$taxon_rank))
  epi <- tolower(word(str_squish(inter$species), -1))
  inter$taxon <- ifelse(inter$taxon_rank %in% c("species", "subspecies") & inter$genus != "" &
                          !is.na(epi) & epi != "", paste(inter$genus, epi), NA_character_)
  inter$month  <- suppressWarnings(as.integer(substr(inter$observed_on, 6, 7)))
  inter$year   <- suppressWarnings(as.integer(substr(inter$observed_on, 1, 4)))
  inter$method <- tolower(str_squish(inter$survey_method))
  inter$method[is.na(inter$method) | inter$method == ""] <- "unknown"
  inter[!is.na(inter$taxon) & !is.na(inter$plant_genus) & inter$plant_genus != "" &
        !is.na(inter$month) & !is.na(inter$year), c("taxon", "plant_genus", "month", "year", "method")]
}

# generic matched-availability test on a `taxon` column (mirror of selectivity_table's core).
# Skips the Monte-Carlo for sub-threshold taxa (they can't be judged anyway) to stay fast.
.forage_core <- function(rec, min_rec) {
  plants <- sort(unique(rec$plant_genus)); P <- length(plants); taxa <- sort(unique(rec$taxon))
  gmarg <- as.numeric(table(factor(rec$plant_genus, plants))); gmarg <- gmarg / sum(gmarg)
  MIN_CELL <- 10; REG <- 0.05; rec$ym <- rec$year * 100L + rec$month
  share_of <- function(pg) { t <- as.numeric(table(factor(pg, plants))); if (sum(t) == 0) NULL else t / sum(t) }
  rows <- lapply(taxa, function(b) {
    isb <- rec$taxon == b; rb <- rec[isb, , drop = FALSE]
    x <- as.numeric(table(factor(rb$plant_genus, plants))); names(x) <- plants; n <- sum(x)
    rc <- rec[!isb, , drop = FALSE]
    cw <- table(paste(rb$ym, rb$method, sep = "|")) / n; Epref <- numeric(P)
    for (k in names(cw)) {
      parts <- strsplit(k, "\\|")[[1]]; ymk <- as.integer(parts[1]); mth <- parts[2]; mm <- ymk %% 100L
      m1 <- rc$ym == ymk & rc$method == mth; sh <- if (sum(m1) >= MIN_CELL) share_of(rc$plant_genus[m1]) else NULL
      if (is.null(sh)) { m2 <- rc$month == mm & rc$method == mth; sh <- if (sum(m2) >= MIN_CELL) share_of(rc$plant_genus[m2]) else NULL }
      if (is.null(sh)) { m3 <- rc$month == mm; sh <- if (sum(m3) >= MIN_CELL) share_of(rc$plant_genus[m3]) else gmarg }
      if (is.null(sh)) sh <- gmarg
      Epref <- Epref + as.numeric(cw[k]) * sh
    }
    if (sum(Epref) <= 0) Epref <- gmarg
    Epref <- Epref / sum(Epref); Epref <- (1 - REG) * Epref + REG / P; names(Epref) <- plants
    p_ym  <- if (n < min_rec) NA_real_ else tryCatch(
      suppressWarnings(chisq.test(x, p = Epref, simulate.p.value = TRUE, B = SELECT_B)$p.value), error = function(e) NA_real_)
    ratio <- ifelse(Epref > 0, (x / n) / Epref, NA_real_); names(ratio) <- plants
    elig  <- x >= pmax(3, 0.05 * n); pref <- if (any(elig)) names(which.max(ifelse(elig, ratio, -Inf))) else NA_character_
    data.frame(taxon = b, n_records = n, n_plants = sum(x > 0), chi_p = p_ym,
               selective = !is.na(p_ym) & p_ym < 0.05 & n >= min_rec,
               top_plant = names(sort(x, decreasing = TRUE))[1], preferred_plant = pref,
               preferred_ratio = if (is.na(pref)) NA_real_ else round(unname(ratio[pref]), 1), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows); out[order(-out$n_records), ]
}

selectivity_table_species <- function(min_rec = SELECT_MIN_REC) {
  key <- paste0("sp:", min_rec); if (!is.null(.sel_env[[key]])) return(.sel_env[[key]])
  out <- .forage_core(.selectivity_records_species(), min_rec); .sel_env[[key]] <- out; out
}

# One-line forage-preference verdict for a SPECIES (Genus epithet), for the species field guide.
forage_preference_label_species <- function(species, plant_fmt = function(x) x, min_rec = SELECT_MIN_REC) {
  t <- selectivity_table_species(min_rec); idx <- match(species, t$taxon)
  vapply(seq_along(species), function(i) {
    j <- idx[i]; if (is.na(j)) return("-"); r <- t[j, ]
    if (isTRUE(r$selective) && !is.na(r$preferred_plant))
      sprintf("Selective -> %s (%.1fx vs available)", plant_fmt(r$preferred_plant), r$preferred_ratio)
    else if (!is.na(r$n_records) && r$n_records >= min_rec)
      "Generalist (visits ~ availability)"
    else
      "not enough records"
  }, character(1))
}
