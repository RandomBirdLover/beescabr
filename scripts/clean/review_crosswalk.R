# =============================================================
# clean/review_crosswalk.R
# beescabr -- interactive review of unknown tags & fields
# Rewritten 2026-07-14 for the concept-per-row crosswalk_master.csv.
#
# Feels like the bee-name resolver: it shows each unknown tag/field one at a
# time and you pick where it goes:
#   <number> = roll it into that existing concept (tp, cabr_bee_general_survey,
#              flower_visiting, ignore, an exclude row, ...)
#   i        = shortcut: roll into the "ignore" catch-all row
#   n        = new concept (asks name + what_for)
#   s        = skip     q = save & quit
#
# Rolling in appends the item to that concept's variant cell (deduped):
#   a tag   -> inat_tag_variants
#   a field -> inat_field_variants
#
# Run: source("scripts/clean/review_crosswalk.R"); review_unknowns("tags")
#      then                                        review_unknowns("fields")
# =============================================================

library(dplyr); library(readr); library(stringr)

CW_PATH    <- "data/project_info/crosswalk_master.csv"
UNK_TAGS   <- "data/project_info/crosswalk_unknown_bee_tags.csv"
UNK_FIELDS <- "data/project_info/crosswalk_unknown_bee_fields.csv"

.split <- function(s) if (is.na(s) || s == "") character(0) else str_trim(str_split(s, ";\\s*")[[1]])
.join  <- function(v) { v <- unique(v[!is.na(v) & v != "" & !(tolower(v) %in% c("n/a", "na"))])
                        if (length(v)) paste(v, collapse = "; ") else NA_character_ }

# append one value to row i's column `col`, deduped
cw_append <- function(cw, i, col, val) {
  cw[[col]][i] <- .join(c(.split(cw[[col]][i]), val)); cw
}

# make sure an "ignore" catch-all row exists; return list(cw, i)
cw_ensure_ignore <- function(cw) {
  i <- which(cw$name == "ignore")
  if (length(i)) return(list(cw = cw, i = i[1]))
  new <- cw[1, ]; new[] <- NA
  new$name <- "ignore"; new$what_for <- "ignore"
  new$applies_to_plant_bee_or_both <- "both"; new$nonlethal_or_lethal <- "both"
  new$notes <- "catch-all: tags/fields reviewed and dismissed as not relevant"
  cw <- bind_rows(cw, new)
  list(cw = cw, i = nrow(cw))
}

review_unknowns <- function(kind = c("tags", "fields"), cw_path = CW_PATH,
                            prompt_fn = readline, write = TRUE, max_items = Inf) {
  kind <- match.arg(kind)
  cw <- read_csv(cw_path, show_col_types = FALSE, col_types = cols(.default = "c"))
  cw <- cw[!(is.na(cw$name) & is.na(cw$what_for)), ]        # drop blank rows

  unk <- read_csv(if (kind == "tags") UNK_TAGS else UNK_FIELDS, show_col_types = FALSE)
  item_col   <- if (kind == "tags") "tag" else "field"
  target_col <- if (kind == "tags") "inat_tag_variants" else "inat_field_variants"
  if ("cabrillo_ish" %in% names(unk)) unk <- arrange(unk, desc(cabrillo_ish), desc(n_obs))
  else if ("n_obs" %in% names(unk))   unk <- arrange(unk, desc(n_obs))
  unk <- head(unk, max_items)
  if (!nrow(unk)) { message("Nothing to review for ", kind, "."); return(invisible(cw)) }

  e <- cw_ensure_ignore(cw); cw <- e$cw
  concept_i <- which(!is.na(cw$name) & cw$name != "")

  message(sprintf("Reviewing %d unknown %s. Roll each into a concept, or i/n/s/q.", nrow(unk), kind))
  changed <- FALSE
  for (k in seq_len(nrow(unk))) {
    it <- unk[[item_col]][k]
    n_obs <- if ("n_obs" %in% names(unk)) unk$n_obs[k] else NA
    cat(sprintf("\n[%d/%d] unknown %s: \"%s\"%s\n", k, nrow(unk), sub("s$", "", kind), it,
                if (!is.na(n_obs)) sprintf("  (%s obs)", n_obs) else ""))
    nm <- cw$name[concept_i]
    cat("  roll into:\n   ", paste(sprintf("%d=%s", seq_along(nm), nm), collapse = "   "), "\n")
    cat("  or:  i=ignore   n=new concept   s=skip   q=save & quit\n")
    ans <- tolower(trimws(prompt_fn("> ")))

    if (ans == "q") break
    if (ans %in% c("s", "")) next
    if (ans == "i") { cw <- cw_append(cw, which(cw$name == "ignore")[1], target_col, it); changed <- TRUE; next }
    if (ans == "n") {
      nn <- trimws(prompt_fn("  new concept name: "))
      wf <- trimws(prompt_fn("  what_for: "))
      new <- cw[1, ]; new[] <- NA; new$name <- nn; new$what_for <- wf
      cw <- bind_rows(cw, new)
      cw <- cw_append(cw, nrow(cw), target_col, it)
      concept_i <- which(!is.na(cw$name) & cw$name != ""); changed <- TRUE; next
    }
    idx <- suppressWarnings(as.integer(ans))
    if (!is.na(idx) && idx >= 1 && idx <= length(nm)) {
      cw <- cw_append(cw, concept_i[idx], target_col, it); changed <- TRUE
    } else cat("  ? didn't understand -- skipped\n")
  }

  if (changed && write) { write.csv(cw, cw_path, row.names = FALSE, na = ""); cat("\nSaved crosswalk_master.csv\n") }
  else cat("\nNo changes written.\n")
  invisible(cw)
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0)
  message('Sourced. Run: review_unknowns("tags")  or  review_unknowns("fields")')
