# =============================================================
# project_info/resolve_beeple_transects_per_survey.R
# beescabr -- pick the ONE transect that goes in master_per_survey_info.csv for each
# BEEPLE survey day.
#
# WHAT IT'S FOR
#   A beeple surveys ONE transect per day, but their individual iNat obs sometimes carry a
#   stray wrong transect tag (e.g. 50 tagged TP + 3 accidentally UPMON). This picks the real
#   transect for that surveyor-day by the MAJORITY of their tags, so the survey record
#   (master_per_survey_info.csv) lists the correct single transect -- not the typos.
#
# HOW IT'S USED
#   Called by the brain (finding_project_info.R) on the membership, AFTER it is built and
#   BEFORE the survey dates. resolve_transects(membership) returns list(membership, mistags, ties):
#     * membership -- every beeple obs re-stamped with its day's resolved `transect` (the raw tag
#       is kept in `transect_tagged`). finding_survey_dates.R then groups these per surveyor-day,
#       so the resolved value becomes the `transects` cell in master_per_survey_info.csv.
#     * mistags    -- outvoted obs -> review_mistagged_transects.csv (obs URL, tagged vs should_be).
#     * ties       -- no clear majority (looks like two transects in a day) -> review_transect_overlap.csv;
#       rule it in review_transect_ties() ("both" keeps a genuine two-transect day). If you rule any,
#       the brain re-runs at stage 3f so master_per_survey_info.csv reflects it THIS run.
#
#   INTERNS are LEFT ALONE -- they walk a multi-transect route, so their obs keep their own tags
#   (only status=="keep" & surveyor_type=="beeple" obs are resolved).
#
# Pure tag counting -- no shapefiles. Outputs: mistags -> data/observations/review/ (per-obs
# fixes); ties -> data/project_info/review/ (the ruling that sets the survey-record transect).
# =============================================================
suppressWarnings(suppressMessages({library(dplyr); library(tibble); library(readr)}))

ST_MISTAG_OUT <- "data/observations/review/review_mistagged_transects.csv"
ST_TIES_OUT   <- "data/project_info/review/review_transect_overlap.csv"

st_norm_transect <- function(x) {
  u <- toupper(gsub("^#", "", trimws(as.character(x))))
  dplyr::case_when(
    u %in% c("", "NA", "N/A") ~ NA_character_,
    startsWith(u, "TP")    ~ "TP",     # TP / TP1 / TP2 -> TP
    startsWith(u, "UPMON") ~ "UPMON",
    startsWith(u, "BST")   ~ "BST",
    u == "OT"              ~ "OT",
    TRUE                   ~ u)
}

# membership -> list(membership, mistags, ties). `ties` is the tie-review table (the
# brain writes it); prior rulings in ties_path are read back and applied here.
resolve_transects <- function(membership, ties_path = ST_TIES_OUT) {
  m <- membership
  if (!"transect_tagged" %in% names(m)) m$transect_tagged <- m$transect   # preserve the raw tag
  st <- if ("surveyor_type" %in% names(m)) as.character(m$surveyor_type) else rep(NA_character_, nrow(m))

  base <- tibble(
    .row = seq_len(nrow(m)), obs_id = as.character(m$obs_id), observer = m$observer,
    d = as.Date(m$observed_on),
    kind = if ("kind" %in% names(m)) m$kind else NA_character_,
    trn = st_norm_transect(m$transect),
    beeple_survey = (m$status == "keep" & st == "beeple" & !is.na(st)))

  # tag counts per beeple surveyor-day (only obs carrying a transect tag vote)
  votes <- base |> filter(beeple_survey, !is.na(trn)) |> count(observer, d, trn, name = "n")
  perday <- votes |> group_by(observer, d) |>
    summarise(win_str = paste(sort(trn[n == max(n)]), collapse = "|"),
              n_win   = sum(n == max(n)),
              counts  = paste(paste0(trn, ":", n)[order(-n)], collapse = " | "),
              .groups = "drop")

  # ---- prior tie rulings (persisted in ties_path) ----
  prior <- tibble(observer = character(), d = as.Date(character()),
                  decision = character(), decision_note = character())
  if (file.exists(ties_path)) {
    p <- suppressWarnings(read_csv(ties_path, show_col_types = FALSE))
    if (all(c("inat_username", "date", "decision") %in% names(p)))
      prior <- tibble(observer = as.character(p$inat_username), d = as.Date(p$date),
                      decision = tolower(trimws(as.character(p$decision))),
                      decision_note = if ("decision_note" %in% names(p)) as.character(p$decision_note) else NA_character_)
  }

  clear <- perday |> filter(n_win == 1) |> transmute(observer, d, resolved = win_str)
  ties  <- perday |> filter(n_win > 1)  |> left_join(prior, by = c("observer", "d"))

  # a tie resolves only when the ruling names ONE of the tied transects ("both"/blank stay multi)
  if (nrow(ties)) {
    tie_resolved <- ties |>
      mutate(dec_up = toupper(coalesce(decision, "")),
             .hit   = mapply(function(dec, win) dec %in% strsplit(win, "\\|")[[1]],
                             dec_up, win_str, USE.NAMES = FALSE)) |>
      filter(.hit) |>
      transmute(observer, d, resolved = dec_up)
  } else {
    tie_resolved <- tibble(observer = character(), d = as.Date(character()), resolved = character())
  }

  stamp <- bind_rows(clear, tie_resolved)
  base <- base |> left_join(stamp, by = c("observer", "d")) |> arrange(.row)
  if (!"resolved" %in% names(base)) base$resolved <- NA_character_
  base$transect_new <- ifelse(base$beeple_survey & !is.na(base$resolved), base$resolved, base$trn)
  m$transect <- base$transect_new                       # stamp resolved onto every obs

  mistags <- base |>
    filter(beeple_survey, !is.na(resolved), !is.na(trn), trn != resolved) |>
    transmute(obs_id, observer, date = d, kind, tagged = trn, should_be = resolved,
              url = paste0("https://www.inaturalist.org/observations/", obs_id)) |>
    arrange(observer, date)

  # ---- tie-review table: per tie day, the tag counts + a few URLs, ruling persisted ----
  urls <- base |> filter(beeple_survey) |> semi_join(ties, by = c("observer", "d")) |>
    group_by(observer, d) |>
    summarise(obs_urls = paste(paste0("https://www.inaturalist.org/observations/",
                                      head(obs_id, 15L)), collapse = "; "), .groups = "drop")
  ties_out <- ties |> left_join(urls, by = c("observer", "d")) |>
    transmute(inat_username = observer, date = d, tag_counts = counts,
              obs_urls = coalesce(obs_urls, ""),
              decision = decision, decision_note = decision_note) |>
    arrange(inat_username, date)

  list(membership = m, mistags = mistags, ties = ties_out)
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0)
  message("Sourced resolve_beeple_transects_per_survey.R -- resolve_transects(membership) is called by the brain.")
