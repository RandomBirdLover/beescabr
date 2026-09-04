# =============================================================
# project_info/surveys/rescue_on_transect_surveys.R
# beescabr -- rescue accidentally-UNTAGGED survey observations.
#
# WHAT IT'S FOR
#   The brain scores an obs as a survey only if it carries a Cabrillo survey TAG. But a surveyor
#   sometimes forgets to tag some of their obs on a day they WERE surveying -- those land as
#   status=="flag" (in the box, untagged) and get dropped, even though they sit right on the
#   transect that surveyor worked that day. This step rescues exactly those:
#
#     an untagged (status=="flag") obs is upgraded to a survey (status=="keep") IFF
#       * it is on a day that surveyor has a resolved survey transect (a survey day for them), AND
#       * it falls within `off_m` metres of the SAME transect they worked that day
#         (the majority transect resolve_transects() already picked -- the user's chosen rule).
#
#   Rescued obs are marked survey_source=="inferred_on_transect" (vs "tag" for genuinely-tagged
#   surveys) so nothing is laundered in -- you can always filter tag-confirmed vs inferred.
#
# HOW IT'S USED
#   Called by the brain (finding_project_info.R) on the membership, AFTER resolve_transects()
#   (which resolves each beeple survey day to its majority transect) and BEFORE fpi_survey_dates(),
#   so the rescued obs flow through to BOTH the survey-date counts and the cleaned tables at once.
#   coords = base |> select(obs_id, latitude, longitude); transects_sf = the transect lines with a
#   normalized `T` column (TP/BST/OT/UPMON).
#
# NOTE: this only ADDS obs on EXISTING survey days -- it never invents a survey day where nothing
#   was tagged at all (that "no-tag day" case is intentionally left to the calendar-window review).
# =============================================================
suppressWarnings(suppressMessages({library(dplyr); library(tibble); library(sf)}))

# fpi_surveyday_transect(): PURE. From the KEEP obs, the single resolved transect each surveyor
# worked each day (majority of their stamped transect values), plus that day's surveyor_type/year.
# One row per (observer, day). Flag/exclude obs never contribute.
fpi_surveyday_transect <- function(membership) {
  empty <- tibble(observer = character(), day = as.Date(character()),
                  resolved_transect = character(), surveyor_type = character(), survey_year = character())
  if (!nrow(membership) || !"status" %in% names(membership)) return(empty)
  keep <- membership |>
    filter(status == "keep") |>
    mutate(day = as.Date(observed_on)) |>
    filter(!is.na(transect), !toupper(trimws(as.character(transect))) %in% c("", "NA", "N/A"))
  if (!nrow(keep)) return(empty)
  keep |>
    group_by(observer, day) |>
    summarise(resolved_transect = names(sort(table(transect), decreasing = TRUE))[1],
              surveyor_type = dplyr::first(stats::na.omit(surveyor_type)),
              survey_year = dplyr::first(stats::na.omit(survey_year)),
              .groups = "drop")
}

# fpi_rescue_on_transect(): given membership + per-obs coords + the transect lines, upgrade each
# untagged (flag) obs that is on a survey day for its surveyor AND within off_m of THAT day's
# resolved transect. Adds a survey_source column ("tag" for existing keeps, "inferred_on_transect"
# for rescues, NA otherwise). The sf distance test is the only impure part.
fpi_rescue_on_transect <- function(membership, coords, transects_sf, off_m = 50) {
  m <- membership
  m$obs_id <- as.character(m$obs_id)                 # iNat ids arrive numeric -> keep join keys aligned
  if (!"survey_source" %in% names(m)) m$survey_source <- NA_character_
  m$survey_source[m$status == "keep"] <- "tag"     # pre-existing keeps are tag-confirmed

  sdt <- fpi_surveyday_transect(m)
  if (!nrow(sdt) || is.null(transects_sf) || !nrow(transects_sf) || !"T" %in% names(transects_sf))
    return(m)

  sdt <- sdt |> rename(sd_type = surveyor_type, sd_year = survey_year)         # avoid clashing with m's cols
  cand <- m |>
    mutate(day = as.Date(observed_on)) |>
    filter(status == "flag") |>
    inner_join(sdt, by = c("observer", "day")) |>                           # only that surveyor's survey days
    filter(!is.na(resolved_transect)) |>
    left_join(coords |> mutate(obs_id = as.character(obs_id)) |> distinct(obs_id, .keep_all = TRUE),
              by = "obs_id") |>
    filter(!is.na(latitude), !is.na(longitude))
  if (!nrow(cand)) return(m)

  cand$dist <- NA_real_
  for (tr in unique(cand$resolved_transect)) {
    line <- transects_sf[!is.na(transects_sf$T) & transects_sf$T == tr, ]
    if (!nrow(line)) next
    idx <- which(cand$resolved_transect == tr)
    pts <- st_as_sf(cand[idx, ], coords = c("longitude", "latitude"), crs = 4326) |>
      st_transform(st_crs(transects_sf))
    cand$dist[idx] <- as.numeric(st_distance(pts, st_union(st_geometry(line))))
  }
  resc <- cand |> filter(!is.na(dist), dist <= off_m)
  if (!nrow(resc)) return(m)

  ridx <- match(resc$obs_id, m$obs_id)
  m$status[ridx]        <- "keep"
  m$survey_source[ridx] <- "inferred_on_transect"
  m$transect[ridx]      <- resc$resolved_transect
  m$surveyor_type[ridx]   <- ifelse(is.na(resc$sd_type), m$surveyor_type[ridx], resc$sd_type)
  m$survey_year[ridx]   <- ifelse(is.na(resc$sd_year), m$survey_year[ridx], resc$sd_year)
  m$status_reason[ridx] <- sprintf("inferred survey: within %dm of transect %s on a survey day (tag missing)",
                                    as.integer(off_m), resc$resolved_transect)
  message(sprintf("  rescued %d untagged obs onto their survey-day transect (survey_source=inferred_on_transect)",
                  nrow(resc)))
  m
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0)
  message("Sourced rescue_on_transect_surveys.R -- fpi_rescue_on_transect(membership, coords, transects_sf).")
