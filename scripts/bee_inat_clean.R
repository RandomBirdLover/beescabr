# =============================================================
# Clean Non-Lethal iNaturalist Survey Data (Intern + Beeple)
# beescabr pipeline
# Author: Brandi Sanchez  |  Rewritten: 2026-07-06
#
# WHAT THIS DOES (one script, end to end):
#   0. FIELDMETA for any field row marked "fill in", fetch its datatype and
#              dropdown options from the iNat field-definition endpoint and
#              write them back into the crosswalk (one-time per field).
#   1. FETCH   non-lethal survey observations straight from the iNaturalist
#              API (v1) -- NOT the CSV export. The standard export drops tags
#              and observation fields (both unchecked to save size), and those
#              are exactly what the crosswalk triages on, so the API is the
#              only source that carries them. See README > iNaturalist API.
#              Scope = roster observers, CABR box, bees minus Apis mellifera,
#              any grade.
#   2. CLIP    to the real cabr_survey_box polygon (st_within, EPSG:26946),
#              same spatial logic as native_bee_checklist.R.
#   3. CLEAN   dates, missing-data flags, and data_source (intern vs beeple,
#              read from the roster's role column).
#   4. TRIAGE  every observation against project_tags_fields.csv:
#                keep    = carries a valid Cabrillo survey tag
#                exclude = carries an exclude tag (other project / pilot) --
#                          retained WITH a reason, never silently dropped
#                flag    = no recognized survey tag (personal/untagged) -> review
#              Tags are normalized via the crosswalk's inat_variants; the
#              TP-family transects fold to TP.
#   5. FIELDS  pull each observation's observation fields, mapped to their
#              crosswalk canonical by field_id (name is a fallback for rows
#              without an id yet). Folded synonyms share one canonical via a
#              ;-separated field_id list. Any field NOT in the crosswalk is
#              reported (built-in field discovery) so it can't silently fall behind.
#
# The crosswalk IS the spec: this script READS it. As you fill in field
# variants / add rows, the triage and field-folding improve with NO code change.
#
# NOTE: this is a live crawl each run. If you need a frozen dataset (so results
# don't shift when someone adds an observation between runs), save the fetched
# tables to a dated file and read that instead -- easy to add later.
#
# Outputs:
#   data/outputs/CABR_nonlethal_inat_clean.csv   -- cleaned + triaged observations
#   data/outputs/CABR_inat_unknown_fields.csv    -- fields seen but not in crosswalk
#   data/outputs/CABR_inat_unknown_tags.csv      -- tags seen but not in crosswalk
#
# REVIEWING THE TWO "unknown" FILES (do this after each run):
#   These are the pipeline's early-warning system. The crosswalk
#   (project_tags_fields.csv) only triages tags/fields it knows about; anything
#   it doesn't recognize is IGNORED. These two files list what got ignored, so
#   nothing important slips by silently.
#
#   unknown_fields.csv -- an observation field not in the crosswalk. If empty,
#     the crosswalk covers every field in the data (the goal). If rows appear,
#     someone used a new field: decide keep / fold / ignore, then (if keeping)
#     add a row to the crosswalk with its field_id. Re-run and it clears.
#
#   unknown_tags.csv -- a tag not in the crosswalk. This list is normally LONG
#     and mostly harmless (camera/lens tags like "D500", species names, photo
#     filenames, "City Nature Challenge", etc.) -- ignore those. You are scanning
#     for one thing only: a tag that looks like a SURVEY tag we missed -- a new
#     typo of a Cabrillo survey tag, or a new survey year. If you spot one, add
#     it as an inat_variant on the matching survey row in the crosswalk, re-run,
#     and those observations move from "flag" to "keep".
#
#   Rule of thumb: unknown_fields should trend to zero; unknown_tags won't (and
#   shouldn't) -- you're just skimming it for missed survey tags.
# =============================================================

library(tidyverse)
library(httr2)
library(sf)

# ---- Config / paths ---------------------------------------------------------
spatial_utils_path <- "scripts/spatial_utils.R"   # provides cabr_survey_box (EPSG:26946)
info_dir           <- "data/project_info"
roster_path        <- list.files(info_dir, pattern = "^observers_by_year.*\\.csv$",   full.names = TRUE)[1]
crosswalk_path     <- list.files(info_dir, pattern = "^project_tags_fields.*\\.csv$", full.names = TRUE)[1]
out_clean          <- "data/outputs/CABR_nonlethal_inat_clean.csv"
out_unknown        <- "data/outputs/CABR_inat_unknown_fields.csv"
out_unknown_tags   <- "data/outputs/CABR_inat_unknown_tags.csv"

TAXON_BEES  <- 630955   # Bees (Anthophila)
TAXON_HONEY <- 47219    # Apis mellifera (excluded)
UA          <- "beescabr bee_inat_clean (brandirenesanchez16@gmail.com)"

# helpers: normalize a tag/field string for matching; safe snake_case for columns
norm_key <- function(x) tolower(gsub("^#", "", trimws(x)))  # trim FIRST, then strip # (leading space was blocking ^#)
snake    <- function(x) { x <- tolower(x); x <- gsub("[^a-z0-9]+", "_", x); gsub("^_|_$", "", x) }

# always overwrite: clear an old file OR a stray folder at `path`, then write fresh
write_fresh <- function(x, path, ...) {
  if (dir.exists(path)) unlink(path, recursive = TRUE, force = TRUE)
  else if (file.exists(path)) unlink(path, force = TRUE)
  write_csv(x, path, ...)
}

# ---- 0. Boundary + roster + crosswalk --------------------------------------
source(spatial_utils_path)
if (!exists("cabr_survey_box"))
  stop("cabr_survey_box not found after sourcing ", spatial_utils_path)

roster <- read_csv(roster_path, show_col_types = FALSE)

usernames <- roster |>
  filter(method == "non-lethal", !is.na(inaturalist_username),
         trimws(inaturalist_username) != "") |>
  pull(inaturalist_username) |> unique()

# username -> data_source (intern / beeple) from the roster's role column
role_lookup <- roster |>
  filter(inaturalist_username %in% usernames) |>
  distinct(inaturalist_username, role) |>
  group_by(inaturalist_username) |>
  summarise(data_source = paste(sort(unique(role)), collapse = "/"), .groups = "drop")

crosswalk <- read_csv(crosswalk_path, show_col_types = FALSE)
message(length(usernames), " observers | crosswalk rows: ", nrow(crosswalk))

# ---- 0c. Fill field metadata (datatype + allowed_values) from the API --------
# For any field row still marked "fill in", look up its definition at
# inaturalist.org/observation_fields/{id}.json and record its datatype and
# dropdown options (blank if it's a free-text field). Idempotent: only fetches
# rows that still need it, then writes the filled-in crosswalk back to disk.
if (all(c("datatype", "allowed_values") %in% names(crosswalk))) {
  need <- which(crosswalk$allowed_values == "fill in" &
                !is.na(crosswalk$field_id) & trimws(crosswalk$field_id) != "")
  if (length(need) > 0) {
    message("Fetching field definitions for ", length(need), " field(s)...")
    for (i in need) {
      pid <- trimws(strsplit(crosswalk$field_id[i], ";")[[1]][1])   # primary id
      def <- tryCatch(
        request(sprintf("https://www.inaturalist.org/observation_fields/%s.json", pid)) |>
          req_user_agent(UA) |>
          req_retry(max_tries = 4,
                    is_transient = ~ resp_status(.x) %in% c(429, 500, 502, 503, 504),
                    backoff = ~ min(30, 3 * 2^(.x - 1))) |>
          req_perform() |> resp_body_json(),
        error = function(e) NULL)
      if (is.null(def)) next
      av <- def$allowed_values %||% ""
      av <- if (nzchar(av)) paste(trimws(strsplit(av, "\\|")[[1]]), collapse = " | ") else ""
      crosswalk$datatype[i]       <- def$datatype %||% crosswalk$datatype[i]
      crosswalk$allowed_values[i] <- av           # dropdown options, or blank if free text
      Sys.sleep(1)                                 # be polite to the API
    }
    write_csv(crosswalk, crosswalk_path, na = "")   # keep blank cells blank, not "NA"
    message("Updated field metadata -> ", crosswalk_path)
  }
}

# ---- 1. Fetch from the iNaturalist API (v1) --------------------------------
bb <- cabr_survey_box |> st_transform(4326) |> st_bbox()   # API needs WGS84; polygon clip is Step 2

params <- list(
  user_id = paste(usernames, collapse = ","),
  swlat = bb[["ymin"]], swlng = bb[["xmin"]],
  nelat = bb[["ymax"]], nelng = bb[["xmax"]],
  taxon_id = TAXON_BEES, without_taxon_id = TAXON_HONEY,
  per_page = 200, order_by = "id", order = "asc"
)

obs_base <- list(); tags_long <- list(); ofv_long <- list()
id_above <- 0; page <- 0

repeat {
  page <- page + 1
  resp <- request("https://api.inaturalist.org/v1/observations") |>
    req_user_agent(UA) |>
    # pause + back off on 429 (rate limit) / 5xx instead of crashing;
    # honors any Retry-After the API sends. backoff: 5,10,20,40,60s (capped).
    req_retry(max_tries = 6,
              is_transient = ~ resp_status(.x) %in% c(429, 500, 502, 503, 504),
              backoff = ~ min(60, 5 * 2^(.x - 1))) |>
    req_url_query(!!!params, id_above = id_above) |>
    req_perform() |> resp_body_json()

  results <- resp$results
  if (length(results) == 0) break

  obs_base[[page]] <- map_dfr(results, function(o) {
    coord <- o$geojson$coordinates
    tibble(
      obs_id          = o$id,
      observer        = o$user$login %||% NA_character_,
      lng             = if (is.null(coord)) NA_real_ else coord[[1]],
      lat             = if (is.null(coord)) NA_real_ else coord[[2]],
      observed_on     = o$observed_on %||% NA_character_,
      taxon_id        = o$taxon$id %||% NA_integer_,
      scientific_name = o$taxon$name %||% NA_character_,
      quality_grade   = o$quality_grade %||% NA_character_
    )
  })

  tags_long[[page]] <- map_dfr(results, function(o) {
    tg <- o$tags %||% list()
    if (length(tg) == 0) return(NULL)
    tibble(obs_id = o$id, tag = unlist(tg))
  })

  ofv_long[[page]] <- map_dfr(results, function(o) {
    if (length(o$ofvs) == 0) return(NULL)
    map_dfr(o$ofvs, function(f) tibble(
      obs_id     = o$id,
      field_id   = f$field_id %||% NA_integer_,
      field_name = f$name     %||% NA_character_,
      datatype   = f$datatype %||% NA_character_,
      value      = as.character(f$value %||% NA_character_)
    ))
  })

  last_id <- results[[length(results)]]$id
  message(sprintf("page %d: %d obs (through id %d)", page, length(results), last_id))
  id_above <- last_id
  if (length(results) < params$per_page) break
  Sys.sleep(1)   # be polite to the API
}

obs_base  <- bind_rows(obs_base)
tags_long <- bind_rows(tags_long)
ofv_long  <- bind_rows(ofv_long)

# ---- 2. Clip to cabr_survey_box (precise polygon) --------------------------
pts <- obs_base |>
  filter(!is.na(lng), !is.na(lat)) |>
  st_as_sf(coords = c("lng", "lat"), crs = 4326) |>
  st_transform(st_crs(cabr_survey_box))

kept <- pts$obs_id[lengths(st_within(pts, cabr_survey_box)) > 0]
message(sprintf("kept %d of %d located obs inside cabr_survey_box", length(kept), nrow(pts)))

obs_base  <- filter(obs_base, obs_id %in% kept)
tags_long <- filter(tags_long, obs_id %in% kept)
ofv_long  <- filter(ofv_long, obs_id %in% kept)

# ---- 3. Tag triage (crosswalk-driven) --------------------------------------
# Build a variant -> canonical/category map from every tag row (name + inat_variants).
tag_map <- crosswalk |>
  filter(type == "tag") |>
  transmute(canonical = name, category, variants = paste(name, inat_variants, sep = "; ")) |>
  separate_rows(variants, sep = ";") |>
  mutate(key = norm_key(variants)) |>
  filter(key != "", !grepl("^\\(", key)) |>          # drop "(none found...)" placeholders
  distinct(key, canonical, category)

tagged <- tags_long |>
  mutate(key = norm_key(tag)) |>
  left_join(tag_map, by = "key")

# 3a. Tags NOT in the crosswalk -> safety net, same idea as unknown fields.
# Lists any tag the crosswalk doesn't recognize so new/typo tags can't slip past.
unknown_tags <- tagged |>
  filter(is.na(canonical)) |>
  count(tag, sort = TRUE, name = "n_obs")

if (nrow(unknown_tags) > 0) {
  warning(nrow(unknown_tags), " tag(s) not in the crosswalk -- see ", out_unknown_tags)
  write_fresh(unknown_tags, out_unknown_tags)
}

obs_tags <- tagged |>
  group_by(obs_id) |>
  summarise(
    is_exclude  = any(category == "Exclude", na.rm = TRUE),
    exclude_tag = paste(unique(canonical[category == "Exclude"]), collapse = "; "),
    survey_tags = paste(unique(canonical[category %in% c("Beeple","Intern","General")]), collapse = "; "),
    has_survey  = any(category %in% c("Beeple","Intern","General"), na.rm = TRUE),
    survey_year = { yr <- str_extract(canonical[category %in% c("Beeple","Intern")], "\\d{4}")
                    yr <- yr[!is.na(yr)]; if (length(yr)) paste(unique(yr), collapse = "; ") else NA_character_ },
    survey_type = { st <- if_else(str_detect(canonical[category %in% c("Beeple","Intern")], "Intern"),
                                  "intern", "beeple")
                    st <- st[!is.na(st)]; if (length(st)) paste(unique(st), collapse = "; ") else NA_character_ },
    transect    = { tr <- canonical[category == "Transect"]
                    tr <- if_else(str_starts(tr, "TP"), "TP", tr)   # fold TP1/TP2/... -> TP
                    tr <- tr[!is.na(tr)]; if (length(tr)) paste(sort(unique(tr)), collapse = "; ") else NA_character_ },
    is_10min      = any(canonical == "CabrilloBee10MinuteSurvey", na.rm = TRUE),
    location_hint = any(category == "Location", na.rm = TRUE),
    .groups = "drop"
  )

# ---- 4. Field extraction (crosswalk-driven) --------------------------------
field_rows <- crosswalk |> filter(type %in% c("obs_field", "notes_field"))

# (a) primary match: by field_id. The field_id cell may hold a ;-separated list
#     (folded synonyms all mapping to one canonical), so explode it to one id/row.
id_map <- field_rows |>
  filter(!is.na(field_id), trimws(field_id) != "") |>
  transmute(canonical = name, field_id = as.character(field_id)) |>
  separate_rows(field_id, sep = ";") |>
  mutate(field_id = suppressWarnings(as.integer(trimws(field_id)))) |>
  filter(!is.na(field_id)) |>
  distinct(field_id, canonical)

# (b) fallback: by name/variant, for field rows that don't have an id yet
name_map <- field_rows |>
  transmute(canonical = name, variants = paste(name, inat_variants, sep = "; ")) |>
  separate_rows(variants, sep = ";") |>
  mutate(key = norm_key(variants)) |>
  filter(key != "", !grepl("^\\(", key)) |>
  distinct(key, canonical)

ofv_mapped <- ofv_long |>
  left_join(id_map, by = "field_id") |>                       # try field_id first
  mutate(key = norm_key(field_name)) |>
  left_join(name_map, by = "key", suffix = c("", "_nm")) |>
  mutate(canonical = coalesce(canonical, canonical_nm)) |>    # else fall back to name
  select(-canonical_nm, -key)

# 4a. Fields NOT in the crosswalk -> built-in discovery report
unknown_fields <- ofv_mapped |>
  filter(is.na(canonical)) |>
  group_by(field_id, field_name, datatype) |>
  summarise(n_obs = n_distinct(obs_id), .groups = "drop") |>
  arrange(desc(n_obs))

if (nrow(unknown_fields) > 0) {
  warning(nrow(unknown_fields), " observation field(s) not in the crosswalk -- see ", out_unknown)
  write_fresh(unknown_fields, out_unknown)
}

# 4b. Known fields -> one column per canonical field (f_<snake_name>)
known_wide <- ofv_mapped |>
  filter(!is.na(canonical)) |>
  mutate(col = paste0("f_", snake(canonical))) |>
  group_by(obs_id, col) |>
  summarise(value = first(value), .groups = "drop") |>
  pivot_wider(id_cols = obs_id, names_from = col, values_from = value)

# ---- 5. Assemble clean table + triage decision -----------------------------
clean <- obs_base |>
  left_join(role_lookup, by = c("observer" = "inaturalist_username")) |>
  left_join(obs_tags,  by = "obs_id") |>
  left_join(known_wide, by = "obs_id") |>
  mutate(
    observed_date  = as.Date(observed_on),
    missing_coords = is.na(lng) | is.na(lat),
    missing_taxon  = is.na(taxon_id),
    is_exclude     = coalesce(is_exclude, FALSE),
    has_survey     = coalesce(has_survey, FALSE),
    triage = case_when(
      is_exclude ~ "exclude",
      has_survey ~ "keep",
      TRUE       ~ "flag"
    ),
    triage_reason = case_when(
      is_exclude ~ paste0("exclude tag: ", exclude_tag),
      has_survey ~ "valid Cabrillo survey tag",
      TRUE       ~ "no recognized survey tag (personal/untagged) -- review"
    )
  ) |>
  relocate(obs_id, observer, data_source, observed_date, scientific_name,
           triage, triage_reason, survey_year, survey_type, transect, is_10min)

# ---- 6. Write + summary -----------------------------------------------------
if (nrow(clean) == 0) {
  cat("No non-lethal iNat observations returned for the current scope.\n")
} else {
  write_fresh(clean, out_clean)
  cat("\n--- NON-LETHAL CLEANING SUMMARY ---\n")
  cat("Observations kept in box:", nrow(clean), "\n\n")
  cat("By triage:\n");        print(count(clean, triage))
  cat("\nBy data_source:\n"); print(count(clean, data_source))
  cat("\nBy survey_year:\n"); print(count(clean, survey_year))
  cat("\nSaved cleaned data -> ", out_clean, "\n", sep = "")

  # ---- Action nudge: tell the person what needs reviewing --------------------
  nf <- nrow(unknown_fields); nt <- nrow(unknown_tags)
  if (nf > 0 || nt > 0) {
    cat("\n================= ACTION NEEDED =================\n")
    if (nf > 0)
      cat(sprintf("* %d new FIELD(S) in this export are not in the crosswalk.\n", nf),
          "    -> Review ", out_unknown, "\n",
          "    -> For each: keep as a new field, fold into an existing one, or ignore.\n",
          "       If keeping, add a row to project_tags_fields.csv with its field_id.\n", sep = "")
    if (nt > 0)
      cat(sprintf("* %d new TAG(S) in this export are not in the crosswalk.\n", nt),
          "    -> Review ", out_unknown_tags, "\n",
          "    -> Most are harmless (camera/species/filename tags) -- ignore those.\n",
          "       Only act on ones that look like a MISSED SURVEY TAG (new typo or year):\n",
          "       add it as an inat_variant on the matching survey row, then re-run.\n", sep = "")
    cat("Re-run after editing the crosswalk; these counts should shrink.\n")
    cat("================================================\n")
  } else {
    cat("\nNo new tags or fields to review -- crosswalk fully covers this export.\n")
  }
}
