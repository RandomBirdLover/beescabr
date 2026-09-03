# =============================================================
# NPS summary tables -- descriptive data summaries ONLY (no interpretation)
# beescabr / Cabrillo National Monument (CABR) native bees
#
# For the data-focused NPS report: plain counts a reader can cite, with NO analysis
# or interpretation. Four table sets:
#   1. Participation   -- people, trips, survey-days, years, transects, by role.
#   2. Bees found      -- record + genus + species counts (overall and by method),
#                         plus a full bee species checklist.
#   3. Methods         -- records by method x surveyor type.
#   4. Plants found    -- plant genera/species recorded, plus a plant checklist.
#
# SCOPE: all records (the report describes the whole dataset). Everything here is a
# straight count -- deliberately no tests, rates, or interpretation.
#
# Run from the repo root:  Rscript scripts/analysis/nps_summary_tables.R
# Depends on: dplyr, stringr (+ config.R).
# =============================================================

suppressPackageStartupMessages({ library(dplyr); library(stringr) })

if (!exists("PATHS")) source("scripts/config.R")
if (!exists("iucn_code_of")) try(source("scripts/analysis/conservation_status.R"), silent = TRUE)   # IUCN status lookup (optional)
.iucn_of <- function(sp) if (exists("iucn_code_of")) iucn_code_of(sp) else rep("NE", length(sp))
OUT_DIR       <- file.path(DIR_REPORT, "reference/nps_summary")
SPECIES_RANKS <- c("species", "subspecies")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
kv <- function(...) { x <- list(...); data.frame(metric = names(x), value = unlist(x), row.names = NULL) }
tok <- function(v) { t <- unlist(strsplit(as.character(v), "[,;/&]")); t <- str_squish(t); unique(t[t != "" & !is.na(t)]) }

spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
psf  <- read.csv(if (!is.null(PATHS$per_survey)) PATHS$per_survey else
                 PATHS$per_survey, stringsAsFactors = FALSE, check.names = FALSE)

# ---- 1. participation --------------------------------------------------------
# EFFORT (trips, days, method split) comes from the per-survey trip log; WHO surveyed
# comes from the roster. The trip log CANNOT count people: it stores netters by first
# name and iNat folks by handle, so one person recurs across trips/years and can't be
# deduped (that produced the old inflated "39 field / 24 iNat" figures). The roster is
# one row per person-year, so distinct people = unique full names. Role != method:
# 2021-2023 interns netted (lethal), 2024 interns switched to photos (non-lethal), so a
# person can count under both methods -- we report people by ROLE and surveys by METHOD.
uniq_lc  <- function(v) { v <- str_squish(tolower(as.character(v))); unique(v[v != "" & !is.na(v)]) }
psf$year_i <- suppressWarnings(as.integer(psf$year))
pmethod  <- str_squish(tolower(psf$method))                                    # trip method: lethal / non-lethal

# people.csv = WHO exists (declared, one row each). participation.csv = who was in the
# field which year, DERIVED from the survey record -- so an assignment nobody showed up
# to can no longer inflate a headcount, which is what the old roster allowed.
rost <- read.csv(PATHS$people, stringsAsFactors = FALSE, check.names = FALSE)
rost <- rost[as.logical(rost$surveyor) %in% TRUE, , drop = FALSE]
prt  <- if (!is.null(PATHS$participation) && file.exists(PATHS$participation))
          read.csv(PATHS$participation, stringsAsFactors = FALSE, check.names = FALSE) else
          data.frame(person_id = character(0), role = character(0))
person   <- str_squish(tolower(paste(rost$first_name, rost$last_name)))        # canonical people key, one row per human
role_ids <- function(r) unique(prt$person_id[str_squish(tolower(prt$role)) == r])
ndistinct <- function(mask) length(unique(person[mask & person != "" & !is.na(person)]))

# General public = iNat observers who are NOT on the roster (matched by handle); total
# contributors = dedicated roster people + public, with no double count (roster handles
# are removed from the public set first).
public_n <- length(setdiff(uniq_lc(inat$observer), uniq_lc(rost$inaturalist_username)))
dedic_n  <- ndistinct(TRUE)

part <- kv(
  years_covered              = paste(range(psf$year_i, na.rm = TRUE), collapse = "–"),
  n_years                    = length(unique(na.omit(psf$year_i))),
  transects                  = length(tok(psf$transects)),
  total_surveys              = nrow(psf),
  netting_surveys            = sum(pmethod == "lethal",     na.rm = TRUE),
  inaturalist_surveys        = sum(pmethod == "non-lethal", na.rm = TRUE),
  interns                    = ndistinct(rost$person_id %in% role_ids("intern")),
  beeple                     = ndistinct(rost$person_id %in% role_ids("beeple")),
  dedicated_surveyors        = dedic_n,
  public_contributors        = public_n,
  total_contributors         = dedic_n + public_n)
write.csv(part, file.path(OUT_DIR, "nps_participation.csv"), row.names = FALSE)
# (transect count is in nps_participation above; the transect NAMES list is reference/design
#  info, not a summary output, so it lives in data/project_info/, not here.)

# ---- 2. bees found -----------------------------------------------------------
beek <- function(df, method) data.frame(
  method = method, genus = str_squish(df$genus), taxon_rank = str_squish(tolower(df$taxon_rank)),
  species = df$species, stringsAsFactors = FALSE) %>%
  mutate(species_key = ifelse(taxon_rank %in% SPECIES_RANKS & !is.na(genus) & genus != "" &
                                !is.na(species) & species != "", paste(genus, word(species, -1)), NA))
bees <- bind_rows(beek(spec, "lethal (specimen)"), beek(inat, "non-lethal (iNaturalist)"))

# subspecies-rank records (trinomials) -- kept separately so they can have their own
# checklist; they are ALSO folded into species_total/checklist above (via species_key),
# so this is additive detail, not a double count.
subk <- function(df, method) {
  r <- str_squish(tolower(df$taxon_rank)); keep <- r == "subspecies" & !is.na(r)
  if (!any(keep)) return(NULL)
  d <- df[keep, , drop = FALSE]
  # authoritative full name rendered all-italic: drop the roman "ssp."/"subsp." connector
  disp <- str_squish(gsub("\\s+(ssp|subsp)\\.?\\s+", " ", d$scientific_name))
  fb <- str_squish(paste(d$genus, d$species, d$subspecies)); bad <- disp == "" | is.na(disp)
  disp[bad] <- fb[bad]
  data.frame(method = method, genus = str_squish(d$genus), subspecies = disp, stringsAsFactors = FALSE)
}
subsp <- bind_rows(subk(spec, "lethal (specimen)"), subk(inat, "non-lethal (iNaturalist)"))

gkey <- function(d) unique(na.omit(d$genus[d$genus != ""]))
skey <- function(d) unique(na.omit(d$species_key))
bees_summary <- kv(
  total_bee_records          = nrow(bees),
  records_lethal             = sum(bees$method == "lethal (specimen)"),
  records_nonlethal          = sum(bees$method == "non-lethal (iNaturalist)"),
  genera_total               = length(gkey(bees)),
  species_total              = length(skey(bees)),
  subspecies_total           = length(unique(subsp$subspecies)),
  genera_lethal              = length(gkey(bees[bees$method == "lethal (specimen)", ])),
  genera_nonlethal           = length(gkey(bees[bees$method == "non-lethal (iNaturalist)", ])),
  species_lethal             = length(skey(bees[bees$method == "lethal (specimen)", ])),
  species_nonlethal          = length(skey(bees[bees$method == "non-lethal (iNaturalist)", ])))
write.csv(bees_summary, file.path(OUT_DIR, "nps_bees_summary.csv"), row.names = FALSE)

# full bee species checklist (species-resolved), with record counts + methods
checklist <- bees %>% filter(!is.na(species_key)) %>%
  group_by(species = species_key) %>%
  summarise(genus = genus[1], n_records = n(),
            in_specimens = any(method == "lethal (specimen)"),
            in_inaturalist = any(method == "non-lethal (iNaturalist)"), .groups = "drop") %>%
  arrange(genus, species) %>%
  mutate(iucn = .iucn_of(species)) %>%
  select(species, genus, iucn, n_records, in_specimens, in_inaturalist)
write.csv(checklist, file.path(OUT_DIR, "nps_bee_checklist_species.csv"), row.names = FALSE)

# subspecies checklist (trinomials only), parallel columns to the species checklist
subsp_checklist <- if (nrow(subsp)) {
  subsp %>% filter(!is.na(subspecies), subspecies != "") %>%
    group_by(subspecies) %>%
    summarise(genus = genus[1], n_records = n(),
              in_specimens = any(method == "lethal (specimen)"),
              in_inaturalist = any(method == "non-lethal (iNaturalist)"), .groups = "drop") %>%
    arrange(genus, subspecies) %>%
    mutate(iucn = .iucn_of(word(subspecies, 1, 2))) %>%   # subspecies inherit their species' IUCN status
    select(subspecies, genus, iucn, n_records, in_specimens, in_inaturalist)
} else data.frame(subspecies = character(), genus = character(), iucn = character(), n_records = integer(),
                  in_specimens = logical(), in_inaturalist = logical())
write.csv(subsp_checklist, file.path(OUT_DIR, "nps_bee_checklist_subspecies.csv"), row.names = FALSE)

# genera checklist (includes genus-only records)
gen_checklist <- bees %>% filter(!is.na(genus), genus != "") %>%
  group_by(genus) %>% summarise(n_records = n(),
            n_species_resolved = n_distinct(species_key[!is.na(species_key)]), .groups = "drop") %>%
  arrange(genus)
write.csv(gen_checklist, file.path(OUT_DIR, "nps_bee_checklist_genus.csv"), row.names = FALSE)

# ---- 3. methods x surveyor type ---------------------------------------------
styp <- function(df, method) data.frame(method = method,
  surveyor = ifelse(is.na(df$surveyor_type) | str_squish(df$surveyor_type) == "",
                    "unattributed", str_squish(tolower(df$surveyor_type))), stringsAsFactors = FALSE)
ms <- bind_rows(styp(spec, "lethal (specimen)"), styp(inat, "non-lethal (iNaturalist)"))
methods_tbl <- as.data.frame.matrix(table(ms$method, ms$surveyor))
methods_tbl <- cbind(method = rownames(methods_tbl), methods_tbl, total = rowSums(methods_tbl))
write.csv(methods_tbl, file.path(OUT_DIR, "nps_methods.csv"), row.names = FALSE)

# ---- 4. plants found ---------------------------------------------------------
# PUBLIC page: only trustworthy plant IDs -> RESEARCH-GRADE iNaturalist observations, PLUS the plant
# tags on the netted specimens. (The raw iNat plant file also holds needs-ID/casual records; we drop
# those here so a reader can trust every plant on the list.)
inat_pl <- read.csv(PATHS$inat_plant_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat_pl <- inat_pl[str_squish(tolower(inat_pl$quality_grade)) == "research", ]
plant_src <- bind_rows(
  data.frame(plant_genus = str_squish(inat_pl$plant_genus), plant_species = str_squish(inat_pl$plant_species), stringsAsFactors = FALSE),
  data.frame(plant_genus = str_squish(spec$plant_genus),    plant_species = str_squish(spec$plant_species),    stringsAsFactors = FALSE))
plant_src <- plant_src[!is.na(plant_src$plant_genus) & plant_src$plant_genus != "", ]
pg <- plant_src$plant_genus; ps <- plant_src$plant_species
plants_summary <- kv(
  plant_records              = nrow(plant_src),
  plant_genera_recorded      = length(unique(pg[pg != "" & !is.na(pg)])),
  plant_species_recorded     = length(unique(ps[ps != "" & !is.na(ps)])))
write.csv(plants_summary, file.path(OUT_DIR, "nps_plants_summary.csv"), row.names = FALSE)
plant_checklist <- plant_src %>%
  group_by(plant_genus) %>% summarise(n_records = n(),
            n_species = n_distinct(plant_species[plant_species != "" & !is.na(plant_species)]),
            .groups = "drop") %>% arrange(plant_genus)
write.csv(plant_checklist, file.path(OUT_DIR, "nps_plant_checklist_genus.csv"), row.names = FALSE)

# ============================================================================
# 5. RENDERED TABLES (HTML + PNG) -- same styling family as the field guides, with a
#    scope caption (Scope | Method | Rank | Source) ABOVE each table. The CSVs above
#    stay the machine-readable source; these are the read-and-cite version.
# ============================================================================
if (!exists("scope_cap")) source("scripts/analysis/theme_beescabr.R")   # shared scope-caption format
esc <- function(x) { x <- gsub("&", "&amp;", x); x <- gsub("<", "&lt;", x); gsub(">", "&gt;", x) }
# metric_key -> display label: title-case, iNaturalist re-cased, with a few hand-set overrides
.metric_labels <- c(
  n_years              = "Number of Years",
  total_surveys        = "Total Surveys",
  netting_surveys      = "Lethal Surveys",
  inaturalist_surveys  = "Non-Lethal Surveys",
  dedicated_surveyors  = "Total Dedicated Surveyors",
  public_contributors  = "General Public Contributors",
  total_contributors   = "Total Contributors")
pretty_metric <- function(m) {
  out <- gsub("Inaturalist", "iNaturalist", tools::toTitleCase(gsub("_", " ", m)))
  hit <- m %in% names(.metric_labels); out[hit] <- unname(.metric_labels[m[hit]])
  out
}

# Plain-English caption per table (this page is public-facing, so we skip the technical
# Scope|Method|Rank format used on the analysis figures). Each keeps a short, citable source line.
SRC       <- paste0("Source: iNaturalist photos and netted specimens, Cabrillo National Monument (data as of ", bee_data_asof(), ").")
SRC_PLANT <- paste0("Source: research-grade iNaturalist observations and netted-specimen tags, Cabrillo National Monument (data as of ", bee_data_asof(), ").")
pub_cap <- function(desc, src = SRC) paste0(desc, " ", src)
cap_part   <- pub_cap("How much surveying went into this project across all years. The survey rows count effort: the trips and transects walked, split by how bees were recorded (netting versus iNaturalist photos). The people rows count individuals, not trips. Dedicated surveyors are the interns and beeple who ran the structured surveys, and general public contributors are everyone else who photographed a park bee on iNaturalist.")
cap_bees   <- pub_cap("Every native bee recorded in the park, all years combined. Totals for how they were found (netted as a specimen, or photographed on iNaturalist) and how many genera and species.")
cap_meth   <- pub_cap("How the bees were recorded, netted specimens versus iNaturalist photos, and by whom.")
cap_plants <- pub_cap("The plants bees were seen visiting across the park, all years. Total visits, plus how many kinds of plant. Only trustworthy IDs are counted: research-grade iNaturalist records and specimen tags.", SRC_PLANT)
cap_chk    <- pub_cap("Every bee identified to species, with how many times it was recorded and whether it turned up as a netted specimen, an iNaturalist photo, or both. A low record count does not always mean a species is rare: records that could only be named to genus are not counted toward a species, so the count can understate how present a bee really is. IUCN is each bee's Red List status. Codes: NE (Not Evaluated), DD (Data Deficient), LC (Least Concern), NT (Near Threatened), VU (Vulnerable), EN (Endangered), CR (Critically Endangered).")
cap_sschk  <- pub_cap("Bees identified all the way down to subspecies, a finer level than species.")
cap_gchk   <- pub_cap("Every bee genus found, including bees that could only be identified to genus. A low record count does not always mean a genus is rare, since a small number can reflect how little a bee has been recorded as much as true rarity.")
cap_pchk   <- pub_cap("Every plant genus bees were seen on, with how many bee visits and how many plant species. Only trustworthy IDs are counted: research-grade iNaturalist records and specimen tags.", SRC_PLANT)

# ---- HTML ------------------------------------------------------------------
df_to_html <- function(df, caption, heading, metric_col = FALSE, italic_cols = character(0)) {
  d <- df
  if (metric_col) d[[1]] <- pretty_metric(d[[1]])
  # A column is right-aligned ("num") if it is genuinely numeric, OR it is the value
  # column of a metric table -- those hold numbers (500, 6) but are stored as character
  # because of ranges like "2021-2026", so they'd otherwise fall through to left-align.
  num_align <- vapply(seq_along(d), function(j)
    is.numeric(d[[j]]) || (metric_col && j == 2L), logical(1))
  hd <- paste0(vapply(seq_along(d), function(j) {
      cls <- if (is.logical(d[[j]]) || names(d)[j] == "iucn") ' class="chk"' else if (num_align[j]) ' class="num"' else ""   # header aligns with its column's values
      sprintf('<th%s>%s</th>', cls, esc(gsub("_", " ", names(d)[j])))
    }, ""), collapse = "")
  body <- vapply(seq_len(nrow(d)), function(i) {
    cells <- vapply(seq_along(d), function(j) {
      v <- d[[j]][i]
      if (names(d)[j] == "iucn")   # IUCN code -> color chip (all NE for these bees)
        return(sprintf('<td class="chk"><span class="iucn i-%s">%s</span></td>', tolower(as.character(v)), esc(as.character(v))))
      if (is.logical(d[[j]]))   # boolean column -> teal checkmark (yes) / muted dash (no)
        return(sprintf('<td class="chk">%s</td>', if (isTRUE(v)) '<span class="yes">&#10003;</span>' else '<span class="no">&ndash;</span>'))
      val <- if (is.numeric(d[[j]])) format(v, big.mark = ",", trim = TRUE) else esc(as.character(v))
      if (names(d)[j] %in% italic_cols) val <- sprintf('<i>%s</i>', val)   # scientific taxon column -> italic
      sprintf('<td%s>%s</td>', if (num_align[j]) ' class="num"' else "", val)
    }, "")
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  }, "")
  # Caption BELOW its table: it explains what the numbers are, which is a question you
  # ask after seeing them. Each table carries its own, so there is no page-level box.
  paste0('<h2>', esc(heading), '</h2>',
         sprintf('<table class="t%s"><thead><tr>', if (ncol(d) <= 3) " compact" else ""), hd, '</tr></thead><tbody>',
         paste(body, collapse = ""), '</tbody></table>',
         '<p class="scope">', esc(caption), '</p>')
}

# headline totals for the intro bar
.gv <- function(df, k) df$value[df$metric == k]
n_bg <- .gv(bees_summary, "genera_total");    n_bs  <- .gv(bees_summary, "species_total")
n_bss <- .gv(bees_summary, "subspecies_total"); n_pg <- .gv(plants_summary, "plant_genera_recorded")
html <- paste0(
'<!doctype html><html><head><meta charset="utf-8"><title>Cabrillo National Monument &mdash; Native Bee Summary Tables</title><style>',
# static multi-table page -> shares the BEE_HTML color tokens + polished card look of the interactive
# tables, but NOT their sticky/sortable header behavior (these are static reference tables).
'*{box-sizing:border-box}',
paste0('html{background:', BEE_HTML[["page_alt"]], '}'),
paste0("body{font:15px/1.55 -apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;color:", BEE_HTML[["ink"]], ";max-width:940px;margin:26px auto;background:", BEE_HTML[["page"]], ";padding:34px 34px 46px;border:1px solid ", BEE_PANEL[["card_border"]], ";border-radius:14px;box-shadow:0 1px 2px rgba(20,20,20,.05),0 14px 40px rgba(20,20,20,.06);-webkit-font-smoothing:antialiased}"),
paste0('.org{font-size:12.5px;font-weight:700;text-transform:uppercase;letter-spacing:.09em;color:', BEE_HTML_GREEN[["mid"]], ';margin:0 0 3px}'),
paste0('h1{font-size:25px;font-weight:700;letter-spacing:-.01em;margin:0;white-space:nowrap;color:', BEE_HTML_GREEN[["deep"]], '}'),
paste0("h1:after{content:'';display:block;width:56px;height:3px;background:", BEE_HTML_GREEN[["mid"]], ";border-radius:2px;margin:11px 0 2px}"),
paste0('.byline{font-size:13px;color:', BEE_HTML[["sub"]], ';margin:7px 0 0;font-style:italic}'),
paste0('h2{font-size:14px;font-weight:700;text-transform:uppercase;letter-spacing:.05em;margin:30px 0 6px;color:', BEE_HTML_GREEN[["deep"]], '}'),
paste0('p.sub{color:', BEE_HTML[["sub"]], ';margin:13px 0 4px;font-size:13.5px}'),
paste0('p.scope{color:', BEE_HTML[["scope"]], ';margin:14px 0 6px;font-size:12px;font-weight:600;background:', BEE_HTML[["head_bg"]], ';border-left:3px solid ', BEE_HTML_GREEN[["mid"]], ';padding:9px 13px;border-radius:0 7px 7px 0}'),
'table.t{border-collapse:separate;border-spacing:0;width:100%;font-size:13px;margin:8px 0 6px}',
paste0('table.t th,table.t td{text-align:left;padding:8px 12px;border-bottom:1px solid ', BEE_HTML[["border_lt"]], '}'),
paste0('table.t th{position:sticky;top:0;z-index:2;background:', BEE_HTML[["head_bg"]], ';font-weight:700;text-transform:uppercase;letter-spacing:.04em;font-size:11px;color:', BEE_HTML_GREEN[["deep"]], ';border-bottom:2px solid ', BEE_HTML_GREEN[["light"]], ';white-space:nowrap}'),
paste0('table.t tbody tr:nth-child(even){background:', BEE_TABLE[["row_even"]], '}'),
paste0('table.t tbody tr:hover{background:', BEE_HTML[["row_hover"]], '}'),
'table.t th.num{text-align:right}table.t td.num{text-align:right;font-variant-numeric:tabular-nums}',
'table.t th.chk,table.t td.chk{text-align:center}',
# few-column tables (metric key/value, genus + counts) size to content + left-align so the
# columns sit together instead of stretching apart across the full card width.
'table.t.compact{width:auto;min-width:340px}',
'table.t.compact td,table.t.compact th{padding-right:30px}',
paste0('.yes{color:', BEE_HTML_GREEN[["mid"]], ';font-weight:700;font-size:15px}'),
'.no{color:', BEE_PANEL[["empty"]], '}',
'.iucn{display:inline-block;padding:2px 8px;border-radius:5px;font-size:10.5px;font-weight:700}',
bee_badge_css(BEE_IUCN_BG, BEE_IUCN_FG, function(k) paste0(".iucn.i-", k)),   # IUCN chips (all NE here)
'</style></head><body>',
'<div class="org">Cabrillo National Monument</div>',
'<h1>Native Bee Summary Tables &#128029;</h1>',
'<div class="byline">by Brandi Sanchez</div>',
sprintf('<p class="sub">A by-the-numbers look at the native bees of Cabrillo National Monument. How many native bees have been found, and the flowers they visit. So far: <b>%s bee genera</b>, <b>%s species</b>, and <b>%s subspecies</b>, recorded on <b>%s plant genera</b>. Everything here is a plain count you can look up and cite, with no statistics or interpretation, just what was recorded in the field.</p>', n_bg, n_bs, n_bss, n_pg),
df_to_html(part,            cap_part,   "1. Participation", metric_col = TRUE),
df_to_html(bees_summary,    cap_bees,   "2. Bees found",    metric_col = TRUE),
df_to_html(methods_tbl,     cap_meth,   "3. Records by method x surveyor type"),
df_to_html(plants_summary,  cap_plants, "4. Plants found",  metric_col = TRUE),
df_to_html(gen_checklist,   cap_gchk,   "Bee genera checklist",   italic_cols = "genus"),          # broad -> fine: genus,
df_to_html(checklist,       cap_chk,    "Bee species checklist", italic_cols = c("species", "genus")),   # then species,
df_to_html(subsp_checklist, cap_sschk,  "Bee subspecies checklist", italic_cols = c("subspecies", "genus")),  # then subspecies
df_to_html(plant_checklist, cap_pchk,   "Plant genera checklist", italic_cols = "plant_genus"),
'</body></html>')
writeLines(html, file.path(OUT_DIR, "nps_summary_tables.html"))

# ---- PNG (the four compact summary tables; long checklists stay CSV/HTML) ----
if (requireNamespace("gridExtra", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE)) {
  cap_all <- scope_cap(scope = "whole dataset: every record, all years, whole park (plain counts, no test or interpretation)",
                       method = "lethal + non-lethal pooled",
                       rank = "records, genera & species", width = 10000)
  th <- gridExtra::ttheme_minimal(base_size = 8,
    core = list(fg_params = list(hjust = 0, x = 0.02), bg_params = list(fill = c(BEE_TABLE[["row_odd"]], BEE_TABLE[["row_even"]]))),
    colhead = list(fg_params = list(hjust = 0, x = 0.02, fontface = "bold")))
  mk  <- function(df, metric_col = FALSE) {
    d <- df; if (metric_col) d[[1]] <- pretty_metric(d[[1]])
    names(d) <- gsub("_", " ", names(d)); gridExtra::tableGrob(d, rows = NULL, theme = th)
  }
  ttl <- function(txt) grid::textGrob(txt, x = grid::unit(0.004, "npc"), hjust = 0, just = "left",
                        gp = grid::gpar(fontsize = 9, fontface = "bold", col = BEE_TABLE[["head"]]))
  scp <- grid::textGrob(paste(strwrap(cap_all, width = 130), collapse = "\n"),   # scope caption ABOVE the tables
                        x = grid::unit(0.004, "npc"), hjust = 0, just = "left",
                        gp = grid::gpar(fontsize = 7.5, fontface = "bold", col = BEE_INK$secondary, lineheight = 1.15))
  nr    <- c(nrow(part), nrow(bees_summary), nrow(methods_tbl), nrow(plants_summary))
  grobs <- list(scp,
                ttl("1. Participation"),                      mk(part, TRUE),
                ttl("2. Bees found"),                         mk(bees_summary, TRUE),
                ttl("3. Records by method x surveyor type"),  mk(methods_tbl),
                ttl("4. Plants found"),                       mk(plants_summary, TRUE))
  heights <- grid::unit.c(grid::unit(2.4, "lines"),
                          grid::unit(1.3, "lines"), grid::unit(nr[1], "null"),
                          grid::unit(1.3, "lines"), grid::unit(nr[2], "null"),
                          grid::unit(1.3, "lines"), grid::unit(nr[3], "null"),
                          grid::unit(1.3, "lines"), grid::unit(nr[4], "null"))
  G <- gridExtra::arrangeGrob(grobs = grobs, ncol = 1, heights = heights)
  bee_ggsave(file.path(OUT_DIR, "nps_summary_tables.png"), G,
                  width = 8.5, height = 0.30 * sum(nr) + 0.34 * 4 + 1.5, limitsize = FALSE, bg = "white")
} else message("  (gridExtra/ggplot2 not available -- skipped PNG; CSV + HTML written)")

message("NPS summary tables written to ", OUT_DIR, ":")
message(sprintf("  participation: %s surveys (%s), %s dedicated surveyors, %s public contributors",
                part$value[part$metric=="total_surveys"], part$value[part$metric=="years_covered"],
                part$value[part$metric=="dedicated_surveyors"], part$value[part$metric=="public_contributors"]))
message(sprintf("  bees: %s genera, %s species, %s subspecies (%s records)",
                bees_summary$value[bees_summary$metric=="genera_total"],
                bees_summary$value[bees_summary$metric=="species_total"],
                bees_summary$value[bees_summary$metric=="subspecies_total"],
                bees_summary$value[bees_summary$metric=="total_bee_records"]))
message(sprintf("  plants: %s genera, %s species",
                plants_summary$value[plants_summary$metric=="plant_genera_recorded"],
                plants_summary$value[plants_summary$metric=="plant_species_recorded"]))
