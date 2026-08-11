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
OUT_DIR       <- file.path(DIR_REPORT, "reference/nps_summary")
SPECIES_RANKS <- c("species", "subspecies")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
kv <- function(...) { x <- list(...); data.frame(metric = names(x), value = unlist(x), row.names = NULL) }
tok <- function(v) { t <- unlist(strsplit(as.character(v), "[,;/&]")); t <- str_squish(t); unique(t[t != "" & !is.na(t)]) }

spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
psf  <- read.csv(if (!is.null(PATHS$per_survey)) PATHS$per_survey else
                 "data/project_info/master_per_survey_info.csv", stringsAsFactors = FALSE, check.names = FALSE)

# ---- 1. participation --------------------------------------------------------
psf$year_i <- suppressWarnings(as.integer(psf$year))
role <- str_squish(tolower(psf$role))
part <- kv(
  survey_trips              = nrow(psf),
  survey_days               = sum(suppressWarnings(as.numeric(psf$n_days)), na.rm = TRUE),
  years_covered             = paste(range(psf$year_i, na.rm = TRUE), collapse = "-"),
  n_years                   = length(unique(na.omit(psf$year_i))),
  transects                 = length(tok(psf$transects)),
  unique_field_surveyors    = length(tok(psf$surveyors)),
  unique_inaturalist_users  = length(tok(psf$inat_username)),
  intern_trips              = sum(role == "intern", na.rm = TRUE),
  beeple_trips              = sum(role == "beeple", na.rm = TRUE))
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
gkey <- function(d) unique(na.omit(d$genus[d$genus != ""]))
skey <- function(d) unique(na.omit(d$species_key))
bees_summary <- kv(
  total_bee_records          = nrow(bees),
  records_lethal             = sum(bees$method == "lethal (specimen)"),
  records_nonlethal          = sum(bees$method == "non-lethal (iNaturalist)"),
  genera_total               = length(gkey(bees)),
  species_total              = length(skey(bees)),
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
  arrange(genus, species)
write.csv(checklist, file.path(OUT_DIR, "nps_bee_species_checklist.csv"), row.names = FALSE)

# genera checklist (includes genus-only records)
gen_checklist <- bees %>% filter(!is.na(genus), genus != "") %>%
  group_by(genus) %>% summarise(n_records = n(),
            n_species_resolved = n_distinct(species_key[!is.na(species_key)]), .groups = "drop") %>%
  arrange(genus)
write.csv(gen_checklist, file.path(OUT_DIR, "nps_bee_genera_checklist.csv"), row.names = FALSE)

# ---- 3. methods x surveyor type ---------------------------------------------
styp <- function(df, method) data.frame(method = method,
  surveyor = ifelse(is.na(df$surveyor_type) | str_squish(df$surveyor_type) == "",
                    "unattributed", str_squish(tolower(df$surveyor_type))), stringsAsFactors = FALSE)
ms <- bind_rows(styp(spec, "lethal (specimen)"), styp(inat, "non-lethal (iNaturalist)"))
methods_tbl <- as.data.frame.matrix(table(ms$method, ms$surveyor))
methods_tbl <- cbind(method = rownames(methods_tbl), methods_tbl, total = rowSums(methods_tbl))
write.csv(methods_tbl, file.path(OUT_DIR, "nps_methods.csv"), row.names = FALSE)

# ---- 4. plants found ---------------------------------------------------------
plants <- read.csv(PATHS$inat_plant_clean, stringsAsFactors = FALSE, check.names = FALSE)
pg <- str_squish(plants$plant_genus); ps <- str_squish(plants$plant_species)
plants_summary <- kv(
  plant_records              = nrow(plants),
  plant_genera_recorded      = length(unique(pg[pg != "" & !is.na(pg)])),
  plant_species_recorded     = length(unique(ps[ps != "" & !is.na(ps)])))
write.csv(plants_summary, file.path(OUT_DIR, "nps_plants_summary.csv"), row.names = FALSE)
plant_checklist <- plants %>% mutate(plant_genus = pg, plant_species = ps) %>%
  filter(plant_genus != "", !is.na(plant_genus)) %>%
  group_by(plant_genus) %>% summarise(n_records = n(),
            n_species = n_distinct(plant_species[plant_species != "" & !is.na(plant_species)]),
            .groups = "drop") %>% arrange(plant_genus)
write.csv(plant_checklist, file.path(OUT_DIR, "nps_plant_genera_checklist.csv"), row.names = FALSE)

# ============================================================================
# 5. RENDERED TABLES (HTML + PNG) -- same styling family as the field guides, with a
#    scope caption (Scope | Method | Rank | Source) ABOVE each table. The CSVs above
#    stay the machine-readable source; these are the read-and-cite version.
# ============================================================================
if (!exists("scope_cap")) source("scripts/analysis/theme_beescabr.R")   # shared scope-caption format
esc <- function(x) { x <- gsub("&", "&amp;", x); x <- gsub("<", "&lt;", x); gsub(">", "&gt;", x) }
pretty_metric <- function(m) tools::toTitleCase(gsub("_", " ", m))       # metric_key -> "Metric Key"

# one scope caption per table set
cap_part   <- scope_cap(scope = "all survey trips logged; all years, whole park",
                        method = "field surveys (interns) + iNaturalist (beeple)",
                        rank = "people / trips / days / transects", width = 10000)
cap_bees   <- scope_cap(scope = "every bee record; all years, whole park",
                        method = "lethal (specimen) + non-lethal (iNaturalist), pooled and split",
                        rank = "records / genera / species", width = 10000)
cap_meth   <- scope_cap(scope = "every bee record, by method x surveyor type; all years, whole park",
                        method = "lethal (specimen) vs non-lethal (iNaturalist)",
                        rank = "records", width = 10000)
cap_plants <- scope_cap(scope = "every plant record a bee was seen on; all years, whole park",
                        method = "non-lethal iNaturalist plant associations",
                        rank = "records / genera / species", width = 10000)
cap_chk    <- scope_cap(scope = "species-resolved bee checklist; all years, whole park",
                        method = "lethal (specimen) + non-lethal (iNaturalist)",
                        rank = "species", width = 10000)
cap_gchk   <- scope_cap(scope = "bee genera checklist (includes genus-only records); all years, whole park",
                        method = "lethal (specimen) + non-lethal (iNaturalist)",
                        rank = "genus", width = 10000)
cap_pchk   <- scope_cap(scope = "plant genera bees were recorded on; all years, whole park",
                        method = "non-lethal iNaturalist plant associations",
                        rank = "plant genus", width = 10000)

# ---- HTML ------------------------------------------------------------------
df_to_html <- function(df, caption, heading, metric_col = FALSE) {
  d <- df
  if (metric_col) d[[1]] <- pretty_metric(d[[1]])
  hd <- paste0("<th>", vapply(names(d), function(nm) esc(gsub("_", " ", nm)), ""), "</th>", collapse = "")
  body <- vapply(seq_len(nrow(d)), function(i) {
    cells <- vapply(seq_along(d), function(j) {
      v <- d[[j]][i]; num <- is.numeric(d[[j]])
      val <- if (num) format(v, big.mark = ",", trim = TRUE) else esc(as.character(v))
      sprintf('<td%s>%s</td>', if (num) ' class="num"' else "", val)
    }, "")
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  }, "")
  paste0('<h2>', esc(heading), '</h2>',
         '<p class="scope">', esc(caption), '</p>',
         '<table class="t"><thead><tr>', hd, '</tr></thead><tbody>',
         paste(body, collapse = ""), '</tbody></table>')
}

html <- paste0(
'<!doctype html><html><head><meta charset="utf-8"><title>CABR NPS Summary Tables</title><style>',
'body{font:14px/1.45 -apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;color:#1a1a1a;margin:24px;background:#fcfcfb;max-width:900px}',
'h1{font-size:20px;margin:0 0 2px}h2{font-size:15px;margin:24px 0 4px}',
'p.sub{color:#6b6a66;margin:0 0 14px;font-size:13px}',
'p.scope{color:#52514e;margin:0 0 8px;font-size:12px;font-weight:600;border-left:3px solid #d8d5cc;padding-left:8px}',
'table.t{border-collapse:collapse;width:100%;font-size:13px;margin-bottom:6px}',
'table.t th,table.t td{text-align:left;padding:6px 10px;border-bottom:1px solid #eee}',
'table.t th{background:#f3f1ec;font-weight:600;border-bottom:2px solid #ddd;white-space:nowrap}',
'table.t td.num{text-align:right;font-variant-numeric:tabular-nums}',
'</style></head><body>',
'<h1>CABR native bees &mdash; NPS summary tables</h1>',
'<p class="sub">Plain counts for the data-focused NPS report &mdash; deliberately no tests, rates, or interpretation. Every number here is a straight count a reader can cite.</p>',
df_to_html(part,            cap_part,   "1. Participation", metric_col = TRUE),
df_to_html(bees_summary,    cap_bees,   "2. Bees found",    metric_col = TRUE),
df_to_html(methods_tbl,     cap_meth,   "3. Records by method x surveyor type"),
df_to_html(plants_summary,  cap_plants, "4. Plants found",  metric_col = TRUE),
df_to_html(checklist,       cap_chk,    "Bee species checklist"),
df_to_html(gen_checklist,   cap_gchk,   "Bee genera checklist"),
df_to_html(plant_checklist, cap_pchk,   "Plant genera checklist"),
'</body></html>')
writeLines(html, file.path(OUT_DIR, "nps_summary_tables.html"))

# ---- PNG (the four compact summary tables; long checklists stay CSV/HTML) ----
if (requireNamespace("gridExtra", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE)) {
  cap_all <- scope_cap(scope = "whole dataset: every record, all years, whole park (plain counts, no test or interpretation)",
                       method = "lethal (specimen) + non-lethal (iNaturalist)",
                       rank = "records, genera & species", width = 10000)
  th <- gridExtra::ttheme_minimal(base_size = 8,
    core = list(fg_params = list(hjust = 0, x = 0.02), bg_params = list(fill = c("#ffffff", "#f6f5f2"))),
    colhead = list(fg_params = list(hjust = 0, x = 0.02, fontface = "bold")))
  mk  <- function(df, metric_col = FALSE) {
    d <- df; if (metric_col) d[[1]] <- pretty_metric(d[[1]])
    names(d) <- gsub("_", " ", names(d)); gridExtra::tableGrob(d, rows = NULL, theme = th)
  }
  ttl <- function(txt) grid::textGrob(txt, x = grid::unit(0.004, "npc"), hjust = 0, just = "left",
                        gp = grid::gpar(fontsize = 9, fontface = "bold", col = "#1a1a1a"))
  scp <- grid::textGrob(paste(strwrap(cap_all, width = 130), collapse = "\n"),   # scope caption ABOVE the tables
                        x = grid::unit(0.004, "npc"), hjust = 0, just = "left",
                        gp = grid::gpar(fontsize = 7.5, fontface = "bold", col = "#52514e", lineheight = 1.15))
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
  ggplot2::ggsave(file.path(OUT_DIR, "nps_summary_tables.png"), G,
                  width = 8.5, height = 0.30 * sum(nr) + 0.34 * 4 + 1.5, dpi = 200, limitsize = FALSE, bg = "white")
} else message("  (gridExtra/ggplot2 not available -- skipped PNG; CSV + HTML written)")

message("NPS summary tables written to ", OUT_DIR, ":")
message(sprintf("  participation: %s trips, %s field surveyors, %s iNat users, %s",
                part$value[part$metric=="survey_trips"], part$value[part$metric=="unique_field_surveyors"],
                part$value[part$metric=="unique_inaturalist_users"], part$value[part$metric=="years_covered"]))
message(sprintf("  bees: %s genera, %s species (%s records)",
                bees_summary$value[bees_summary$metric=="genera_total"],
                bees_summary$value[bees_summary$metric=="species_total"],
                bees_summary$value[bees_summary$metric=="total_bee_records"]))
message(sprintf("  plants: %s genera, %s species",
                plants_summary$value[plants_summary$metric=="plant_genera_recorded"],
                plants_summary$value[plants_summary$metric=="plant_species_recorded"]))
