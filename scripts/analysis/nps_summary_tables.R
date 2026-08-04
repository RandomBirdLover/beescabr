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
OUT_DIR       <- "data/analysis/reference/nps_summary"
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
