# =============================================================
# Bee Bounties -- actionable "go find this bee" lists from the method gaps
# beescabr / Cabrillo National Monument (CABR) native bees
#
# Two complementary gaps between the lethal (specimen) and non-lethal (iNaturalist)
# records, each turned into a targeted task list with where/when/on-what context:
#
#   * SPECIMEN BEE BOUNTY  (for netting / lethal surveyors)
#       taxa recorded in iNaturalist photos but with NO specimen -> the park needs a
#       VOUCHER specimen to conclusively confirm the ID. "We see it in photos; go net one."
#
#   * iNATURALIST BEE BOUNTY  (for photography surveyors)
#       taxa held as specimens but with NO species-level iNaturalist record -> get a
#       community-science PHOTO of it. "It's in the collection; go photograph it in the field."
#
# For every bounty taxon we attach the practical context from wherever it HAS been
# seen: peak months, top transects, top plant genera, and (for the specimen bounty)
# an example iNaturalist URL -- so a surveyor knows when/where/on-what to look.
#
# SCOPE: ALL records, both methods (a bounty is about finding the bee at all, not
# standardized effort). Ranks: species AND genus. Descriptive -- no p-value.
#
# Run from the repo root:  Rscript scripts/analysis/bee_bounties.R
# Depends on: dplyr, stringr, ggplot2 (+ config.R).
# =============================================================

for (pkg in c("ggplot2", "sf")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
}
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(ggplot2); library(sf) })

if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_METHOD_COL")) source("scripts/analysis/theme_beescabr.R")   # shared house style
OUT_DIR       <- "data/analysis/coverage/bee_bounties"
SPECIES_RANKS <- c("species", "subspecies")
GENUS_RANKS   <- c("species", "subspecies", "subgenus", "complex", "genus")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
scope_cap <- function(src) sprintf("Scope: ALL records  |  context from %s  |  ranks: species + genus", src)

# ---- 1. read + key both sources ---------------------------------------------
read_prep <- function(f) {
  d <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  data.frame(
    taxon_rank = str_squish(tolower(d$taxon_rank)),
    genus      = str_squish(d$genus),
    species    = d$species,
    month      = suppressWarnings(as.integer(substr(d$observed_on, 6, 7))),
    transect   = toupper(str_squish(d$transect)),
    plant      = if ("plant_genus" %in% names(d)) str_squish(d$plant_genus) else NA_character_,
    url        = if ("url" %in% names(d)) d$url else NA_character_,
    lat        = suppressWarnings(as.numeric(d$latitude)),
    lon        = suppressWarnings(as.numeric(d$longitude)),
    stringsAsFactors = FALSE) %>%
    mutate(species_key = ifelse(taxon_rank %in% SPECIES_RANKS & !is.na(genus) & genus != "" &
                                  !is.na(species) & species != "", paste(genus, word(species, -1)), NA),
           genus_key   = ifelse(taxon_rank %in% GENUS_RANKS & !is.na(genus) & genus != "", genus, NA))
}
spec <- read_prep(PATHS$specimen_clean)
inat <- read_prep(PATHS$inat_clean)

# ---- 2. context helpers ------------------------------------------------------
top2 <- function(x) { x <- x[!is.na(x) & x != ""]; if (!length(x)) return(NA_character_)
  tb <- sort(table(x), decreasing = TRUE); paste(names(tb)[seq_len(min(2, length(tb)))], collapse = ", ") }
peak_mo <- function(m) { m <- m[!is.na(m)]; if (!length(m)) return(NA_character_)
  tb <- sort(table(m), decreasing = TRUE); paste(month.abb[as.integer(names(tb)[seq_len(min(2, length(tb)))])], collapse = ", ") }

# summarise one source's records for a set of taxa, keyed by key_col
context_table <- function(src, key_col, taxa, want_url = FALSE) {
  d <- src[!is.na(src[[key_col]]) & src[[key_col]] %in% taxa, ]
  d %>% group_by(taxon = .data[[key_col]]) %>%
    summarise(n_records = n(),
              peak_months = peak_mo(month),
              top_transects = top2(transect),
              top_plants = top2(plant),
              example_url = if (want_url) { u <- url[!is.na(url) & url != ""]; if (length(u)) u[1] else NA_character_ } else NA_character_,
              .groups = "drop") %>%
    arrange(desc(n_records))
}

# ---- 3. build the two bounties at each rank ----------------------------------
build_bounty <- function(rank_label, key_col) {
  spec_taxa <- unique(na.omit(spec[[key_col]]))
  inat_taxa <- unique(na.omit(inat[[key_col]]))
  # Specimen bounty: in iNat, NOT in specimens -> collect a voucher (context from iNat)
  need_specimen <- setdiff(inat_taxa, spec_taxa)
  sb <- context_table(inat, key_col, need_specimen, want_url = TRUE) %>%
    mutate(rank = rank_label, bounty = "collect voucher specimen") %>%
    rename(n_photo_records = n_records)
  # iNat bounty: in specimens, NOT in iNat -> get a photo (context from specimens)
  need_photo <- setdiff(spec_taxa, inat_taxa)
  ib <- context_table(spec, key_col, need_photo, want_url = FALSE) %>%
    mutate(rank = rank_label, bounty = "get iNaturalist photo") %>%
    rename(n_specimen_records = n_records) %>% select(-example_url)
  list(specimen = sb, inat = ib)
}
sp <- build_bounty("species", "species_key")
gn <- build_bounty("genus",   "genus_key")

specimen_bounty <- bind_rows(sp$specimen, gn$specimen) %>%
  select(rank, taxon, n_photo_records, peak_months, top_transects, top_plants, example_url) %>%
  arrange(rank, desc(n_photo_records))
inat_bounty <- bind_rows(sp$inat, gn$inat) %>%
  select(rank, taxon, n_specimen_records, peak_months, top_transects, top_plants) %>%
  arrange(rank, desc(n_specimen_records))

write.csv(specimen_bounty, file.path(OUT_DIR, "specimen_bee_bounty.csv"), row.names = FALSE)
write.csv(inat_bounty,     file.path(OUT_DIR, "inaturalist_bee_bounty.csv"), row.names = FALSE)
message(sprintf("SPECIMEN BOUNTY (collect voucher): %d species + %d genera photographed but never collected",
                sum(specimen_bounty$rank == "species"), sum(specimen_bounty$rank == "genus")))
message(sprintf("iNAT BOUNTY (get a photo): %d species + %d genera in specimens but not on iNaturalist",
                sum(inat_bounty$rank == "species"), sum(inat_bounty$rank == "genus")))

# ---- 4. figures: top species targets, most 'findable' first ------------------
# EVERY gap species (no cap) -- a bounty must be the complete list of taxa missing from the other method.
bar <- function(df, ncol_records, title, sub, fill, file) {
  d <- df %>% filter(rank == "species") %>% arrange(desc(.data[[ncol_records]]))
  d$taxon <- factor(d$taxon, levels = rev(d$taxon))
  g <- ggplot(d, aes(x = .data[[ncol_records]], y = taxon)) +
    geom_col(fill = fill, width = 0.72) +
    geom_text(aes(label = .data[[ncol_records]]), hjust = -0.25, size = 3) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(title = title, subtitle = str_wrap(sprintf("%s  (all %d species)", sub, nrow(d)), 95),
         x = "records in the source method (more = easier to target)", y = NULL) +
    theme_beescabr(11) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y = element_text(face = "italic", colour = BEE_INK$muted), panel.grid.major.y = element_blank())
  ggsave(file, g, width = 9, height = max(6.4, 0.30 * nrow(d) + 1.8), dpi = 200, bg = "white")
}
bar(specimen_bounty, "n_photo_records",
    "Specimen Bee Bounty: Species to Collect",
    paste0("In iNaturalist photos but no specimen - net a voucher.  ", scope_cap("iNaturalist")),
    unname(BEE_METHOD_COL["lethal"]), file.path(OUT_DIR, "specimen_bee_bounty.png"))   # collect = net = purple
bar(inat_bounty, "n_specimen_records",
    "iNaturalist Bee Bounty: Species to Photograph",
    paste0("In specimens but not on iNaturalist - get a community photo.  ", scope_cap("specimens")),
    unname(BEE_METHOD_COL["nonlethal"]), file.path(OUT_DIR, "inaturalist_bee_bounty.png"))   # photograph = vermillion

message("Wrote specimen_bee_bounty.{csv,png} + inaturalist_bee_bounty.{csv,png} to ", OUT_DIR)

# ---- 5. two bounty maps -- each uses the spatial data that exists for its taxa ----
# Specimen bounty taxa ARE in iNaturalist -> real GPS points (where to net them).
# iNat bounty taxa exist ONLY as specimens, whose coords are transect centroids
# (980 records -> ~18 points), so we don't map fake points: we shade the transect
# CORRIDOR (its iNat trail buffered) = "walk this stretch to photograph it".
TR <- c("BST", "UPMON", "TP", "OT")
HALF_WIDTH_M <- 5                                    # corridor half-width (~10 m band, tight to the trail)
inat_geo <- inat %>% filter(!is.na(lat), !is.na(lon), transect %in% TR)
base_pts <- geom_point(data = inat_geo, aes(lon, lat), color = "grey70", size = 0.4, alpha = 0.2)
map_theme <- theme_beescabr(10) +
  theme(legend.position = "top")

# --- 5a. Specimen Bee Bounty: iNat sightings of the collect-targets ---
sb_sp <- specimen_bounty$taxon[specimen_bounty$rank == "species"]
sb_gn <- specimen_bounty$taxon[specimen_bounty$rank == "genus"]
sb_tgt <- inat_geo %>% filter(species_key %in% sb_sp | genus_key %in% sb_gn)
g1 <- ggplot() + base_pts +
  geom_point(data = sb_tgt, aes(lon, lat), color = unname(BEE_METHOD_COL["lethal"]), size = 1.3, alpha = 0.6) +   # net-targets = purple
  coord_quickmap() +
  labs(title = "Specimen Bee Bounty: Where to Net a Voucher",
       subtitle = str_wrap(sprintf("iNaturalist sightings of the %d taxa photographed but never collected (grey = all iNat effort)",
                                    length(sb_sp) + length(sb_gn)), 92),
       x = NULL, y = NULL) + map_theme + theme(plot.title = element_text(hjust = 0.5))
ggsave(file.path(OUT_DIR, "specimen_bee_bounty_map.png"), g1, width = 7.5, height = 8, dpi = 200, bg = "white")

# --- 5b. iNaturalist Bee Bounty: transect corridors for the photograph-targets ---
ib_sp <- inat_bounty$taxon[inat_bounty$rank == "species"]
ib_gn <- inat_bounty$taxon[inat_bounty$rank == "genus"]
ib_spec <- spec %>% filter(!is.na(lat), !is.na(lon), transect %in% TR,
                           species_key %in% ib_sp | genus_key %in% ib_gn)
tr_involved <- sort(unique(ib_spec$transect))
build_corridor <- function(d) {
  p <- st_transform(st_as_sf(d, coords = c("lon", "lat"), crs = 4326), 32611)
  st_sf(transect = d$transect[1],
        geometry = st_transform(st_sfc(st_union(st_buffer(p, HALF_WIDTH_M)), crs = 32611), 4326))
}
corr <- do.call(rbind, lapply(split(inat_geo[inat_geo$transect %in% tr_involved, ],
                                     inat_geo$transect[inat_geo$transect %in% tr_involved]), build_corridor))
sp_dots <- ib_spec %>% distinct(transect, lat, lon)
g2 <- ggplot() + base_pts +
  geom_sf(data = corr, aes(fill = transect), color = NA, alpha = 0.35) +
  geom_point(data = sp_dots, aes(lon, lat), color = BEE_INK$note, size = 2.2) +   # photograph-here attention marker
  scale_fill_manual(values = BEE_TRANSECT, name = "transect corridor") +
  coord_sf(expand = TRUE) +
  labs(title = "iNaturalist Bee Bounty: Where to Photograph",
       subtitle = str_wrap(sprintf("%d taxa held only as specimens; shaded band = that transect's walked trail (specimen coords are transect centroids, red dots)",
                                    length(ib_sp) + length(ib_gn)), 92),
       x = NULL, y = NULL) + map_theme + theme(plot.title = element_text(hjust = 0.5))
ggsave(file.path(OUT_DIR, "inaturalist_bee_bounty_map.png"), g2, width = 7.8, height = 8, dpi = 200, bg = "white")
message("Wrote specimen_bee_bounty_map.png + inaturalist_bee_bounty_map.png to ", OUT_DIR)
