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

for (pkg in c("ggplot2", "sf", "leaflet", "htmlwidgets")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
}
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(ggplot2); library(sf) })

if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_METHOD_COL")) source("scripts/analysis/theme_beescabr.R")   # shared house style
OUT_DIR       <- file.path(DIR_REPORT, "coverage/bee_bounties")
SPECIES_RANKS <- c("species", "subspecies")
GENUS_RANKS   <- c("species", "subspecies", "subgenus", "complex", "genus")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
# scope_cap(): use the SHARED helper from theme_beescabr.R -- adds Source + data-as-of, one canonical order (no local override).

# ---- 1. read + key both sources ---------------------------------------------
read_prep <- function(f) {
  d <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  data.frame(
    taxon_rank = str_squish(tolower(d$taxon_rank)),
    family     = if ("family" %in% names(d)) str_squish(d$family) else NA_character_,
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
bar <- function(df, ncol_records, title, sub, fill, emoji, file) {
  d <- df %>% filter(rank == "species") %>% arrange(desc(.data[[ncol_records]]))
  d$taxon <- factor(d$taxon, levels = rev(d$taxon))
  # bar length + colour = the evidence the taxon ALREADY has (records + which method they came from);
  # the emoji in the TITLE is that bounty's single call-to-action = the method it still NEEDS.
  g <- ggplot(d, aes(x = .data[[ncol_records]], y = taxon)) +
    geom_col(fill = fill, width = 0.72) +
    geom_text(aes(label = .data[[ncol_records]]), hjust = -0.25, size = 3) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(title = paste0(title, "  ", emoji),
         subtitle = sub,   # the takeaway IS the subtitle -- the directional "do this" line
         caption = scope_cap(scope = "all records, whole park",
                             method = "lethal vs non-lethal",
                             rank = "species", n = nrow(d)),
         x = "records in the source method (more = easier to target)", y = NULL) +
    theme_beescabr(11) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y = element_text(face = "italic", colour = BEE_INK$muted), panel.grid.major.y = element_blank())
  bee_ggsave(file, g, width = 9, height = max(6.4, 0.30 * nrow(d) + 1.8), bg = "white")
}
bar(specimen_bounty, "n_photo_records",
    "Specimen Bee Bounty: Species to Collect",
    "In iNaturalist photos but no specimen - net a voucher.",
    unname(BEE_METHOD_COL["nonlethal"]), "\U0001F52C", file.path(OUT_DIR, "specimen_bee_bounty.png"))   # colour = the records it HAS (iNat photos = non-lethal periwinkle); \U0001F52C = the method it NEEDS (a specimen voucher)
bar(inat_bounty, "n_specimen_records",
    "iNaturalist Bee Bounty: Species to Photograph",
    "In specimens but not on iNaturalist - get a community photo.",
    unname(BEE_METHOD_COL["lethal"]), "\U0001F4F7", file.path(OUT_DIR, "inaturalist_bee_bounty.png"))   # colour = the records it HAS (specimens = lethal rose-red); \U0001F4F7 = the method it NEEDS (an iNaturalist photo)

message("Wrote specimen_bee_bounty.{csv,png} + inaturalist_bee_bounty.{csv,png} to ", OUT_DIR)

# ---- 5. two bounty maps -- each uses the spatial data that exists for its taxa ----
# Specimen bounty taxa ARE in iNaturalist -> real GPS points (where to net them).
# iNat bounty taxa exist ONLY as specimens, whose coords are transect centroids
# (980 records -> ~18 points), so we don't map fake points: we shade the transect
# CORRIDOR (its iNat trail buffered) = "walk this stretch to photograph it".
TR <- c("BST", "UPMON", "TP", "OT")
HALF_WIDTH_M <- 5                                    # corridor half-width (~10 m band, tight to the trail)
inat_geo <- inat %>% filter(!is.na(lat), !is.na(lon), transect %in% TR)
# These maps are now INTERACTIVE HTML (leaflet) -- the flat static scatter wasn't legible. Each opens
# with a Satellite/Street basemap you can toggle, every point/corridor has a click popup, and colours
# follow the bounty rule: collect-targets = non-lethal blue (iNat records we have); specimen locations
# = lethal red (specimen records we have). Saved as a SINGLE self-contained .html (like every other
# HTML in this project) when pandoc is available; falls back to a lib/ folder only if pandoc is missing.

# --- 5a. Specimen Bee Bounty: iNat sightings of the collect-targets (blue) ---
sb_sp <- specimen_bounty$taxon[specimen_bounty$rank == "species"]
sb_gn <- specimen_bounty$taxon[specimen_bounty$rank == "genus"]
sb_tgt <- inat_geo %>% filter(species_key %in% sb_sp | genus_key %in% sb_gn) %>%
  mutate(taxon = ifelse(!is.na(species_key) & species_key != "", species_key, genus_key),
         popup = sprintf("<b><i>%s</i></b><br>peak month: %s<br>flower: %s%s",
                         ifelse(is.na(taxon), "bee", taxon),
                         ifelse(is.na(month), "?", month.abb[month]),
                         ifelse(is.na(plant) | plant == "", "—", plant),
                         ifelse(is.na(url) | url == "", "",
                                sprintf('<br><a href="%s" target="_blank">iNaturalist observation ↗</a>', url))))
# colour every collect-target by its FAMILY hue (BEE_FAMILY -- distinct hues), then within a family
# spread the taxa across a light->dark ramp of that hue ORDERED by genus,species -- so each genus is a
# contiguous shade band and species within a genus are adjacent steps. Legend is keyed to family; the
# popup names the exact species. (Anchoring on family keeps the busy family, Apidae, from colliding.)
fam_of <- function(f) { f <- str_squish(f); f[is.na(f) | f == ""] <- "Other"
  ifelse(f %in% names(BEE_FAMILY), f, "Other") }
sb_tgt$fam <- fam_of(sb_tgt$family)
shade_ramp <- function(base, k) {                       # k variations of one hue: light tint -> base -> dark shade
  if (k <= 1) return(base)                              # range WIDENED for contrast (Apidae has ~10 genera)
  lo <- grDevices::rgb(t(255 - (255 - grDevices::col2rgb(base)) * 0.40), maxColorValue = 255)  # lighter tint
  hi <- grDevices::rgb(t(grDevices::col2rgb(base) * 0.48), maxColorValue = 255)                 # darker shade
  grDevices::colorRampPalette(c(lo, base, hi))(k)
}
taxo <- sb_tgt %>% distinct(fam, genus, taxon) %>% arrange(fam, genus, taxon)
taxo$tcol <- NA_character_
for (fm in unique(taxo$fam)) {                          # one hue per family; taxa (genus-then-species order) span its ramp
  idx <- which(taxo$fam == fm)
  taxo$tcol[idx] <- shade_ramp(unname(BEE_FAMILY[fm]), length(idx))
}
tax_col     <- setNames(taxo$tcol, taxo$taxon)
sb_tgt$gcol <- ifelse(sb_tgt$taxon %in% names(tax_col), unname(tax_col[sb_tgt$taxon]), BEE_GENUS_GREY)
fam_present <- intersect(BEE_FAMILY_ORDER, unique(taxo$fam))   # families for the family legend, canonical order
# SECOND legend (left side): genera parsed out UNDER their family, each swatch a shade (variation) of
# the family hue -- so you can read a specific dot's genus. Representative colour = the genus band's midpoint.
genus_leg <- taxo %>% arrange(match(fam, BEE_FAMILY_ORDER), genus) %>%
  group_by(fam, genus) %>% summarise(col = tcol[ceiling(dplyr::n() / 2)], .groups = "drop")
.dot <- function(col) sprintf('<span style="display:inline-block;width:10px;height:10px;border-radius:50%%;background:%s;margin-right:5px;vertical-align:middle"></span>', col)
genus_html <- paste0(
  '<div style="background:rgba(255,255,255,0.92);padding:6px 9px;border-radius:5px;font:12px -apple-system,sans-serif;max-height:74vh;overflow:auto;box-shadow:0 1px 4px rgba(0,0,0,.3)">',
  '<div style="font-weight:700">collect-target genus \U0001F52C</div>',
  '<div style="font-weight:400;font-size:11px;color:#555;margin-bottom:3px">net a voucher \U00B7 species in popup</div>',
  paste(vapply(fam_present, function(fm) {
    gg <- genus_leg[genus_leg$fam == fm, ]
    paste0(sprintf('<div style="font-weight:700;color:%s;margin-top:4px">%s</div>', unname(BEE_FAMILY[fm]), fm),
           paste(sprintf('<div style="margin-left:4px">%s<i>%s</i></div>', .dot(gg$col), gg$genus), collapse = ""))
  }, character(1)), collapse = ""),
  '</div>')

# --- 5b. iNaturalist Bee Bounty: photo-targets at their transect + the trail to walk ---
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
# per-transect list of photo-targets, for the trail popups
ib_taxa <- ib_spec %>%
  mutate(taxon = ifelse(!is.na(species_key) & species_key != "", species_key, genus_key)) %>%
  filter(!is.na(taxon)) %>% distinct(transect, taxon) %>%
  group_by(transect) %>% summarise(n = dplyr::n(), taxa = paste(sort(unique(taxon)), collapse = ", "), .groups = "drop")
corr <- dplyr::left_join(corr, ib_taxa, by = "transect")
corr$popup <- sprintf("<b>%s trail</b><br>walk this stretch \U00B7 %d taxa to photograph:<br><i>%s</i>",
                      corr$transect, ifelse(is.na(corr$n), 0L, corr$n),
                      ifelse(is.na(corr$taxa), "\U2014", corr$taxa))
# IMPORTANT: specimen coords are transect CENTROIDS -- there is no real per-bee GPS. So we do NOT scatter
# fake points; we mark each TRANSECT once (at its centroid) and its popup lists that transect's photo-
# targets, colour-coded by family->genus. The map answers WHERE (which transect to walk); the popup +
# side legend answer WHAT (the taxa). Colour describes the taxa (a catalogue), never a precise location.
ib_tx <- ib_spec %>%
  mutate(taxon = ifelse(!is.na(species_key) & species_key != "", species_key, genus_key),
         fam   = fam_of(family)) %>%
  filter(!is.na(taxon)) %>% distinct(transect, taxon, fam, genus)
ib_taxo <- ib_tx %>% distinct(fam, genus, taxon) %>% arrange(match(fam, BEE_FAMILY_ORDER), genus, taxon)
ib_taxo$tcol <- NA_character_
for (fm in unique(ib_taxo$fam)) { idx <- which(ib_taxo$fam == fm)
  ib_taxo$tcol[idx] <- shade_ramp(unname(BEE_FAMILY[fm]), length(idx)) }
ib_colmap <- setNames(ib_taxo$tcol, ib_taxo$taxon)
ib_tx$col <- ifelse(ib_tx$taxon %in% names(ib_colmap), unname(ib_colmap[ib_tx$taxon]), BEE_GENUS_GREY)
ib_tx <- ib_tx %>% arrange(transect, match(fam, BEE_FAMILY_ORDER), genus, taxon)
# one labelled marker per transect at its centroid; popup = that transect's colour-coded target list
tr_list <- ib_tx %>% group_by(transect) %>%
  summarise(nt = dplyr::n_distinct(taxon),
            lst = paste(sprintf('<div style="margin:1px 0">%s<i>%s</i></div>', .dot(col), taxon), collapse = ""),
            .groups = "drop")
ib_cent <- ib_spec %>% group_by(transect) %>%
  summarise(lat = stats::median(lat, na.rm = TRUE), lon = stats::median(lon, na.rm = TRUE), .groups = "drop") %>%
  dplyr::left_join(tr_list, by = "transect")
ib_cent$popup <- sprintf('<b>%s trail</b> \U2014 %d taxa to photograph<br><span style="font-size:11px;color:#555">specimen-only: no exact GPS, so walk the whole transect</span><br>%s',
                         ib_cent$transect, ib_cent$nt, ib_cent$lst)
ib_fam_present <- intersect(BEE_FAMILY_ORDER, unique(ib_taxo$fam))
ib_glg <- ib_taxo %>% group_by(fam, genus) %>% summarise(col = tcol[ceiling(dplyr::n() / 2)], .groups = "drop") %>%
  arrange(match(fam, BEE_FAMILY_ORDER), genus)
ib_genus_html <- paste0(
  '<div style="background:rgba(255,255,255,0.92);padding:6px 9px;border-radius:5px;font:12px -apple-system,sans-serif;max-height:74vh;overflow:auto;box-shadow:0 1px 4px rgba(0,0,0,.3)">',
  '<div style="font-weight:700">photograph-target genus \U0001F4F7</div>',
  '<div style="font-weight:400;font-size:11px;color:#555;margin-bottom:3px">get a photo \U00B7 walk the transect it is found on (click a marker)</div>',
  paste(vapply(ib_fam_present, function(fm) { gg <- ib_glg[ib_glg$fam == fm, ]
    paste0(sprintf('<div style="font-weight:700;color:%s;margin-top:4px">%s</div>', unname(BEE_FAMILY[fm]), fm),
           paste(sprintf('<div style="margin-left:4px">%s<i>%s</i></div>', .dot(gg$col), gg$genus), collapse = ""))
  }, character(1)), collapse = ""),
  '</div>')

# ---- build + save the two interactive maps (shared lib/ dir) ----
TILE_SAT <- "Esri.WorldImagery"; TILE_STR <- "CartoDB.Positron"
m1 <- leaflet::leaflet() %>%
  leaflet::addProviderTiles(TILE_SAT, group = "Satellite") %>%
  leaflet::addProviderTiles(TILE_STR, group = "Street") %>%
  leaflet::addCircleMarkers(data = inat_geo, lng = ~lon, lat = ~lat, radius = 2,
      color = BEE_INK[["muted"]], stroke = FALSE, fillOpacity = 0.2, group = "all iNat effort") %>%
  leaflet::addCircleMarkers(data = sb_tgt, lng = ~lon, lat = ~lat, radius = 5, color = "white",
      weight = 1, fillColor = ~gcol, fillOpacity = 0.9, popup = ~popup,
      group = "collect-targets") %>%
  leaflet::addControl(html = genus_html, position = "bottomleft") %>%
  leaflet::addLayersControl(baseGroups = c("Satellite", "Street"),
      overlayGroups = c("all iNat effort", "collect-targets"),
      options = leaflet::layersControlOptions(collapsed = FALSE))
m2 <- leaflet::leaflet() %>%
  leaflet::addProviderTiles(TILE_SAT, group = "Satellite") %>%
  leaflet::addProviderTiles(TILE_STR, group = "Street") %>%
  leaflet::addCircleMarkers(data = inat_geo, lng = ~lon, lat = ~lat, radius = 2,
      color = BEE_INK[["muted"]], stroke = FALSE, fillOpacity = 0.2, group = "all iNat effort") %>%
  leaflet::addPolygons(data = corr, fillColor = "#8A8880", color = "#8A8880", weight = 1, fillOpacity = 0.35,
      popup = ~popup, group = "transect trails") %>%
  leaflet::addCircleMarkers(data = ib_cent, lng = ~lon, lat = ~lat, radius = 9, color = "white",
      weight = 2, fillColor = BEE_INK[["primary"]], fillOpacity = 0.85, popup = ~popup,
      label = ~transect, labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "top",
              style = list("font-weight" = "700", "background" = "rgba(255,255,255,0.85)", "padding" = "1px 5px")),
      group = "transects (click for targets)") %>%
  leaflet::addControl(html = ib_genus_html, position = "bottomleft") %>%
  leaflet::addLayersControl(baseGroups = c("Satellite", "Street"),
      overlayGroups = c("all iNat effort", "transect trails", "transects (click for targets)"),
      options = leaflet::layersControlOptions(collapsed = FALSE))
# self-contained single file when pandoc is available (matches every other HTML in the project);
# fall back to a lib/ folder only if pandoc is missing, so the pipeline never hard-fails.
.sc <- requireNamespace("rmarkdown", quietly = TRUE) && isTRUE(try(rmarkdown::pandoc_available(), silent = TRUE))
.libd <- if (.sc) NULL else "lib"    # self-contained -> no external lib dir; fallback -> a shared lib/ folder
htmlwidgets::saveWidget(m1, file.path(normalizePath(OUT_DIR), "specimen_bee_bounty_map.html"),
                        selfcontained = .sc, libdir = .libd, title = "Specimen Bee Bounty -- Where to Net a Voucher")
htmlwidgets::saveWidget(m2, file.path(normalizePath(OUT_DIR), "inaturalist_bee_bounty_map.html"),
                        selfcontained = .sc, libdir = .libd, title = "iNaturalist Bee Bounty -- Where to Photograph")
if (.sc) unlink(c(file.path(normalizePath(OUT_DIR), "lib"),                                   # tidy: self-contained html inlines everything, so
                  Sys.glob(file.path(normalizePath(OUT_DIR), "*_map_files"))), recursive = TRUE) # the lib/ + *_map_files dep dirs are just leftovers
message(sprintf("Wrote specimen_bee_bounty_map.html + inaturalist_bee_bounty_map.html (%s) to %s",
                if (.sc) "self-contained single files" else "interactive; shared lib/ folder", OUT_DIR))
