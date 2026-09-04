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

# Dependencies are CHECKED here, not installed: see beescabr_require() in config.R.
if (!exists("beescabr_require")) source("scripts/config.R")
beescabr_require()
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(ggplot2); library(sf) })
# interactive pages live in a website/ subfolder beside the figures they came from
.web <- function(d) { p <- file.path(d, "website"); dir.create(p, recursive = TRUE, showWarnings = FALSE); p }

if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_METHOD_COL")) source("scripts/analysis/theme_beescabr.R")   # shared house style
if (!exists("inat_photo_link")) source("scripts/analysis/inat_taxon_links.R") # iNat logo -> taxon photo page
OUT_DIR       <- file.path(DIR_REPORT, "coverage/bee_bounties")
# quote-free aliases: the popup/JS strings below are double-quoted, so a BEE_HTML[["x"]]
# subscript cannot be spliced into them directly. The palette is still the only source.
.DEEP <- BEE_HTML_GREEN[["deep"]]; .SUB <- BEE_HTML[["sub"]]; .CN <- BEE_HTML[["cn"]]
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
  xlab <- if (grepl("specimen", ncol_records)) "specimen records" else "iNaturalist records"   # bar length = the evidence the taxon already has (from the OTHER method)
  d <- df %>% filter(rank == "species") %>% arrange(desc(.data[[ncol_records]]))
  d$taxon <- factor(d$taxon, levels = rev(d$taxon))
  # bar length + color = the evidence the taxon ALREADY has (records + which method they came from);
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
         x = xlab, y = NULL) +
    theme_beescabr(11) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.y = element_text(face = "italic", colour = BEE_INK$muted), panel.grid.major.y = element_blank())
  bee_ggsave(file, g, width = 9, height = max(6.4, 0.30 * nrow(d) + 1.8), bg = "white")
}
bar(specimen_bounty, "n_photo_records",
    "Specimen Bee Bounty: Species to Collect",
    "In iNaturalist photos but no specimen - net a voucher.",
    unname(BEE_METHOD_COL["nonlethal"]), "\U0001F52C", file.path(OUT_DIR, "specimen_bee_bounty.png"))   # color = the records it HAS (iNat photos = non-lethal periwinkle); \U0001F52C = the method it NEEDS (a specimen voucher)
bar(inat_bounty, "n_specimen_records",
    "iNaturalist Bee Bounty: Species to Photograph",
    "In specimens but not on iNaturalist - get a community photo.",
    unname(BEE_METHOD_COL["lethal"]), "\U0001F4F7", file.path(OUT_DIR, "inaturalist_bee_bounty.png"))   # color = the records it HAS (specimens = lethal rose-red); \U0001F4F7 = the method it NEEDS (an iNaturalist photo)

message("Wrote specimen_bee_bounty.{csv,png} + inaturalist_bee_bounty.{csv,png} to ", OUT_DIR)

# ---- 5. two bounty maps -- each uses the spatial data that exists for its taxa ----
# Specimen bounty taxa ARE in iNaturalist -> real GPS points (where to net them).
# iNat bounty taxa exist ONLY as specimens, whose coords are transect centroids, so we don't map fake
# points: we draw the real transect LINES from the GIS shapefile and list each one's photo-targets.
TR <- c("BST", "UPMON", "TP", "OT")
inat_geo <- inat %>% filter(!is.na(lat), !is.na(lon), transect %in% TR)

# --- per-species popup detail (both maps): month WINDOW + one flower field -- the SELECTIVE favorite
#     where the availability test can run (>= 50 flower-visit records), else the MOST-RECORDED plant. ---
if (!exists("selectivity_table_species")) source("scripts/analysis/forage_selectivity.R")
.sel_fav <- { st <- selectivity_table_species()          # taxon -> selective favorite plant (NA if none)
  setNames(ifelse(st$selective & !is.na(st$preferred_plant), st$preferred_plant, NA_character_), st$taxon) }
.detail_tbl <- function(src, taxa) {                       # window (month range) + top plant per taxon
  src$tkey <- ifelse(!is.na(src$species_key) & src$species_key != "", src$species_key, src$genus_key)
  src <- src[!is.na(src$tkey) & src$tkey %in% taxa, ]
  src %>% group_by(taxon = tkey) %>% summarise(
    win = { m <- month[!is.na(month)]; if (!length(m)) "\U2014" else { r <- range(m)
            if (r[1] == r[2]) month.abb[r[1]] else paste0(month.abb[r[1]], "\U2013", month.abb[r[2]]) } },
    top = { p <- plant[!is.na(plant) & plant != ""]; if (!length(p)) NA_character_ else names(sort(table(p), decreasing = TRUE))[1] },
    .groups = "drop") }
.detail_html <- function(taxon, win, top) {                # muted "window: … · favorite flower / most recorded on: …"
  fav <- unname(.sel_fav[taxon])
  flower <- if (!is.na(fav)) sprintf("favorite flower: <i>%s</i>", fav)
            else if (!is.na(top)) sprintf("most recorded on: <i>%s</i>", top) else NA_character_
  bits <- c(sprintf("window: %s", if (is.na(win)) "\U2014" else win), flower); bits <- bits[!is.na(bits)]
  sprintf('<span style="color:%s;font-weight:400">%s</span>', BEE_HTML[["sub"]], paste(bits, collapse = " \U00B7 ")) }
# These maps are INTERACTIVE HTML (leaflet): a Satellite/Street basemap you can toggle, the NPS park
# boundary outlined, every point/line has a click popup, colors from the house palette. Saved as a
# SINGLE self-contained .html when pandoc is available; falls back to a lib/ folder if pandoc is missing.

# --- 5a. Specimen Bee Bounty: iNat sightings of the collect-targets (blue) ---
sb_sp <- specimen_bounty$taxon[specimen_bounty$rank == "species"]
sb_gn <- specimen_bounty$taxon[specimen_bounty$rank == "genus"]
sb_tgt <- inat_geo %>% filter(species_key %in% sb_sp | genus_key %in% sb_gn) %>%
  mutate(taxon = ifelse(!is.na(species_key) & species_key != "", species_key, genus_key))
sb_tgt <- dplyr::left_join(sb_tgt, .detail_tbl(inat, unique(sb_tgt$taxon)), by = "taxon")   # aggregate window + flower
# iNat taxon ids for the logo link in every popup (a lookup miss -> name search, never dead)
.lk <- read.csv(PATHS$taxonomy_lookup, stringsAsFactors = FALSE)
.gid <- .lk[tolower(str_squish(.lk$rank)) == "genus", c("genus", "taxon_id")]
.lk_nm <- str_squish(.lk$scientific_name); .lk_nm[is.na(.lk_nm) | .lk_nm == ""] <- NA_character_
# A bounty row is either "Genus species" or a bare genus. Resolve against the lookup's
# STRUCTURED columns, not its display name -- matching scientific_name used to miss any
# genus the checklist spells differently (Biastes, Trachusa).
.tid_of <- function(taxon) {
  # Blank out empty names on BOTH sides: the lookup carries 56 rows with no
  # scientific_name, and match("", ...) / match(NA, ...) would silently land on one.
  tx <- str_squish(taxon); tx[is.na(tx) | tx == ""] <- NA_character_
  id <- .lk$taxon_id[match(tx, .lk_nm)]
  ifelse(is.na(id), .gid$taxon_id[match(tx, .gid$genus)], id)
}
sb_tgt$popup <- mapply(function(taxon, win, top, url)
  sprintf('<b><i>%s</i></b>%s<br>%s%s', ifelse(is.na(taxon), "bee", taxon),
          if (is.na(taxon)) "" else inat_photo_link(.tid_of(taxon), taxon),
          .detail_html(taxon, win, top),
          if (is.na(url) || url == "") "" else sprintf('<br><a href="%s" target="_blank">iNaturalist observation \U2197</a>', url)),
  sb_tgt$taxon, sb_tgt$win, sb_tgt$top, sb_tgt$url)
# color every collect-target by its FAMILY hue (BEE_FAMILY -- distinct hues), then within a family
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
# the family hue -- so you can read a specific dot's genus. Representative color = the genus band's midpoint.
genus_leg <- taxo %>% arrange(match(fam, BEE_FAMILY_ORDER), genus) %>%
  group_by(fam, genus) %>% summarise(col = tcol[ceiling(dplyr::n() / 2)], .groups = "drop")
.dot <- function(col) sprintf('<span style="display:inline-block;width:11px;height:11px;border-radius:50%%;background:%s;margin-right:7px;vertical-align:middle;box-shadow:0 0 0 1px rgba(0,0,0,.14)"></span>', col)
# shared white-card chrome for the map title + side legends -- matches the report tables' look
.CARD <- sprintf("background:%s;border:1px solid %s;border-radius:12px;box-shadow:0 4px 20px rgba(20,20,20,.14);font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,sans-serif;color:%s",
                 BEE_HTML[["page"]], BEE_HTML[["border"]], BEE_HTML[["ink"]])
# a genus legend grouped under its family: NPS eyebrow, teal heading, family rows with a colored
# accent bar (its hue) + dark name, genera indented + italic with their color dot.
# Composable legend pieces (the map's title/instructions live in the title card, NOT here):
#   .col_title  -- a short uppercase section label for a legend column
#   .genus_block -- FAMILY (uppercase roman header + hue accent bar) > GENUS (indented italic + dot)
#   .tran_block  -- transect rows, each a short line swatch in its transect hue
#   .legend_wrap -- the shared white card around whatever columns get passed
.col_title   <- function(t) sprintf('<div style="font-weight:700;font-size:10.5px;text-transform:uppercase;letter-spacing:.05em;color:%s;margin:0 0 5px">%s</div>', BEE_HTML_GREEN[["deep"]], t)
.genus_block <- function(fams, glg) paste(vapply(fams, function(fm) { gg <- glg[glg$fam == fm, ]
    paste0(sprintf('<div style="font-weight:700;font-size:10.5px;text-transform:uppercase;letter-spacing:.06em;color:%s;margin:8px 0 3px;padding-left:7px;border-left:3px solid %s">%s</div>', BEE_HTML_GREEN[["deep"]], unname(BEE_FAMILY[fm]), fm),
           paste(sprintf('<div class="bx-grow" data-genus="%s" style="margin:2px 0 2px 12px">%s<i style="color:%s">%s</i></div>', gg$genus, .dot(gg$col), BEE_HTML[["ink"]], gg$genus), collapse = ""))
  }, character(1)), collapse = "")
.tran_block  <- function(tks) paste(vapply(tks, function(t) sprintf(
    '<div style="margin:3px 0;white-space:nowrap"><span style="display:inline-block;width:18px;height:3px;border-radius:2px;background:%s;vertical-align:middle;margin-right:8px"></span>%s</div>',
    unname(BEE_TRANSECT[t]), t), character(1)), collapse = "")
.legend_wrap <- function(...) paste0('<div style="', .CARD, ';padding:11px 14px;max-height:calc(100vh - 320px);overflow:auto;font-size:12px;line-height:1.4">', ..., '</div>')
# both legends (genus_html for m1, ib_genus_html for m2) are built together in 5b, once the transect
# data exists -- they share the same stacked TRANSECT-over-TARGET-GENERA layout.

# --- 5b. iNaturalist Bee Bounty: photo-targets at their transect + the trail to walk ---
ib_sp <- inat_bounty$taxon[inat_bounty$rank == "species"]
ib_gn <- inat_bounty$taxon[inat_bounty$rank == "genus"]
ib_spec <- spec %>% filter(transect %in% TR, species_key %in% ib_sp | genus_key %in% ib_gn)
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
ib_tx <- dplyr::left_join(ib_tx, .detail_tbl(spec, unique(ib_tx$taxon)), by = "taxon")   # window + flower per species
# per-transect species list: color dot + name, then a muted window + flower sub-line
tr_list <- ib_tx %>% group_by(transect) %>%
  summarise(nt = dplyr::n_distinct(taxon),
            lst = paste(mapply(function(col, taxon, win, top)
              sprintf('<div style="margin:5px 0">%s<i>%s</i>%s<div style="margin-left:19px;font-size:10.5px;line-height:1.3">%s</div></div>',
                      .dot(col), taxon, inat_photo_link(.tid_of(taxon), taxon), .detail_html(taxon, win, top)),
              col, taxon, win, top), collapse = ""),
            .groups = "drop")

# real transect LINES + the NPS park boundary, straight from the GIS shapefiles (single source:
# data/spatial/shapefiles/). Specimen coords are only transect centroids, so we draw the actual survey lines.
read_shp <- function(p) tryCatch(sf::st_transform(sf::st_read(p, quiet = TRUE), 4326), error = function(e) NULL)
park_bnd <- read_shp("data/spatial/shapefiles/boundaries/cabr/nps_official/cabr_boundary_nps_official.shp")
tran_ln  <- read_shp("data/spatial/shapefiles/transects/cabr_bee_transects.shp")
if (!is.null(tran_ln)) {
  tran_ln$transect <- toupper(str_squish(tran_ln$Name))
  tran_ln$col <- ifelse(tran_ln$transect %in% names(BEE_TRANSECT), unname(BEE_TRANSECT[tran_ln$transect]), BEE_HTML[["cn"]])
  tl <- dplyr::left_join(sf::st_drop_geometry(tran_ln)["transect"], tr_list, by = "transect")   # targets per line
  tran_ln$popup <- ifelse(is.na(tl$lst),
    sprintf('<b>%s transect</b><br><span style="font-size:11px;color:%s">no photograph-targets recorded here</span>',
            tran_ln$transect, BEE_HTML[["sub"]]),
    sprintf('<b>%s transect</b> \U2014 %d species to photograph<br><span style="font-size:11px;color:%s">specimen-only: no exact GPS, so walk the whole transect</span>%s',
            tran_ln$transect, ifelse(is.na(tl$nt), 0L, tl$nt), BEE_HTML[["sub"]], tl$lst))
  # label anchor = each line's true MIDPOINT (leaflet otherwise pins polyline labels near an end)
  .midpt <- function(g) { g <- sf::st_line_merge(sf::st_transform(g, 3310))   # CA Albers (metres) for planar interpolation
    if (as.character(sf::st_geometry_type(g)) == "MULTILINESTRING") {
      p <- sf::st_cast(g, "LINESTRING"); g <- p[which.max(as.numeric(sf::st_length(p)))] }
    sf::st_coordinates(sf::st_transform(sf::st_line_interpolate(g, 0.5, normalized = TRUE), 4326))[1, 1:2] }
  mm <- t(vapply(seq_len(nrow(tran_ln)), function(i) .midpt(sf::st_geometry(tran_ln)[i]), numeric(2)))
  tran_lab <- data.frame(transect = tran_ln$transect, lon = mm[, 1], lat = mm[, 2])
}
ib_fam_present <- intersect(BEE_FAMILY_ORDER, unique(ib_taxo$fam))
ib_glg <- ib_taxo %>% group_by(fam, genus) %>% summarise(col = tcol[ceiling(dplyr::n() / 2)], .groups = "drop") %>%
  arrange(match(fam, BEE_FAMILY_ORDER), genus)
# both maps: TRANSECT key stacked ABOVE the TARGET-GENERA key (a thin rule between them)
tks <- if (is.null(tran_ln)) character(0) else { u <- unique(tran_ln$transect); u[order(match(u, names(BEE_TRANSECT)))] }
.legend_stacked <- function(fams, glg) .legend_wrap(
  if (length(tks)) paste0(.col_title("Transects"), .tran_block(tks),
    sprintf('<div style="height:1px;background:%s;margin:9px 0 4px"></div>', BEE_HTML[["scope_rule"]])) else "",
  # #bx-tgt is swapped in-place by the collect map's click-to-isolate JS (genus key <-> selected species)
  '<div id="bx-tgt">', .col_title("Target genera"), .genus_block(fams, glg), '</div>')
genus_html    <- .legend_stacked(fam_present, genus_leg)    # m1 (collect map)
ib_genus_html <- .legend_stacked(ib_fam_present, ib_glg)    # m2 (photograph map)

# ---- collect map (m1) click-to-isolate: click a target dot -> dim every other species, swap the
# legend's target key to the picked species, offer "Show all" to reset. m1 draws real GPS points, so
# each species is its own leaflet group (group = ~taxon) that the JS below can dim/restore. TAXA maps
# taxon -> its dot color. The control-row helper runs first, then this wires the marker clicks.
# per-taxon payload for the drill-down: color (c), iNat-photo count (n, = findability), and whether the
# record is genus-level (g) -- photographed only to genus, so it lists as "Genus sp." not a binomial.
foc <- sb_tgt %>%
  mutate(is_genus = is.na(species_key) | species_key == "") %>%
  group_by(taxon) %>%
  summarise(gcol = dplyr::first(gcol), n = dplyr::n(),
            is_genus = dplyr::first(is_genus), .groups = "drop")
foc_json <- jsonlite::toJSON(setNames(lapply(seq_len(nrow(foc)), function(i)
  list(c = foc$gcol[i], n = foc$n[i], g = foc$is_genus[i])), foc$taxon), auto_unbox = TRUE)
.filter_rest <- paste0(
  "var leg=el.querySelector('#bx-tgt');var legDefault='';el.classList.add('bx-filterable');",
  "function dot(c){return '<span style=\"display:inline-block;width:12px;height:12px;border-radius:50%;background:'+c+';margin-right:8px;vertical-align:middle;box-shadow:0 0 0 1px rgba(0,0,0,.14)\"></span>';}",
  "function gof(t){var i=t.indexOf(' ');return i<0?t:t.substring(0,i);}",   # genus of a taxon ('Genus species' -> 'Genus')
  "var groups={};Object.keys(TAXA).forEach(function(t){groups[t]=[];var g=map.layerManager.getLayerGroup(t,false);if(g)g.eachLayer(function(m){if(m.setStyle){groups[t].push(m);m.on('click',function(){focusSpecies(t);});}});});",
  "function setDim(t,dim){groups[t].forEach(function(m){m.setStyle({opacity:dim?0.15:1,fillOpacity:dim?0.06:0.9});});}",
  # fixed-width wrapper (~ the default target column) so the hint WRAPS instead of forcing the card wide,
  # and the per-row count right-aligns to a stable column.
  # back link is context-aware (bt): a genus name -> back to that genus's species list; '__all__' -> all genera.
  "function render(inner,bl,bt){if(leg)leg.innerHTML='<div style=\"width:264px\"><a href=\"#\" class=\"bx-showall\" data-back=\"'+bt+'\" style=\"display:inline-block;font-size:10.5px;font-weight:600;color:", .DEEP, ";text-decoration:underline;margin:0 0 8px\">&larr; '+bl+'</a><div style=\"font-weight:700;font-size:10.5px;text-transform:uppercase;letter-spacing:.05em;color:", .DEEP, ";margin:0 0 6px\">Showing only</div>'+inner+'</div>';}",
  "function focusTaxa(keep,inner,bl,bt){Object.keys(groups).forEach(function(tt){setDim(tt,!keep[tt]);});render(inner,bl,bt);}",
  "function nm(t){return TAXA[t].g?('<i>'+t+'</i> sp.'):('<i>'+t+'</i>');}",   # display name: genus-level record -> 'Genus sp.'
  "function focusSpecies(t){var k={};k[t]=1;focusTaxa(k,'<div style=\"margin:2px 0;font-size:12px;white-space:nowrap\">'+dot(TAXA[t].c)+'<span>'+nm(t)+'</span></div><div style=\"margin:7px 0 0;font-size:10px;color:", .CN, ";font-style:italic;line-height:1.3\">Click its dot for where &amp; when.</div>','Go back to all species',gof(t));}",
  "function focusGenus(g){var list=Object.keys(TAXA).filter(function(t){return gof(t)===g;}).sort(function(a,b){return TAXA[b].n-TAXA[a].n;});if(!list.length)return;var k={},rows='';list.forEach(function(t){k[t]=1;rows+='<div class=\"bx-sp\" data-taxon=\"'+t+'\" style=\"display:flex;align-items:center;font-size:11.5px;padding:2px 4px\">'+dot(TAXA[t].c)+'<span style=\"white-space:nowrap\">'+nm(t)+'</span><span style=\"margin-left:auto;padding-left:12px;color:", .CN, ";font-size:10px\">'+TAXA[t].n+'</span></div>';});var head='<div style=\"margin:2px 0 5px;white-space:nowrap\"><i>'+g+'</i> <span style=\"color:", .SUB, ";font-size:10.5px\">('+list.length+' to net)</span></div>';var hint='<div style=\"margin:7px 0 0;font-size:10px;color:", .CN, ";font-style:italic;line-height:1.35\">Numbers are iNaturalist photos in the park. Click a target to isolate its dots, or a dot on the map.</div>';focusTaxa(k,head+rows+hint,'Go back to all genera','__all__');}",
  "function resetAll(){Object.keys(groups).forEach(function(tt){setDim(tt,false);});if(leg)leg.innerHTML=legDefault;}",
  "if(leg){var ctl=leg.closest('.leaflet-control');if(ctl)L.DomEvent.disableClickPropagation(ctl);leg.insertAdjacentHTML('beforeend','<div style=\"margin-top:8px;font-size:10px;color:", .CN, ";font-style:italic;line-height:1.3\">Tip: click a genus below, or a dot on the map, to focus it.</div>');legDefault=leg.innerHTML;leg.addEventListener('click',function(e){var sa=e.target.closest('.bx-showall');if(sa){e.preventDefault();var bt=sa.getAttribute('data-back');if(bt==='__all__')resetAll();else focusGenus(bt);return;}var sp=e.target.closest('[data-taxon]');if(sp){e.preventDefault();focusSpecies(sp.getAttribute('data-taxon'));return;}var gr=e.target.closest('[data-genus]');if(gr){e.preventDefault();focusGenus(gr.getAttribute('data-genus'));}});}")
.zoom_filter <- paste0("function(el, x) { (", BEE_MAP_CTRLROW_JS, ").call(this, el, x); var map=this; var TAXA=", foc_json, "; ", .filter_rest, " }")

# ---- build + save the two interactive maps (shared lib/ dir) ----
TILE_SAT <- "Esri.WorldImagery"; TILE_STR <- "CartoDB.Positron"; TILE_TOPO <- "Esri.WorldTopoMap"
# short one-line basemap credit (matches the explorer) -- the provider default is a long source list
# that wraps onto the bottom-left legend at narrow windows.
.ATTR_ESRI  <- leaflet::providerTileOptions(attribution = "Tiles &copy; Esri")
.ATTR_CARTO <- leaflet::providerTileOptions(attribution = "&copy; OpenStreetMap &copy; CARTO")
# official title overlay (top-left corner) -- white card w/ NPS eyebrow + teal head, matching the tables
.map_title <- function(head, sub, note = NULL) paste0(
  '<div style="', .CARD, ';padding:9px 15px;max-width:430px">',
  sprintf('<div style="font-size:9.5px;font-weight:700;text-transform:uppercase;letter-spacing:.11em;color:%s;margin-bottom:2px">Cabrillo National Monument</div>', BEE_HTML_GREEN[["mid"]]),
  sprintf('<div style="font-weight:700;font-size:15px;letter-spacing:-.01em;line-height:1.18;white-space:nowrap;color:%s">%s</div>', BEE_HTML_GREEN[["deep"]], head),
  sprintf('<div style="font-size:11.5px;color:%s;margin-top:3px;line-height:1.35">%s</div>', BEE_HTML[["sub"]], sub),
  if (!is.null(note)) sprintf('<div style="font-size:10px;color:%s;margin-top:6px;padding-top:6px;border-top:1px solid %s;line-height:1.35">%s</div>', BEE_HTML[["sub"]], BEE_HTML[["border"]], note) else "",
  '</div>')
# shared "look widely" disclaimer: the plants/months shown reflect where people have LOOKED, not where the
# bees truly are -- keep observers from narrowing their search (and biasing future records) to those spots.
.LOOK_WIDELY <- paste0("<b>Look beyond this map.</b> These plants and months are where people have already found ",
  "these bees, not the whole picture of where they live. The bees we are still missing are out there on plants ",
  "and at times no one has checked. So don't just search the spots listed here. Look widely.")
# Brief provenance, same idea as the document pages: what was pooled, source, data-as-of.
.MAP_SCOPE <- paste0("<div style=\"margin-top:5px;opacity:.8\">All bee records, photos and netted specimens pooled. ",
  "Source: iNaturalist observations and park specimen records, Cabrillo National Monument (data as of ",
  bee_data_asof(), ").</div>")
title1 <- .map_title("Bee Bounty: Native Species to Collect\U00A0\U0001F52C", "These bees turn up in iNaturalist photos but aren't in the collection yet. Find one in the field and net it for a voucher!", paste0(.LOOK_WIDELY, .MAP_SCOPE))
title2 <- .map_title("Bee Bounty: Native Species to Photograph\U00A0\U0001F4F7", "These bees are in the collection but still missing an iNaturalist photo. Pick a transect to see what it needs, then head out and snap one!", paste0(.LOOK_WIDELY, .MAP_SCOPE))
# adds zoom, then relocates zoom/basemap/north/scale into ONE bottom-centre row (shared helper, so every
# interactive map -- explorer, transect, both bounties -- carries the identical control strip).
.zoom_tr <- BEE_MAP_CTRLROW_JS
# north arrow (moved into the bottom control row by .zoom_tr; must carry id="bx-north")
.north <- paste0('<div id="bx-north" style="', .CARD, ';padding:5px 9px 6px;text-align:center;line-height:1.05">',
  sprintf('<div style="font-weight:700;font-size:11px;color:%s;margin-bottom:1px">N</div>', BEE_HTML_GREEN[["deep"]]),
  sprintf('<svg width="14" height="16" viewBox="0 0 14 16"><polygon points="7,0 12.5,15.5 7,11.5 1.5,15.5" fill="%s"/></svg>', BEE_INK[["primary"]]),
  '</div>')
# park-boundary outline (white casing + dark-teal core = crisp on either basemap); no-op if the
# shapefile failed to load. Same helper on both maps.
.add_boundary <- function(m) if (is.null(park_bnd)) m else m %>%
  leaflet::addPolygons(data = park_bnd, fill = FALSE, color = BEE_HTML[["page"]], weight = 3, opacity = 0.95, group = "park boundary")

m1 <- leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = FALSE)) %>%
  leaflet::addProviderTiles(TILE_TOPO, group = "Topographic", options = .ATTR_ESRI) %>%
  leaflet::addProviderTiles(TILE_SAT, group = "Satellite", options = .ATTR_ESRI) %>%
  leaflet::addProviderTiles(TILE_STR, group = "Street", options = .ATTR_CARTO) %>%
  .add_boundary()
if (!is.null(tran_ln))   # transect lines as CONTEXT here (the collect-targets are real GPS points, drawn on top)
  m1 <- m1 %>% leaflet::addPolylines(data = tran_ln, color = ~col, weight = 4, opacity = 0.9,
      group = "transects") %>%
    leaflet::addLabelOnlyMarkers(data = tran_lab, lng = ~lon, lat = ~lat, label = ~transect,
      labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "top",
              style = list("font-weight" = "700", "background" = "rgba(255,255,255,0.85)", "padding" = "1px 5px")),
      group = "transects")
m1 <- m1 %>%
  leaflet::addCircleMarkers(data = sb_tgt, lng = ~lon, lat = ~lat, radius = 5, color = "white",
      weight = 1, fillColor = ~gcol, fillOpacity = 0.9, popup = ~popup,
      group = ~taxon) %>%          # one leaflet group per species so click-to-isolate can dim the rest
  leaflet::addControl(html = genus_html, position = "bottomleft") %>%
  leaflet::addControl(html = title1, position = "topleft") %>%
  leaflet::addControl(html = .north, position = "topright") %>%
  leaflet::addScaleBar(position = "bottomright", options = leaflet::scaleBarOptions(imperial = FALSE, maxWidth = 150)) %>%
  leaflet::addLayersControl(baseGroups = c("Topographic", "Satellite", "Street"),
      overlayGroups = c("park boundary", if (!is.null(tran_ln)) "transects"),
      options = leaflet::layersControlOptions(collapsed = TRUE)) %>%
  htmlwidgets::onRender(.zoom_filter)   # control row + click-to-isolate a species
m1 <- htmlwidgets::prependContent(m1, htmltools::tags$style(htmltools::HTML(BEE_MAP_CTRLROW_CSS)))

# m2: one layer for the transects -- the real shapefile line, colored by transect, carrying a SINGLE
# popup (its species to photograph) and a permanent name label. No more twin corridor/marker popups.
m2 <- leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = FALSE)) %>%
  leaflet::addProviderTiles(TILE_TOPO, group = "Topographic", options = .ATTR_ESRI) %>%
  leaflet::addProviderTiles(TILE_SAT, group = "Satellite", options = .ATTR_ESRI) %>%
  leaflet::addProviderTiles(TILE_STR, group = "Street", options = .ATTR_CARTO) %>%
  .add_boundary()
if (!is.null(tran_ln))
  m2 <- m2 %>% leaflet::addPolylines(data = tran_ln, color = ~col, weight = 4, opacity = 0.9, popup = ~popup,
      highlightOptions = leaflet::highlightOptions(weight = 7, opacity = 1, bringToFront = TRUE),
      group = "transects") %>%
    leaflet::addLabelOnlyMarkers(data = tran_lab, lng = ~lon, lat = ~lat, label = ~transect,   # label at the line midpoint
      labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "top",
              style = list("font-weight" = "700", "background" = "rgba(255,255,255,0.85)", "padding" = "1px 5px")),
      group = "transects")
m2 <- m2 %>%
  leaflet::addControl(html = ib_genus_html, position = "bottomleft") %>%
  leaflet::addControl(html = title2, position = "topleft") %>%
  leaflet::addControl(html = .north, position = "topright") %>%
  leaflet::addScaleBar(position = "bottomright", options = leaflet::scaleBarOptions(imperial = FALSE, maxWidth = 150)) %>%
  leaflet::addLayersControl(baseGroups = c("Topographic", "Satellite", "Street"),
      overlayGroups = c("park boundary", if (!is.null(tran_ln)) "transects"),
      options = leaflet::layersControlOptions(collapsed = TRUE)) %>%
  htmlwidgets::onRender(.zoom_tr)
m2 <- htmlwidgets::prependContent(m2, htmltools::tags$style(htmltools::HTML(BEE_MAP_CTRLROW_CSS)))
# self-contained single file when pandoc is available (matches every other HTML in the project);
# fall back to a lib/ folder only if pandoc is missing, so the pipeline never hard-fails.
.sc <- requireNamespace("rmarkdown", quietly = TRUE) && isTRUE(try(rmarkdown::pandoc_available(), silent = TRUE))
.libd <- if (.sc) NULL else "lib"    # self-contained -> no external lib dir; fallback -> a shared lib/ folder
htmlwidgets::saveWidget(m1, file.path(normalizePath(.web(OUT_DIR)), "specimen_bee_bounty_map.html"),
                        selfcontained = .sc, libdir = .libd, title = "Specimen Bee Bounty -- Where to Net a Voucher")
htmlwidgets::saveWidget(m2, file.path(normalizePath(.web(OUT_DIR)), "inaturalist_bee_bounty_map.html"),
                        selfcontained = .sc, libdir = .libd, title = "iNaturalist Bee Bounty -- Where to Photograph")
if (.sc) unlink(c(file.path(normalizePath(.web(OUT_DIR)), "lib"),                                   # tidy: self-contained html inlines everything, so
                  Sys.glob(file.path(normalizePath(.web(OUT_DIR)), "*_map_files"))), recursive = TRUE) # the lib/ + *_map_files dep dirs are just leftovers
message(sprintf("Wrote specimen_bee_bounty_map.html + inaturalist_bee_bounty_map.html (%s) to %s",
                if (.sc) "self-contained single files" else "interactive; shared lib/ folder", OUT_DIR))
