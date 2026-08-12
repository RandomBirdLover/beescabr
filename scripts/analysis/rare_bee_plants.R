# =============================================================
# analysis/rare_bee_plants.R
# beescabr -- plants used by the park's RARE / at-risk native bees  (for management)
#
# THE QUESTION: which plants do the park's rare bees rely on, so management knows
# what to protect and plant? "Rare" here is two things:
#   (a) THREATENED bees on the IUCN Red List (categories CR/EN/VU), read live from the
#       IUCN cache (data/checklists/iucn/iucn_status.csv, written by refresh_iucn_status.R)
#       so a newly listed species is picked up automatically; and
#   (b) every bee species we have FEWER THAN `RARE_CUT` records of (rarely seen here).
#
# A "plant visit" = a bee record (specimen net OR iNaturalist photo) that carries a
# plant_genus, pooled across methods. Counts are small (these bees are rarely seen) --
# read them as WHAT THE BEE WAS RECORDED ON ("where the few sightings concentrate"), NOT as
# what it prefers: with so few records you can't separate a real preference from whatever
# was blooming. The one exception: threatened bees with >= 20 records get an availability-
# corrected PREFERRED plant (same matched test as the genus webs); the rest stay recorded-only.
#
# TWO figures:
#   A. HUBS  -- plant genera ranked by HOW MANY different rare (< RARE_CUT record) bee
#      species use them. A plant feeding many rare bees is a conservation hub.
#   B. THREATENED -- each IUCN-threatened bee's own plant list (per species).
#
# Run from the repo root:  Rscript scripts/analysis/rare_bee_plants.R
# Depends on: dplyr, stringr, ggplot2 (+ config.R).
# =============================================================

for (pkg in c("ggplot2", "ggtext")) {   # ggtext: rich-text facet strips (italicise the Latin in panel headers)
  if (!requireNamespace(pkg, quietly = TRUE))
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
}
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(ggplot2) })

if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_SEQ")) source("scripts/analysis/theme_beescabr.R")   # shared house style
if (!exists("iucn_table")) source("scripts/analysis/conservation_status.R")   # shared IUCN lookups
if (!exists("plant_label")) source("scripts/analysis/plant_names.R")          # shared plant common-name labels
OUT_DIR <- file.path(DIR_REPORT, "reference/conservation")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

RARE_CUT      <- 25                              # a bee is "rare" if we have < this many records
SPECIES_RANKS <- c("species", "subspecies")
has <- function(x) !is.na(x) & x != ""

# ---- 1. all bee records: species key + the plant they were on ----------------
grab <- function(df, method) data.frame(
  method        = method,
  taxon_rank    = tolower(str_squish(df$taxon_rank)),
  genus         = str_squish(df$genus),
  epithet       = tolower(word(str_squish(df$species), -1)),
  common_name   = str_squish(df$common_name),
  plant_genus   = str_squish(df$plant_genus),
  plant_species = str_squish(df$plant_species),
  month         = suppressWarnings(as.integer(substr(df$observed_on, 6, 7))),
  year          = suppressWarnings(as.integer(substr(df$observed_on, 1, 4))),
  stringsAsFactors = FALSE)
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
rec  <- bind_rows(grab(spec, "specimen (net)"), grab(inat, "photo (iNat)")) %>%
  mutate(species_key = ifelse(taxon_rank %in% SPECIES_RANKS & genus != "" & epithet != "",
                              paste(genus, epithet), NA_character_))

# ---- 2. rarity: species-level record counts -> the rare (< RARE_CUT) set ------
sp_counts <- rec %>% filter(!is.na(species_key)) %>% count(species_key, name = "n_records")
rare_keys <- sp_counts$species_key[sp_counts$n_records < RARE_CUT]
message(sprintf("Rare bees (< %d records): %d species", RARE_CUT, length(rare_keys)))

# ---- 3. FIGURE A: plant HUBS -- plants that feed the most different rare bees --
rare_vis <- rec %>% filter(species_key %in% rare_keys, has(plant_genus))
hub <- rare_vis %>% group_by(plant_genus) %>%
  summarise(rare_bee_species = n_distinct(species_key), visits = n(), .groups = "drop") %>%
  arrange(desc(rare_bee_species), desc(visits))
write.csv(hub, file.path(OUT_DIR, "rare_bee_plant_hubs.csv"), row.names = FALSE)
message(sprintf("  plant genera used by rare bees: %d (top: %s)",
                nrow(hub), paste(sprintf("%s[%dspp]", head(hub$plant_genus, 4), head(hub$rare_bee_species, 4)), collapse = " ")))

hub_fig <- hub %>% filter(rare_bee_species >= 2)               # the multi-rare-bee hubs
hub_fig$plant_lab <- plant_label(hub_fig$plant_genus)          # "Common Name (Genus)" for the reader
hub_fig$plant_lab <- factor(hub_fig$plant_lab, levels = rev(hub_fig$plant_lab))
gA <- ggplot(hub_fig, aes(x = rare_bee_species, y = plant_lab, fill = rare_bee_species)) +
  geom_col(width = 0.72) +
  scale_fill_gradientn(colors = BEE_RARE, guide = "none") +   # RED rare/urgent ramp: gradient shades the count
  scale_x_continuous(breaks = scales::breaks_width(2), expand = expansion(mult = c(0, 0.05))) +
  # plant y-axis: common name upright, Latin in parentheses italic (plotmath)
  scale_y_discrete(labels = function(x) as.expression(lapply(x, function(s) {
    m <- regmatches(s, regexec("^(.*) \\(([^)]*)\\)$", s))[[1]]
    if (length(m) == 3) bquote(.(m[2]) ~ "(" * italic(.(m[3])) * ")") else bquote(italic(.(s)))
  }))) +
  labs(title = "Plant Hubs for the Park's Rare Bees",
       subtitle = "A few plants are shared hubs for many of the park's rare bees -- the priorities to protect.",
       caption = paste0(str_wrap(sprintf("Bar = how many different rare bees (< %d records) were recorded on that plant -- where sightings fall, not a tested preference.", RARE_CUT), 104), "\n",
                        str_wrap(scope_cap(sprintf("%d rare bee species (< %d records); hubs used by 2+", length(rare_keys), RARE_CUT),
                            "lethal + non-lethal pooled", "plant genus"), 96)),
       x = "rare bee species recorded", y = NULL) +
  theme_beescabr(11) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none", panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(size = 8.5))
bee_ggsave(file.path(OUT_DIR, "rare_bee_plant_hubs.png"), gA, width = 9, height = 5.8, bg = "white")

# ---- 3b. species-level PREFERRED plant (availability-corrected) -------------
# Same matched approach as the genus forage-selectivity test, keyed to ONE bee species:
# expected plant use = for each (year, month, method) cell the species was recorded in, the
# community's plant-use shares in that same cell (leave-one-out), blended by the species'
# effort per cell; preference = observed / expected. Only trustworthy with enough records --
# returns NA below SP_PREF_MIN, because with a handful of sightings you can't separate a real
# preference from whatever happened to be blooming.
SP_PREF_MIN <- 20
sp_visits <- rec %>% filter(!is.na(species_key), has(plant_genus), !is.na(month), !is.na(year))
preferred_plant_sp <- function(sp_key) {
  d <- sp_visits; plants <- sort(unique(d$plant_genus)); P <- length(plants)
  isb <- d$species_key == sp_key; rb <- d[isb, , drop = FALSE]; n <- nrow(rb)
  if (n < SP_PREF_MIN) return(list(pref = NA_character_, ratio = NA_real_, n = n))
  rc <- d[!isb, , drop = FALSE]
  x  <- as.numeric(table(factor(rb$plant_genus, plants))); names(x) <- plants
  gmarg <- as.numeric(table(factor(d$plant_genus, plants))); gmarg <- gmarg / sum(gmarg)
  rb$ym <- rb$year * 100L + rb$month; rc$ym <- rc$year * 100L + rc$month
  MIN_CELL <- 8; REG <- 0.05
  share_of <- function(pg) { t <- as.numeric(table(factor(pg, plants))); if (sum(t) == 0) NULL else t / sum(t) }
  cw <- table(paste(rb$ym, rb$method, sep = "|")) / n; E <- numeric(P)
  for (k in names(cw)) {
    parts <- strsplit(k, "|", fixed = TRUE)[[1]]; ymk <- as.integer(parts[1]); mth <- parts[2]; mm <- ymk %% 100L
    m1 <- rc$ym == ymk & rc$method == mth; sh <- if (sum(m1) >= MIN_CELL) share_of(rc$plant_genus[m1]) else NULL
    if (is.null(sh)) { m2 <- rc$month == mm & rc$method == mth; sh <- if (sum(m2) >= MIN_CELL) share_of(rc$plant_genus[m2]) else NULL }
    if (is.null(sh)) { m3 <- rc$month == mm;                    sh <- if (sum(m3) >= MIN_CELL) share_of(rc$plant_genus[m3]) else gmarg }
    if (is.null(sh)) sh <- gmarg
    E <- E + as.numeric(cw[k]) * sh
  }
  if (sum(E) <= 0) E <- gmarg; E <- E / sum(E); E <- (1 - REG) * E + REG / P; names(E) <- plants
  ratio <- ifelse(E > 0, (x / n) / E, NA_real_); names(ratio) <- plants
  elig <- x >= pmax(3, 0.05 * n)
  if (!any(elig)) return(list(pref = NA_character_, ratio = NA_real_, n = n))
  pref <- names(which.max(ifelse(elig, ratio, -Inf)))
  list(pref = pref, ratio = round(unname(ratio[pref]), 1), n = n)
}

# ---- 4. FIGURE B: each IUCN-threatened bee's plants -------------------------
# threatened set comes from the shared module (IUCN CR/EN/VU, live from the cache)
.thr          <- flagged_species(IUCN_THREAT_CODES)
threat_status <- setNames(.thr$iucn_category, .thr$scientific_name)
threat_keys   <- intersect(.thr$scientific_name, unique(rec$species_key))   # only ones we recorded
cn_of  <- rec %>% filter(species_key %in% threat_keys, has(common_name)) %>%
  count(species_key, common_name) %>% group_by(species_key) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>% ungroup()
cn_map <- setNames(cn_of$common_name, cn_of$species_key)
NAMED <- data.frame(
  species_key = threat_keys,
  label = vapply(threat_keys, function(k) {
    base <- if (!is.na(cn_map[k]) && nzchar(cn_map[k])) sprintf("%s  (%s)", cn_map[k], k) else k
    sprintf("%s  --  IUCN %s", base, unname(threat_status[k]))
  }, character(1)),
  # markdown variant for the ggtext facet strip: bee Latin italic, common name upright
  label_md = vapply(threat_keys, function(k) {
    base <- if (!is.na(cn_map[k]) && nzchar(cn_map[k])) sprintf("%s  (<i>%s</i>)", cn_map[k], k) else sprintf("<i>%s</i>", k)
    sprintf("%s  --  IUCN %s", base, unname(threat_status[k]))
  }, character(1)),
  stringsAsFactors = FALSE)
md_of_label <- setNames(NAMED$label_md, NAMED$label)   # plain label -> markdown label
message(sprintf("IUCN-threatened bees with records: %d (%s)", nrow(NAMED),
                if (nrow(NAMED)) paste(threat_keys, collapse = ", ") else "none"))

named    <- rec %>% inner_join(NAMED, by = "species_key")
tot      <- named %>% group_by(label) %>%
  summarise(n_records = n(), n_visits = sum(has(plant_genus)), .groups = "drop")
named_v  <- named %>% filter(has(plant_genus))
pg       <- named_v %>% count(label, plant_genus, name = "visits")
top_sp   <- named_v %>% filter(has(plant_species)) %>% count(label, plant_genus, plant_species, name = "n") %>%
  group_by(label, plant_genus) %>% slice_max(n, n = 1, with_ties = FALSE) %>% ungroup() %>%
  transmute(label, plant_genus, top_plant_species = plant_species)
named_tbl <- pg %>% left_join(top_sp, by = c("label", "plant_genus")) %>%
  left_join(tot, by = "label") %>% arrange(label, desc(visits), plant_genus)
write.csv(named_tbl, file.path(OUT_DIR, "rare_named_bee_plants.csv"), row.names = FALSE)

# availability-corrected PREFERRED plant per threatened bee (NA where < SP_PREF_MIN records)
pref_of <- setNames(lapply(NAMED$species_key, preferred_plant_sp), NAMED$label)
# strip = what the bee was RECORDED on (bars) + its availability-corrected PREFERRED plant
# where records allow, else an explicit "too few to judge" note.
# strip text as markdown (ggtext): bee Latin + preferred-plant Latin italic; <br> = line break
panel_of <- setNames(vapply(tot$label, function(L) {
  pf <- pref_of[[L]]
  l2 <- if (!is.na(pf$pref)) sprintf("prefers %s", plant_label(pf$pref, sci_wrap = "<i>%s</i>"))
        else "too few records to judge a preference"
  sprintf("%s  &middot;  %d records<br>%s", md_of_label[L], tot$n_records[tot$label == L], l2)
}, character(1)), tot$label)
# The availability-corrected PREFERRED plant is called out in each panel's strip ("PREFERS X -- Nx
# vs available") rather than marked on the bar -- the strip wording carries it cleanly.
plot_df  <- pg %>% mutate(panel = panel_of[label], row_key = paste(label, plant_genus, sep = "@@"))
lev <- plot_df %>% arrange(visits, plant_genus) %>% pull(row_key)
plot_df$row_key <- factor(plot_df$row_key, levels = unique(lev))
gB <- ggplot(plot_df, aes(x = visits, y = row_key, fill = visits)) +
  geom_col(width = 0.72) +
  facet_wrap(~ panel, ncol = 1, scales = "free") +
  scale_y_discrete(labels = function(x) as.expression(plant_label_expr(sub("^.*@@", "", x)))) +   # plant: common upright, Latin italic
  scale_fill_gradientn(colors = BEE_RARE, guide = "none") +   # RED rare/urgent ramp: gradient shades the count
  scale_x_continuous(expand = expansion(mult = c(0, 0.03))) +
  labs(title = "Plant Hubs for the Park's IUCN-Threatened Bees",
       subtitle = "The park's threatened bees lean on a few key plants -- Deervetch and Milkvetches recur across them.",
       caption = paste0(str_wrap("Bars = where each bee's records fall (what it was recorded on, not corrected for bloom). A preferred plant is named only for bees with >= 20 records.", 104), "\n",
                        str_wrap(scope_cap("IUCN-threatened bees (CR/EN/VU); live from the IUCN Red List",
                            "lethal + non-lethal pooled", "plant genus"), 92)),
       x = "plant visits", y = NULL) +
  theme_beescabr(11) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none", panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(size = 8.5),
        strip.text = ggtext::element_markdown(face = "bold", hjust = 0, size = 9, lineheight = 1.3))  # rich text: italic Latin in the panel header
bee_ggsave(file.path(OUT_DIR, "rare_named_bee_plants.png"), gB,
       width = 11, height = 2.2 + 3.1 * length(unique(plot_df$panel)), bg = "white")

# =============================================================================
# 5. PRESENTATION view -- the "flower": each threatened bee at the CENTRE of a ring
#    of its plants (petal = a plant; bigger/darker = more visits; gold ring = its
#    availability-corrected favourite). One clean radial per bee, made for slides.
# =============================================================================
FLOWER_TOP <- 8                                                   # cap petals so the ring stays readable
iucn_of    <- setNames(sprintf("IUCN %s", unname(threat_status[NAMED$species_key])), NAMED$label)
common_of  <- setNames(ifelse(!is.na(cn_map[NAMED$species_key]) & nzchar(cn_map[NAMED$species_key]),
                              cn_map[NAMED$species_key], NAMED$species_key), NAMED$label)
petal_expr <- function(g) { lab <- plant_label(g)                 # petal label: common name (roman) or, if none, the genus (italic)
  if (grepl("^\\(", lab)) bquote(italic(.(g))) else bquote(.(sub(" \\(.*$", "", lab))) }

# a tiny bee drawn from shapes (the png device here won't render the emoji glyph) -- gold body,
# dark stripes, pale wings; sits on the crimson centre node.
draw_bee <- function(cx = 0, cy = 0) {
  ell <- function(ox, oy, rx, ry) { t <- seq(0, 2*pi, length.out = 60); list(x = cx+ox+rx*cos(t), y = cy+oy+ry*sin(t)) }
  w <- adjustcolor("#F5F3EE", 0.92)
  wl <- ell(-0.055, 0.11, 0.10, 0.066); polygon(wl$x, wl$y, col = w, border = adjustcolor("#7d7d7d", 0.5))
  wr <- ell( 0.055, 0.11, 0.10, 0.066); polygon(wr$x, wr$y, col = w, border = adjustcolor("#7d7d7d", 0.5))
  b  <- ell(0, 0, 0.17, 0.115); polygon(b$x, b$y, col = "#E8B93B", border = "#2a2208", lwd = 1.6)   # gold body
  for (dx in c(-0.075, -0.005, 0.065)) segments(cx+dx, cy-0.093, cx+dx, cy+0.093, col = "#2a2208", lwd = 3.4)  # stripes
  h <- ell(0.185, 0, 0.055, 0.06); polygon(h$x, h$y, col = "#2a2208", border = NA)   # head
}

draw_flower <- function(lbl) {
  d <- pg[pg$label == lbl, ]; d <- d[order(-d$visits), ]
  if (nrow(d) > FLOWER_TOP) d <- d[seq_len(FLOWER_TOP), ]
  N <- nrow(d); v <- d$visits; wmax <- max(v)
  vr   <- if (wmax > min(v)) (v - min(v)) / (wmax - min(v)) else rep(0.7, N)   # crimson shade by visits (the gradient kept)
  pcol <- grDevices::colorRampPalette(BEE_RARE)(101)[round(vr * 100) + 1]
  ang  <- pi/2 - 2*pi*(seq_len(N) - 1)/N; R <- 0.72                 # petals clockwise from the top, most-visited first
  px <- R*cos(ang); py <- R*sin(ang)
  pf <- pref_of[[lbl]]$pref
  plot.new(); plot.window(xlim = c(-1.7, 1.7), ylim = c(-1.45, 1.6), asp = 1)
  for (i in seq_len(N)) segments(0, 0, px[i], py[i], lwd = 1 + 8*v[i]/wmax, col = adjustcolor(pcol[i], 0.6))  # spoke width = visits
  ns <- 0.05 + 0.11*sqrt(v/wmax)
  symbols(px, py, circles = ns, inches = FALSE, add = TRUE, bg = pcol, fg = "white", lwd = 1.5)
  if (!is.null(pf) && !is.na(pf) && pf %in% d$plant_genus) {        # gold ring = availability-corrected favourite
    j <- match(pf, d$plant_genus)
    symbols(px[j], py[j], circles = ns[j] + 0.035, inches = FALSE, add = TRUE, fg = BEE_WEB[["bee"]], bg = NA, lwd = 3)
  }
  lx <- px + (ns + 0.09)*cos(ang); ly <- py + (ns + 0.09)*sin(ang)
  adjx <- ifelse(cos(ang) > 0.15, 0, ifelse(cos(ang) < -0.15, 1, 0.5))
  for (i in seq_len(N)) text(lx[i], ly[i], petal_expr(d$plant_genus[i]), adj = c(adjx[i], 0.5), cex = 0.78, col = BEE_INK$primary)
  symbols(0, 0, circles = 0.25, inches = FALSE, add = TRUE, bg = BEE_RARE[[5]], fg = "white", lwd = 2)   # centre node
  draw_bee(0, 0)                                                                                          # the bee, drawn from shapes
  text(0, 1.46, common_of[lbl], font = 2, cex = 1.0, col = BEE_INK$primary)
  text(0, 1.28, iucn_of[lbl],   cex = 0.72, col = BEE_INK$secondary)
}
ord <- tot$label[order(-tot$n_records)]                            # biggest-sampled bee first
bee_png(file.path(OUT_DIR, "rare_threatened_bee_flowers.png"), width = 720*length(ord), height = 1080, res = 200)
bee_base_par(); par(mfrow = c(1, length(ord)), mar = c(1, 1, 1, 1), oma = c(5.2, 0, 3.2, 0), xpd = NA)
for (lbl in ord) draw_flower(lbl)
mtext("Plants the Park's Threatened Bees Rely On", side = 3, outer = TRUE, font = 2, cex = 1.15, col = BEE_INK$primary, line = 1.5)
mtext("A handful of plants -- Deervetch, Milkvetches, Wirelettuces -- carry the park's threatened bees.",
      side = 3, outer = TRUE, cex = 0.82, col = BEE_INK$secondary, line = 0.4)   # takeaway
# standardized caption: figure note first, then the shared scope_cap provenance line(s)
mtext("Bee at the centre; each petal = a plant it's recorded on (bigger + darker = more visits).  Gold ring = its availability-corrected favourite.",
      side = 1, outer = TRUE, cex = 0.72, col = BEE_INK$secondary, line = 0.9)
.fprov <- strsplit(scope_cap(scope = "IUCN-threatened bees (CR/EN/VU); plant records pooled, live from the IUCN Red List",
                             method = "lethal + non-lethal pooled", rank = "plant genus"), "\n")[[1]]
for (.k in seq_along(.fprov)) mtext(.fprov[.k], side = 1, outer = TRUE, cex = 0.66, col = BEE_INK$secondary, line = 1.9 + 0.9 * (.k - 1))
dev.off()

message("Wrote rare_bee_plant_hubs.{png,csv} + rare_named_bee_plants.{png,csv} + rare_threatened_bee_flowers.png to ", OUT_DIR)
