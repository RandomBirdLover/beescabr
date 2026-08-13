# =============================================================
# analysis/least_sampled_bees.R
# beescabr -- the park's LEAST-SAMPLED native bees, in ONE go-find-it sheet.
#
# THE QUESTION: which bee species are barely sampled -- thin in specimens AND thin in
# photos -- and when / where / on what flower should a surveyor go look for them?
#
# "Least sampled" = fewer than THIN_TOTAL records TOTAL across BOTH methods (the
# report's 50-record low-sample floor). Because the total is small, each method is small
# too, so every species here is under-detected by netting AND by iNaturalist. The
# per-method split is shown so you can see the shape of the gap:
#   * both (thin)   -- a few of each method (seen a little either way)
#   * photo-only    -- only iNaturalist photos, never netted  (also a specimen bounty)
#   * specimen-only -- only in the collection, never photographed (also an iNat bounty)
#
# Context is POOLED across both methods (the goal is to find the bee at all):
#   WHEN  = peak months (top 2 by records) + the month span it has been seen in
#   WHERE = the transect(s) it turns up on most (or "off-transect")
#   FLOWER= the plant genera it is most recorded on
# plus an example iNaturalist photo URL where one exists.
#
# Companion to bee_bounties.R (which lists taxa MISSING from one method) and
# records_per_species_by_evidence.R (the raw per-method counts). This one keeps only
# the under-sampled species and attaches the find-it context in a single table.
#
# Outputs CSV + a styled sortable HTML table + a PNG table image to
# data/analysis/coverage/least_sampled/.
#
# Run from the repo root:  Rscript scripts/analysis/least_sampled_bees.R
# Depends on: dplyr, stringr (+ config.R). PNG needs gridExtra + ggplot2 (optional).
# =============================================================

suppressPackageStartupMessages({ library(dplyr); library(stringr) })
if (!exists("PATHS")) source("scripts/config.R")
if (!exists("iucn_table"))  try(source("scripts/analysis/conservation_status.R"), silent = TRUE)  # IUCN (optional)
if (!exists("plant_label")) source("scripts/analysis/plant_names.R")                                # plant common names
if (!exists("scope_cap"))   source("scripts/analysis/theme_beescabr.R")                              # shared scope-caption format

OUT_DIR       <- file.path(DIR_REPORT, "coverage/least_sampled")
SPECIES_RANKS <- c("species", "subspecies")
THIN_TOTAL    <- 50          # < this many records TOTAL (both methods) -> "least sampled" (matches the report's 50-record floor)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
has <- function(x) !is.na(x) & x != ""

# ---- 1. pool records, tag method, keep species-level rows -------------------
grab <- function(df, method) data.frame(
  method      = method,
  taxon_rank  = tolower(str_squish(df$taxon_rank)),
  genus       = str_squish(df$genus),
  epithet     = tolower(word(str_squish(df$species), -1)),
  common      = str_squish(df$common_name),
  month       = suppressWarnings(as.integer(substr(df$observed_on, 6, 7))),
  plant_genus = str_squish(df$plant_genus),
  transect    = toupper(str_squish(df$transect)),
  url         = if ("url" %in% names(df)) df$url else NA_character_,
  stringsAsFactors = FALSE)
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
rec  <- bind_rows(grab(spec, "specimen (net)"), grab(inat, "photo (iNat)")) %>%
  filter(taxon_rank %in% SPECIES_RANKS, has(genus), has(epithet)) %>%
  mutate(species = paste(genus, epithet))

# ---- 2. per-species method split, keep the under-sampled -------------------
split_tbl <- rec %>% group_by(species) %>%
  summarise(net_records   = sum(method == "specimen (net)"),
            photo_records  = sum(method == "photo (iNat)"),
            total_records  = n(), .groups = "drop") %>%
  filter(total_records < THIN_TOTAL) %>%
  arrange(total_records, desc(pmin(net_records, photo_records)), species)
message(sprintf("Least-sampled bees (< %d records total): %d species", THIN_TOTAL, nrow(split_tbl)))

coverage_of <- function(net, photo)
  ifelse(net > 0 & photo > 0, "both (thin)", ifelse(photo > 0, "photo-only", "specimen-only"))

# ---- 3. context per species (pooled across methods) ------------------------
top2 <- function(x) { x <- x[has(x)]; if (!length(x)) return("-")
  tb <- sort(table(x), decreasing = TRUE); paste(names(tb)[seq_len(min(2, length(tb)))], collapse = ", ") }
peak_mo <- function(m) { m <- m[!is.na(m)]; if (!length(m)) return("-")
  tb <- sort(table(m), decreasing = TRUE); month.abb[as.integer(names(tb)[1])] }   # single busiest month
span_mo <- function(m) { m <- m[!is.na(m)]; if (!length(m)) return("-")
  r <- range(m); if (r[1] == r[2]) month.abb[r[1]] else paste0(month.abb[r[1]], "-", month.abb[r[2]]) }
mode_chr <- function(x) { x <- x[has(x)]; if (!length(x)) return("") ; names(sort(table(x), decreasing = TRUE))[1] }

ctx <- lapply(split_tbl$species, function(k) {
  d  <- rec[rec$species == k, ]
  fl <- d$plant_genus[has(d$plant_genus)]
  u  <- d$url[has(d$url)]
  fl_top <- if (length(fl)) names(sort(table(fl), decreasing = TRUE))[seq_len(min(3, length(unique(fl))))] else character(0)
  data.frame(
    species       = k,
    # species-level row -> take the SPECIES vernacular, not a subspecies' name that may
    # dominate the pooled records (see A. urbana vs its clementina subspecies in the field guide).
    common_name   = { cn <- mode_chr(d$common[d$taxon_rank == "species"]); if (cn == "") cn <- mode_chr(d$common); cn },
    peak_months   = peak_mo(d$month),
    active_window = span_mo(d$month),
    top_transects = top2(d$transect),
    top_flowers      = if (length(fl_top)) paste(plant_label(fl_top), collapse = ", ") else "- (no flower records)",
    top_flowers_html = if (length(fl_top)) paste(plant_label(fl_top, sci_wrap = "<i>%s</i>"), collapse = ", ") else "- (no flower records)",  # HTML: Latin italic
    example_url   = if (length(u)) u[1] else "",
    stringsAsFactors = FALSE)
})
ctx <- do.call(rbind, ctx)

tbl <- split_tbl %>% left_join(ctx, by = "species") %>%
  mutate(coverage = coverage_of(net_records, photo_records))

# IUCN status (optional -- only if the conservation module + cache loaded)
tbl$iucn <- if (exists("iucn_code_of")) iucn_code_of(tbl$species) else NA_character_
tbl$conservation <- if (exists("conservation_label")) conservation_label(tbl$species) else ""
HAVE_IUCN <- exists("iucn_cache_exists") && isTRUE(try(iucn_cache_exists(), silent = TRUE))

tbl <- tbl %>% select(species, common_name, net_records, photo_records, total_records,
                      coverage, peak_months, active_window, top_transects, top_flowers, top_flowers_html,
                      iucn, conservation, example_url)
write.csv(tbl %>% select(-top_flowers_html), file.path(OUT_DIR, "least_sampled_bees.csv"), row.names = FALSE)   # CSV keeps the plain flower list
message(sprintf("  coverage: %d both(thin), %d photo-only, %d specimen-only",
                sum(tbl$coverage == "both (thin)"), sum(tbl$coverage == "photo-only"),
                sum(tbl$coverage == "specimen-only")))

# scope caption (same "Scope | Method | Rank | Source" format as the figure captions), shown ABOVE the table
scope_str <- scope_cap(
  scope  = sprintf("least-sampled species only: < %d records total across both methods; all years, whole park", THIN_TOTAL),
  method = "lethal + non-lethal pooled", rank = "species", width = 10000)

# ---- 4. styled, sortable HTML table -----------------------------------------
esc <- function(x) { x <- gsub("&", "&amp;", x); x <- gsub("<", "&lt;", x); gsub(">", "&gt;", x) }
# iNaturalist-style mark (flying bird on the iNat green badge) for the "example observation" link
INAT_ICON <- '<img src="data:image/png;base64,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" width="16" height="14" alt="iNaturalist observation" style="vertical-align:-3px">'
cov_class <- function(s) ifelse(grepl("^both", s), "cb", ifelse(grepl("^photo", s), "cp", "cs"))
rows_html <- vapply(seq_len(nrow(tbl)), function(i) {
  r <- tbl[i, ]
  cs   <- if (has(r$conservation)) sprintf('<sup class="cs" title="%s">*</sup>', esc(r$conservation)) else ""
  iucn_td <- if (HAVE_IUCN) sprintf('<td class="num"><span class="iucn i-%s">%s</span></td>',
                                    tolower(ifelse(has(r$iucn), r$iucn, "ne")), esc(ifelse(has(r$iucn), r$iucn, "NE"))) else ""
  bee_td <- sprintf('<td class="bee"><i>%s</i>%s%s</td>', esc(r$species), cs,
                    if (has(r$common_name)) paste0('<span class="cn">', esc(r$common_name), '</span>') else "")
  fl <- if (has(r$example_url)) sprintf('%s <a href="%s" title="example iNaturalist observation">%s</a>', r$top_flowers_html, esc(r$example_url), INAT_ICON) else r$top_flowers_html
  cov_icon <- if (grepl("^photo", r$coverage))    ' <span class="vneed" title="specimen voucher needed -- photographed but never netted; go net one">&#128300;</span>'       # microscope = collect a voucher
         else if (grepl("^specimen", r$coverage)) ' <span class="vneed" title="photographs needed -- collected but never photographed; go photograph one">&#128247;</span>'  # camera = take more photos
         else ""
  sprintf(paste0('<tr>%s%s<td class="num">%d</td><td class="num">%d</td><td class="num">%d</td>',
                 '<td style="white-space:nowrap"><span class="pill %s">%s</span>%s</td><td>%s</td><td>%s</td><td class="loc">%s</td><td>%s</td></tr>'),
          bee_td, iucn_td, r$net_records, r$photo_records, r$total_records,
          cov_class(r$coverage), esc(r$coverage), cov_icon, esc(r$active_window), esc(r$peak_months),
          esc(r$top_transects), fl)
}, character(1))
iucn_th <- if (HAVE_IUCN) '<th class="num">IUCN</th>' else ""
html <- paste0(
'<!doctype html><html><head><meta charset="utf-8"><title>Cabrillo National Monument &mdash; Least-Sampled Native Bees</title><style>',
bee_table_css(),                                                                   # shared base table chrome (single source -- theme_beescabr.R)
bee_badge_css(BEE_COVERAGE_BG, BEE_COVERAGE_FG, function(k) paste0(".pill.", k)),   # coverage pills from theme tokens
'.vneed{font-size:12px;vertical-align:middle}',
bee_badge_css(BEE_IUCN_BG, BEE_IUCN_FG, function(k) paste0(".iucn.i-", k)),         # IUCN chips from theme tokens
'a{color:#3a6b8a;text-decoration:none}',
'</style></head><body>',
'<div class="org">Cabrillo National Monument</div>',
'<h1>Least-Sampled Native Bees</h1>',
'<div class="byline">by Brandi Sanchez</div>',
sprintf('<p class="sub">The %d bee species with fewer than %d records TOTAL across both methods &mdash; under-detected by netting AND iNaturalist. <b>Coverage</b>: <span class="pill cb">both (thin)</span> a few of each, <span class="pill cp">photo-only</span> never netted (needs a voucher), <span class="pill cs">specimen-only</span> never photographed. &#128247; = specimen-only, go photograph one; &#128300; = photo-only, go net one. When/where/flower are pooled across methods. %s links an example iNaturalist observation. Click a header to sort.</p>',
        nrow(tbl), THIN_TOTAL, INAT_ICON),
sprintf('<p class="scope">%s</p>', esc(scope_str)),
'<table id="t"><thead><tr><th>Bee</th>', iucn_th, '<th class="num">Net</th><th class="num">Photo</th><th class="num">Total</th>',
'<th>Coverage</th><th>Active window</th><th>Peak month</th><th>Where (transect)</th><th>Top flowers</th>',
'</tr></thead><tbody>', paste(rows_html, collapse = ""), '</tbody></table>',
'<script>',
'document.querySelectorAll("#t th").forEach(function(h,i){h.addEventListener("click",function(){',
'var t=h.closest("table"),b=t.tBodies[0],rows=[].slice.call(b.rows);h._d=!h._d;var d=h._d?1:-1;',
'rows.sort(function(x,y){var a=x.cells[i].innerText.trim(),c=y.cells[i].innerText.trim();',
'var na=parseFloat(a.replace(/[^0-9.\\-]/g,"")),nc=parseFloat(c.replace(/[^0-9.\\-]/g,""));',
'if(!isNaN(na)&&!isNaN(nc))return(na-nc)*d;return a.localeCompare(c)*d;});',
'rows.forEach(function(r){b.appendChild(r);});});});',
'</script></body></html>')
writeLines(html, file.path(OUT_DIR, "least_sampled_bees.html"))

# ---- 5. PNG table image (gridExtra) -----------------------------------------
if (requireNamespace("gridExtra", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE)) {
  disp <- tbl %>% transmute(
    Bee = ifelse(has(conservation), paste0(species, " *"), species),
    Net = net_records, Photo = photo_records, Total = total_records, Coverage = coverage,
    `Active` = active_window, `Peak month` = peak_months,
    `Where` = top_transects, `Top flowers` = top_flowers)
  if (HAVE_IUCN) { disp$IUCN <- ifelse(has(tbl$iucn), tbl$iucn, "NE"); disp <- dplyr::relocate(disp, IUCN, .after = Bee) }
  ff <- matrix("plain", nrow(disp), ncol(disp)); ff[, which(names(disp) == "Bee")] <- "italic"   # bee binomial column italic
  th <- gridExtra::ttheme_minimal(
    base_size = 7,
    core = list(fg_params = list(hjust = 0, x = 0.02, fontface = ff), bg_params = list(fill = c(BEE_TABLE[["row_odd"]], BEE_TABLE[["row_even"]]))),
    colhead = list(fg_params = list(hjust = 0, x = 0.02, fontface = "bold")))
  g   <- gridExtra::tableGrob(disp, rows = NULL, theme = th)
  scap <- grid::textGrob(paste(strwrap(scope_str, width = 150), collapse = "\n"),   # scope caption ABOVE the table
                        x = grid::unit(0.004, "npc"), hjust = 0, just = "left",
                        gp = grid::gpar(fontsize = 7.5, fontface = "bold", col = BEE_INK$secondary, lineheight = 1.15))
  cap <- grid::textGrob(sprintf("The %d least-sampled bees (< %d records total, both methods pooled). Coverage: both(thin)/photo-only/specimen-only. When/where/flower pooled across methods. * = IUCN threatened/near-threatened.  Source: iNaturalist + specimen vouchers, Cabrillo NM.",
                                nrow(disp), THIN_TOTAL),
                        x = grid::unit(0.004, "npc"), hjust = 0, just = "left",
                        gp = grid::gpar(fontsize = 7, col = BEE_TABLE[["subtext"]], lineheight = 1.15))
  g   <- gridExtra::arrangeGrob(scap, g, cap, ncol = 1,
                                heights = grid::unit.c(grid::unit(2.4, "lines"), grid::unit(1, "null"), grid::unit(2.2, "lines")))
  bee_ggsave(file.path(OUT_DIR, "least_sampled_bees.png"), g,
                  width = if (HAVE_IUCN) 15 else 14, height = 0.24 * nrow(disp) + 1.9, limitsize = FALSE, bg = "white")
} else message("  (gridExtra/ggplot2 not available -- skipped PNG; CSV + HTML written)")

message("Wrote least_sampled_bees.{csv,html,png} to ", OUT_DIR)
