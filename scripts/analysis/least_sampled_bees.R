# =============================================================
# analysis/least_sampled_bees.R
# beescabr -- the park's LEAST-SAMPLED native bees, in ONE go-find-it sheet.
#
# THE QUESTION: which bee species are barely sampled -- thin in specimens AND thin in
# photos -- and when / where / on what flower should a surveyor go look for them?
#
# "Least sampled" = fewer than THIN_TOTAL records TOTAL across BOTH methods (the
# project's <10 thin-evidence rule). Because the total is small, each method is small
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
  tb <- sort(table(m), decreasing = TRUE); paste(month.abb[as.integer(names(tb)[seq_len(min(2, length(tb)))])], collapse = ", ") }
span_mo <- function(m) { m <- m[!is.na(m)]; if (!length(m)) return("-")
  r <- range(m); if (r[1] == r[2]) month.abb[r[1]] else paste0(month.abb[r[1]], "-", month.abb[r[2]]) }
mode_chr <- function(x) { x <- x[has(x)]; if (!length(x)) return("") ; names(sort(table(x), decreasing = TRUE))[1] }

ctx <- lapply(split_tbl$species, function(k) {
  d  <- rec[rec$species == k, ]
  fl <- d$plant_genus[has(d$plant_genus)]
  u  <- d$url[has(d$url)]
  data.frame(
    species       = k,
    common_name   = mode_chr(d$common),
    peak_months   = peak_mo(d$month),
    active_window = span_mo(d$month),
    top_transects = top2(d$transect),
    top_flowers   = if (length(fl)) paste(plant_label(names(sort(table(fl), decreasing = TRUE))[seq_len(min(3, length(unique(fl))))]), collapse = ", ") else "- (no flower records)",
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
                      coverage, peak_months, active_window, top_transects, top_flowers,
                      iucn, conservation, example_url)
write.csv(tbl, file.path(OUT_DIR, "least_sampled_bees.csv"), row.names = FALSE)
message(sprintf("  coverage: %d both(thin), %d photo-only, %d specimen-only",
                sum(tbl$coverage == "both (thin)"), sum(tbl$coverage == "photo-only"),
                sum(tbl$coverage == "specimen-only")))

# ---- 4. styled, sortable HTML table -----------------------------------------
esc <- function(x) { x <- gsub("&", "&amp;", x); x <- gsub("<", "&lt;", x); gsub(">", "&gt;", x) }
cov_class <- function(s) ifelse(grepl("^both", s), "cb", ifelse(grepl("^photo", s), "cp", "cs"))
rows_html <- vapply(seq_len(nrow(tbl)), function(i) {
  r <- tbl[i, ]
  cs   <- if (has(r$conservation)) sprintf('<sup class="cs" title="%s">*</sup>', esc(r$conservation)) else ""
  iucn_td <- if (HAVE_IUCN) sprintf('<td class="num"><span class="iucn i-%s">%s</span></td>',
                                    tolower(ifelse(has(r$iucn), r$iucn, "ne")), esc(ifelse(has(r$iucn), r$iucn, "NE"))) else ""
  bee_td <- sprintf('<td class="bee"><i>%s</i>%s%s</td>', esc(r$species), cs,
                    if (has(r$common_name)) paste0('<span class="cn">', esc(r$common_name), '</span>') else "")
  fl <- if (has(r$example_url)) sprintf('%s <a href="%s" title="example photo">&#128247;</a>', esc(r$top_flowers), esc(r$example_url)) else esc(r$top_flowers)
  sprintf(paste0('<tr>%s<td class="num">%d</td><td class="num">%d</td><td class="num">%d</td>',
                 '<td><span class="pill %s">%s</span></td><td>%s</td><td>%s</td><td class="loc">%s</td><td>%s</td>%s</tr>'),
          bee_td, r$net_records, r$photo_records, r$total_records,
          cov_class(r$coverage), esc(r$coverage), esc(r$peak_months), esc(r$active_window),
          esc(r$top_transects), fl, iucn_td)
}, character(1))
iucn_th <- if (HAVE_IUCN) '<th class="num">IUCN</th>' else ""
html <- paste0(
'<!doctype html><html><head><meta charset="utf-8"><title>CABR Least-Sampled Bees</title><style>',
'body{font:14px/1.45 -apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;color:#1a1a1a;margin:24px;background:#fcfcfb}',
'h1{font-size:20px;margin:0 0 2px}p.sub{color:#6b6a66;margin:0 0 14px;font-size:13px}',
'table{border-collapse:collapse;width:100%;font-size:13px}',
'th,td{text-align:left;padding:7px 10px;border-bottom:1px solid #eee;vertical-align:top}',
'th{position:sticky;top:0;background:#f3f1ec;cursor:pointer;font-weight:600;white-space:nowrap;border-bottom:2px solid #ddd}',
'th:hover{background:#e8e5de}tr:hover{background:#f7f6f2}',
'td.bee i{color:#111}td .cn{display:block;color:#8a8880;font-size:11px}sup.cs{color:#8a1c1c;font-weight:700}',
'td.num{text-align:right;font-variant-numeric:tabular-nums}td.loc{color:#52514e;font-size:12px}',
'.pill{display:inline-block;padding:2px 8px;border-radius:10px;font-size:11px;font-weight:600;white-space:nowrap}',
'.pill.cb{background:#e9e7e0;color:#5a5850}.pill.cp{background:#e5dcef;color:#5b3b8a}.pill.cs{background:#f0dcc8;color:#7a4a1e}',
'.iucn{display:inline-block;padding:2px 7px;border-radius:4px;font-size:11px;font-weight:700}',
'.iucn.i-en,.iucn.i-cr{background:#f3d2cc;color:#8a1c1c}.iucn.i-vu{background:#f6dcc6;color:#8a4a12}',
'.iucn.i-nt{background:#efe7cf;color:#6b5a20}.iucn.i-lc{background:#dfeae0;color:#2f6b46}',
'.iucn.i-dd,.iucn.i-ne{background:#efefef;color:#98968f}a{color:#3a6b8a;text-decoration:none}',
'</style></head><body>',
'<h1>CABR least-sampled native bees &mdash; go-find-it sheet</h1>',
sprintf('<p class="sub">The %d bee species with fewer than %d records TOTAL across both methods &mdash; under-detected by netting AND by iNaturalist. <b>Coverage</b> shows the split: <span class="pill cb">both (thin)</span> a few of each, <span class="pill cp">photo-only</span> never netted (needs a voucher), <span class="pill cs">specimen-only</span> never photographed. When / where / flower are pooled across both methods (peak months by record count; active window = month span seen; where = top transect(s); flower = most-recorded plant genera). &#128247; links an example iNaturalist photo. Click a header to sort.</p>',
        nrow(tbl), THIN_TOTAL),
'<table id="t"><thead><tr><th>Bee</th><th class="num">Net</th><th class="num">Photo</th><th class="num">Total</th>',
'<th>Coverage</th><th>Peak months</th><th>Active window</th><th>Where (transect)</th><th>Top flowers</th>', iucn_th,
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
    `Peak months` = peak_months, `Active` = active_window,
    `Where` = top_transects, `Top flowers` = top_flowers)
  if (HAVE_IUCN) disp$IUCN <- ifelse(has(tbl$iucn), tbl$iucn, "NE")
  th <- gridExtra::ttheme_minimal(
    base_size = 7,
    core = list(fg_params = list(hjust = 0, x = 0.02), bg_params = list(fill = c("#ffffff", "#f6f5f2"))),
    colhead = list(fg_params = list(hjust = 0, x = 0.02, fontface = "bold")))
  g   <- gridExtra::tableGrob(disp, rows = NULL, theme = th)
  cap <- grid::textGrob(sprintf("The %d least-sampled bees (< %d records total, both methods pooled). Coverage: both(thin)/photo-only/specimen-only. When/where/flower pooled across methods. * = IUCN threatened/near-threatened.  Source: iNaturalist + specimen vouchers, Cabrillo NM.",
                                nrow(disp), THIN_TOTAL),
                        x = grid::unit(0.004, "npc"), hjust = 0, just = "left",
                        gp = grid::gpar(fontsize = 7, col = "#666666", lineheight = 1.15))
  g   <- gridExtra::arrangeGrob(g, cap, ncol = 1,
                                heights = grid::unit.c(grid::unit(1, "null"), grid::unit(2.2, "lines")))
  ggplot2::ggsave(file.path(OUT_DIR, "least_sampled_bees.png"), g,
                  width = if (HAVE_IUCN) 15 else 14, height = 0.24 * nrow(disp) + 1.4, dpi = 200, limitsize = FALSE, bg = "white")
} else message("  (gridExtra/ggplot2 not available -- skipped PNG; CSV + HTML written)")

message("Wrote least_sampled_bees.{csv,html,png} to ", OUT_DIR)
