# =============================================================
# analysis/bee_field_guide_genus.R
# beescabr -- GENUS-LEVEL companion to the species field guide.
#
# The species field guide (bee_field_guide.R) keeps only records identified to
# SPECIES, which silently drops ~5,300 records that stop at genus / subgenus /
# complex level -- and those are concentrated in exactly the hyperdiverse, hard-to-
# ID genera (Lasioglossum, Ceratina, Colletes, Melissodes, Andrena, Perdita...).
# For habitat / planting decisions we do not want that diversity to vanish, so this
# guide rolls EVERY record up to its GENUS and reports the same field notes:
#
#   * RECORDS       -- all records of the genus (specimen net + iNat, every ID rank)
#   * SPECIES ID'd   -- how many distinct species we HAVE pinned in the genus (0 = none)
#   * PEAK DAY      -- circular mean of record dates
#   * ACTIVE MONTHS -- 5th-95th percentile of record dates
#   * TOP FLOWERS   -- the plant genera the genus is recorded on most (what to plant)
#   * DIET          -- specialist / moderate / generalist, by plant-genus breadth
#   * WHERE TO FIND -- favoured transect(s) or an off-transect centre + buffer
#   * STATUS        -- how often the genus is recorded here (rare / uncommon / common)
#
# Together with the species guide this covers EVERY bee record once (species guide =
# pinned records; this = the whole genus). Outputs a CSV, a styled sortable HTML
# table, and a PNG table image to data/analysis/field_guide/.
#
# Run from the repo root:  Rscript scripts/analysis/bee_field_guide_genus.R
# Depends on: dplyr, stringr (+ config.R). PNG needs gridExtra + ggplot2 (optional).
# =============================================================

suppressPackageStartupMessages({ library(dplyr); library(stringr) })
if (!exists("PATHS")) source("scripts/config.R")
OUT_DIR       <- "data/analysis/field_guide"
SPECIES_RANKS <- c("species", "subspecies")
RARE_CUT      <- 15          # < this many records -> "rare" (rarely recorded here)
UNCOMMON_CUT  <- 50          # < this -> "uncommon"; >= this -> "common"
TRANSECTS     <- c("BST", "UPMON", "TP", "OT")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- helpers (shared with the species guide) --------------------------------
doy_of  <- function(x) suppressWarnings(as.integer(format(as.Date(x), "%j")))
has     <- function(x) !is.na(x) & x != ""
circ_mean_doy <- function(d) { d <- d[!is.na(d)]; if (!length(d)) return(NA_real_)
  a <- d / 365 * 2 * pi; (atan2(mean(sin(a)), mean(cos(a))) %% (2 * pi)) / (2 * pi) * 365 }
md_of    <- function(doy) if (is.na(doy)) NA else trimws(format(as.Date(round(doy) - 1, origin = "2023-01-01"), "%b %e"))
month_of <- function(doy) if (is.na(doy)) NA else format(as.Date(round(doy) - 1, origin = "2023-01-01"), "%b")
active_months <- function(d) { d <- d[!is.na(d)]
  if (length(d) < 3) return(month_of(circ_mean_doy(d)))
  q <- stats::quantile(d, c(0.05, 0.95), type = 7, names = FALSE)
  m1 <- month_of(q[1]); m2 <- month_of(q[2]); if (m1 == m2) m1 else paste0(m1, "-", m2) }
hav <- function(la1, lo1, la2, lo2) { R <- 6371000; k <- pi / 180
  a <- sin((la2 - la1) * k / 2)^2 + cos(la1 * k) * cos(la2 * k) * sin((lo2 - lo1) * k / 2)^2
  2 * R * asin(pmin(1, sqrt(a))) }

# ---- 1. pool records, keep anything carrying a genus (any ID rank) -----------
grab <- function(df) data.frame(
  taxon_rank  = tolower(str_squish(df$taxon_rank)),
  genus       = str_squish(df$genus),
  epithet     = tolower(word(str_squish(df$species), -1)),
  doy         = doy_of(df$observed_on),
  plant_genus = str_squish(df$plant_genus),
  transect    = toupper(str_squish(df$transect)),
  lat         = suppressWarnings(as.numeric(df$latitude)),
  lon         = suppressWarnings(as.numeric(df$longitude)),
  stringsAsFactors = FALSE)
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
rec  <- bind_rows(grab(spec), grab(inat)) %>%
  filter(genus != "") %>%
  mutate(is_species = taxon_rank %in% SPECIES_RANKS & epithet != "",
         species    = ifelse(is_species, paste(genus, epithet), NA_character_))

# ---- 2. one row per GENUS ----------------------------------------------------
breadth_call <- function(n_gen, n_vis) {   # how many plant genera the genus is recorded on
  if (is.na(n_gen) || n_vis < 5) return("- (too few flower records)")
  tier <- if (n_gen <= 3) "Narrow" else if (n_gen <= 7) "Moderate" else if (n_gen <= 24) "Broad" else "Very broad"
  sprintf("%s (%d)", tier, n_gen)
}
status_call <- function(n) if (n < RARE_CUT) "rare" else if (n < UNCOMMON_CUT) "uncommon" else "common"
# genera that include an at-risk species (IUCN Red List); spelled out in the legend below the table
CONSERV_SPP    <- c("Bombus crotchii" = "Endangered", "Bombus sonorus" = "Vulnerable",
                    "Bombus californicus" = "Vulnerable")
CONSERV_GENERA <- unique(word(names(CONSERV_SPP), 1))
CONSERV_LEGEND <- "* genus includes an at-risk species (IUCN Red List) -- Bombus crotchii: Endangered (also a California Endangered Species Act candidate); Bombus sonorus and Bombus californicus: Vulnerable."
where_call <- function(d) {
  tr <- d$transect[d$transect %in% TRANSECTS]
  if (length(tr) >= 0.5 * nrow(d) && length(tr) > 0) {           # transect genus
    tt <- sort(table(tr), decreasing = TRUE); tt <- tt[tt / sum(tt) >= 0.15]
    paste(sprintf("%s (%d%%)", names(tt), round(100 * as.integer(tt) / length(tr))), collapse = ", ")
  } else {                                                       # off-transect -> centre + buffer
    ok <- !is.na(d$lat) & !is.na(d$lon)
    if (!any(ok)) return("off-transect (no coordinates)")
    cla <- median(d$lat[ok]); clo <- median(d$lon[ok])
    buf <- ceiling(stats::quantile(hav(cla, clo, d$lat[ok], d$lon[ok]), 0.8, names = FALSE) / 50) * 50
    sprintf("off-transect - centre %.4f, %.4f (+/-%dm)", cla, clo, as.integer(buf))
  }
}

gen_keys <- sort(unique(rec$genus))
rows <- lapply(gen_keys, function(k) {
  d  <- rec[rec$genus == k, ]
  pv <- d[has(d$plant_genus), ]
  fl <- sort(table(pv$plant_genus), decreasing = TRUE)
  peak <- circ_mean_doy(d$doy)
  top_plant <- if (length(fl) && nrow(pv) >= 5)
    sprintf("%s (%d%%)", names(fl)[1], round(100 * as.integer(fl[1]) / nrow(pv))) else "-"
  data.frame(
    genus          = k,
    n_records      = nrow(d),
    n_species      = length(unique(na.omit(d$species))),
    status         = status_call(nrow(d)),
    conservation   = if (k %in% CONSERV_GENERA) paste(names(CONSERV_SPP)[word(names(CONSERV_SPP), 1) == k], collapse = "; ") else "",
    peak_day       = md_of(peak),
    active_months  = active_months(d$doy),
    top_flowers    = if (length(fl)) paste(head(names(fl), 5), collapse = ", ") else "- (no flower records)",
    n_plant_genera = length(fl),
    flower_breadth = breadth_call(length(fl), nrow(pv)),
    top_plant      = top_plant,
    where_to_find  = where_call(d),
    stringsAsFactors = FALSE)
})
tbl <- do.call(rbind, rows) %>% arrange(desc(n_records))
write.csv(tbl, file.path(OUT_DIR, "bee_field_guide_genus.csv"), row.names = FALSE)
message(sprintf("Genus field guide: %d genera (%d never yet ID'd to species; %d rare, %d uncommon, %d common)",
                nrow(tbl), sum(tbl$n_species == 0),
                sum(tbl$status == "rare"), sum(tbl$status == "uncommon"), sum(tbl$status == "common")))

# ---- 3. styled, sortable HTML table -----------------------------------------
esc <- function(x) { x <- gsub("&", "&amp;", x); x <- gsub("<", "&lt;", x); gsub(">", "&gt;", x) }
rows_html <- vapply(seq_len(nrow(tbl)), function(i) {
  r <- tbl[i, ]; low <- r$status == "rare"
  tag <- if (r$n_species == 0) '<span class="cn">not yet ID&#39;d to species</span>' else ""
  cs  <- if (r$conservation != "") sprintf('<sup class="cs" title="includes at-risk: %s">*</sup>', esc(r$conservation)) else ""
  sprintf(paste0('<tr class="%s"><td class="bee"><i>%s</i>%s%s</td><td class="num">%d</td><td class="num">%d</td>',
                 '<td>%s</td><td>%s</td><td>%s</td><td>%s</td><td class="loc">%s</td>',
                 '<td class="loc">%s</td><td><span class="pill st-%s">%s</span></td></tr>'),
          if (low) "low" else "", esc(r$genus), cs, tag, r$n_records, r$n_species,
          esc(r$peak_day), esc(r$active_months), esc(r$top_flowers),
          esc(r$flower_breadth), esc(r$top_plant), esc(r$where_to_find), r$status, r$status)
}, character(1))
html <- paste0(
'<!doctype html><html><head><meta charset="utf-8"><title>CABR Native Bee Field Guide - by genus</title>',
'<style>',
'body{font:14px/1.45 -apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;color:#1a1a1a;margin:24px;background:#fcfcfb}',
'h1{font-size:20px;margin:0 0 2px}p.sub{color:#6b6a66;margin:0 0 16px;font-size:13px;max-width:1100px}',
'p.note{color:#6b6a66;margin:12px 0 0;font-size:12px;max-width:1100px}',
'sup.cs{color:#8a1c1c;font-weight:700;margin-left:1px}',
'table{border-collapse:collapse;width:100%;font-size:13px}',
'th,td{text-align:left;padding:7px 10px;border-bottom:1px solid #eee;vertical-align:top}',
'th{position:sticky;top:0;background:#f3f1ec;cursor:pointer;font-weight:600;white-space:nowrap;border-bottom:2px solid #ddd}',
'th:hover{background:#e8e5de}tr:hover{background:#f7f6f2}',
'td.bee i{color:#111}td .cn{display:block;color:#8a8880;font-size:11px}',
'td.num{text-align:right;font-variant-numeric:tabular-nums}td.loc{color:#52514e;font-size:12px}',
'tr.low{color:#a09e98}tr.low td.bee i{color:#a09e98}',
'.pill{display:inline-block;padding:2px 8px;border-radius:10px;font-size:11px;font-weight:600;white-space:nowrap}',
'.pill.sp{background:#f0dcc8;color:#7a4a1e}.pill.ge{background:#cfe6e2;color:#0e5a52}',
'.pill.mo{background:#e9e7e0;color:#5a5850}.pill.na{background:#f1f1f1;color:#999}',
'.pill.st-rare{background:#efdcd2;color:#8a3d1e}.pill.st-uncommon{background:#efe9dc;color:#6b5a2e}',
'.pill.st-common{background:#dcebe0;color:#2f6b46}',
'</style></head><body>',
'<h1>CABR native bee field guide - by genus</h1>',
'<p class="sub">Companion to the species guide: one row per GENUS, pooling every record of that genus (specimen net + iNaturalist, all ID ranks) so the hard-to-ID diverse genera keep their flower associations for planting. &quot;Species ID&#39;d&quot; = distinct species we have pinned in the genus (0 = none yet). Peak day = circular mean of record dates; active months = 5th-95th percentile; flower breadth = how many plant genera the genus uses (Narrow 1-3 / Moderate 4-7 / Broad 8-24 / Very broad 25+); top plant = its single most-used plant and that plant&#39;s share of the genus&#39;s flower visits; where = favoured transect(s) or an off-transect centre + buffer; status = how often the genus is recorded here. Rows in grey are rarely recorded (peak/season are rough). Click a column header to sort.</p>',
'<table id="t"><thead><tr>',
'<th>Genus</th><th class="num">Records</th><th class="num">Species ID&#39;d</th><th>Peak day</th><th>Active months</th><th>Top flowers</th><th>Flower breadth</th><th>Top plant</th><th>Where to find</th><th>Status</th>',
'</tr></thead><tbody>', paste(rows_html, collapse = ""), '</tbody></table>',
'<p class="note">', esc(CONSERV_LEGEND), '</p>',
'<script>',
'document.querySelectorAll("#t th").forEach(function(h,i){h.addEventListener("click",function(){',
'var t=h.closest("table"),b=t.tBodies[0],rows=[].slice.call(b.rows);',
'h._d=!h._d;var d=h._d?1:-1;',
'rows.sort(function(x,y){var a=x.cells[i].innerText.trim(),c=y.cells[i].innerText.trim();',
'var na=parseFloat(a.replace(/[^0-9.\\-]/g,"")),nc=parseFloat(c.replace(/[^0-9.\\-]/g,""));',
'if(!isNaN(na)&&!isNaN(nc))return(na-nc)*d;return a.localeCompare(c)*d;});',
'rows.forEach(function(r){b.appendChild(r);});});});',
'</script></body></html>')
writeLines(html, file.path(OUT_DIR, "bee_field_guide_genus.html"))

# ---- 4. PNG table image (gridExtra) -----------------------------------------
if (requireNamespace("gridExtra", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE)) {
  disp <- tbl %>% transmute(Genus = ifelse(conservation != "", paste0(genus, " *"), genus),
                            N = n_records, `Species ID'd` = n_species,
                            `Peak day` = peak_day, `Active months` = active_months,
                            `Top flowers` = top_flowers, `Flower breadth` = flower_breadth,
                            `Top plant` = top_plant,
                            `Where to find` = where_to_find, Status = status)
  th <- gridExtra::ttheme_minimal(
    base_size = 7,
    core = list(fg_params = list(hjust = 0, x = 0.02), bg_params = list(fill = c("#ffffff", "#f6f5f2"))),
    colhead = list(fg_params = list(hjust = 0, x = 0.02, fontface = "bold")))
  g   <- gridExtra::tableGrob(disp, rows = NULL, theme = th)
  cap <- grid::textGrob(paste(strwrap(CONSERV_LEGEND, width = 170), collapse = "\n"),
                        x = grid::unit(0.004, "npc"), hjust = 0, just = "left",
                        gp = grid::gpar(fontsize = 7, col = "#666666", lineheight = 1.15))
  g   <- gridExtra::arrangeGrob(g, cap, ncol = 1,
                                heights = grid::unit.c(grid::unit(1, "null"), grid::unit(2.2, "lines")))
  ggplot2::ggsave(file.path(OUT_DIR, "bee_field_guide_genus.png"), g,
                  width = 17, height = 0.26 * nrow(disp) + 1.4, dpi = 200, limitsize = FALSE, bg = "white")
} else message("  (gridExtra/ggplot2 not available -- skipped PNG; CSV + HTML written)")

message("Wrote bee_field_guide_genus.{csv,html,png} to ", OUT_DIR)
