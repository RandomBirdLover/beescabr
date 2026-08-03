# =============================================================
# analysis/bee_field_guide.R
# beescabr -- per-species FIELD-REFERENCE TABLE for the park's native bees.
#
# One row per bee species, answering (for surveyors + management):
#   * PEAK DAY      -- the single day activity centres on (circular mean of record dates)
#   * ACTIVE MONTHS -- the months that hold the bulk (5th-95th percentile) of records
#   * TOP FLOWERS   -- the 3-5 plant genera it is recorded on most
#   * DIET          -- specialist / moderate / generalist, by how many plant genera it uses
#   * WHERE TO FIND -- the transect(s) it favours, or (if mostly off-transect) a map
#                      centre point + buffer radius
#
# Species with < MIN_CONF records are flagged low-confidence (peak/season rest on a
# few points). Outputs a CSV, a styled sortable HTML table, and a PNG table image to
# data/analysis/reference/field_guide/.
#
# Run from the repo root:  Rscript scripts/analysis/bee_field_guide.R
# Depends on: dplyr, stringr (+ config.R). PNG needs gridExtra + ggplot2 (optional).
# =============================================================

suppressPackageStartupMessages({ library(dplyr); library(stringr) })
if (!exists("PATHS")) source("scripts/config.R")
if (!exists("iucn_table")) source("scripts/analysis/conservation_status.R")   # shared IUCN lookups
if (!exists("plant_label")) source("scripts/analysis/plant_names.R")          # shared plant common-name labels
if (!exists("forage_preference_label_species")) source("scripts/analysis/forage_selectivity.R")  # species-level forage preference
OUT_DIR       <- "data/analysis/reference/field_guide"
SPECIES_RANKS <- c("species", "subspecies")
MIN_CONF      <- 10          # < this many records -> low-confidence flag
RARE_CUT      <- 15          # < this many records -> "rare" (rarely recorded here)
UNCOMMON_CUT  <- 50          # < this -> "uncommon"; >= this -> "common"
TRANSECTS     <- c("BST", "UPMON", "TP", "OT")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- helpers ----------------------------------------------------------------
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

# ---- 1. pool records, keep species-level ------------------------------------
grab <- function(df) data.frame(
  taxon_rank  = tolower(str_squish(df$taxon_rank)),
  genus       = str_squish(df$genus),
  epithet     = tolower(word(str_squish(df$species), -1)),
  common      = str_squish(df$common_name),
  doy         = doy_of(df$observed_on),
  plant_genus = str_squish(df$plant_genus),
  transect    = toupper(str_squish(df$transect)),
  lat         = suppressWarnings(as.numeric(df$latitude)),
  lon         = suppressWarnings(as.numeric(df$longitude)),
  stringsAsFactors = FALSE)
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
rec  <- bind_rows(grab(spec), grab(inat)) %>%
  filter(taxon_rank %in% SPECIES_RANKS, genus != "", epithet != "") %>%
  mutate(species = paste(genus, epithet))

# ---- 2. one row per species --------------------------------------------------
diet_call <- function(n_gen, n_vis) {
  if (is.na(n_gen) || n_vis < 5) return("- (too few flower records)")
  pl <- sprintf("%d plant%s", n_gen, if (n_gen == 1) "" else "s")
  if (n_gen <= 3)      sprintf("Specialist (%s)", pl)
  else if (n_gen >= 8) sprintf("Generalist (%s)", pl)
  else                 sprintf("Moderate (%s)", pl)
}
status_call <- function(n) if (n < RARE_CUT) "rare" else if (n < UNCOMMON_CUT) "uncommon" else "common"
where_call <- function(d) {
  tr <- d$transect[d$transect %in% TRANSECTS]
  if (length(tr) >= 0.5 * nrow(d) && length(tr) > 0) {           # transect bee
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
mode_chr <- function(x) { x <- x[has(x)]; if (!length(x)) return("") ; names(sort(table(x), decreasing = TRUE))[1] }

sp_keys <- sort(unique(rec$species))
rows <- lapply(sp_keys, function(k) {
  d  <- rec[rec$species == k, ]
  pv <- d[has(d$plant_genus), ]
  fl <- sort(table(pv$plant_genus), decreasing = TRUE)
  peak <- circ_mean_doy(d$doy)
  data.frame(
    genus          = d$genus[1],
    bee            = k,
    common_name    = mode_chr(d$common),
    n_records      = nrow(d),
    confidence     = if (nrow(d) < MIN_CONF) "low (n<10)" else "ok",
    status         = status_call(nrow(d)),
    peak_day       = md_of(peak),
    peak_doy       = if (is.na(peak)) 999L else as.integer(round(peak)),   # hidden chronological sort key
    active_months  = active_months(d$doy),
    top_flowers    = if (length(fl)) paste(plant_label(head(names(fl), 5)), collapse = ", ") else "- (no flower records)",
    n_plant_genera = length(fl),
    diet           = diet_call(length(fl), nrow(pv)),
    where_to_find  = where_call(d),
    stringsAsFactors = FALSE)
})
tbl <- do.call(rbind, rows) %>% arrange(genus, bee)

# ---- IUCN Red List status from the shared conservation module (one source) ----
# conservation_status.R reads data/checklists/iucn/iucn_status.csv (written by
# refresh_iucn_status.R); the IUCN column shows when that cache exists.
HAVE_IUCN        <- iucn_cache_exists()
tbl$iucn         <- iucn_code_of(tbl$bee)
tbl$iucn_name    <- iucn_name_of(tbl$bee)
tbl$conservation <- conservation_label(tbl$bee)
# Forage preference -- availability-corrected (matched month/year/method test), SPECIES level.
# Populated for species with >= 20 plant-visit records; "too few records to judge" below that.
tbl$forage_pref  <- forage_preference_label_species(tbl$bee, plant_fmt = plant_label)
write.csv(tbl, file.path(OUT_DIR, "bee_field_guide_species.csv"), row.names = FALSE)
message(sprintf("Field guide: %d species (%d with >= %d records)",
                nrow(tbl), sum(tbl$confidence == "ok"), MIN_CONF))

# ---- 3. styled, sortable HTML table -----------------------------------------
esc <- function(x) { x <- gsub("&", "&amp;", x); x <- gsub("<", "&lt;", x); gsub(">", "&gt;", x) }
diet_class <- function(s) ifelse(grepl("^Special", s), "sp", ifelse(grepl("^General", s), "ge",
                          ifelse(grepl("^Moder", s), "mo", "na")))
pref_class <- function(s) ifelse(grepl("^Selective", s), "pref-sel", ifelse(grepl("^Generalist", s), "pref-gen", "pref-na"))
st_rank <- c(rare = 0L, uncommon = 1L, common = 2L)   # hidden sort key so Status sorts by abundance, not alphabetically
rows_html <- vapply(seq_len(nrow(tbl)), function(i) {
  r <- tbl[i, ]; low <- r$confidence != "ok"
  cs <- if (r$conservation != "") sprintf('<sup class="cs" title="%s">*</sup>', esc(r$conservation)) else ""
  iucn_td <- if (HAVE_IUCN) sprintf('<td class="num"><span class="iucn i-%s" title="%s">%s</span></td>',
                                    tolower(r$iucn), esc(r$iucn_name), esc(r$iucn)) else ""
  sprintf(paste0('<tr class="%s"><td class="bee"><i>%s</i>%s%s</td><td class="num">%d</td>%s',
                 '<td data-sort="%d">%s</td><td>%s</td><td>%s</td><td><span class="pill %s">%s</span></td>',
                 '<td><span class="pill %s">%s</span></td><td class="loc">%s</td>',
                 '<td data-sort="%d"><span class="pill st-%s">%s</span></td></tr>'),
          if (low) "low" else "", esc(r$bee), cs,
          if (has(r$common_name)) paste0('<span class="cn">', esc(r$common_name), '</span>') else "",
          r$n_records, iucn_td, r$peak_doy, esc(r$peak_day), esc(r$active_months), esc(r$top_flowers),
          diet_class(r$diet), esc(r$diet), pref_class(r$forage_pref), esc(r$forage_pref),
          esc(r$where_to_find), unname(st_rank[r$status]), r$status, r$status)
}, character(1))
iucn_th  <- if (HAVE_IUCN) '<th class="num">IUCN</th>' else ""
note_txt <- if (HAVE_IUCN) {
  "IUCN = current IUCN Red List category: CR/EN/VU = threatened, NT = near threatened, LC = least concern, DD = data deficient, NE = not evaluated (most solitary bees). * marks threatened/near-threatened species. Source: IUCN Red List API v4."
} else "* IUCN threatened / near-threatened species, from the last IUCN Red List pull (data/checklists/iucn/iucn_status.csv). Run refresh_iucn_status.R to populate the full IUCN column."
# Records/Status caveat -- this guide pools ALL data (no survey-only filter), so those two
# columns reflect detection/photo effort, not a survey-controlled abundance estimate.
status_note <- "Records and Status count ALL data -- specimen nets plus every iNaturalist photo, including casual public sightings, across all years -- so they show how often a species is DETECTED/photographed here, not a survey-controlled abundance. A showy bee near a busy trail can read 'common' on public photos alone; treat rare/uncommon/common as recording frequency, not true density."
note_txt <- paste(status_note, note_txt)
html <- paste0(
'<!doctype html><html><head><meta charset="utf-8"><title>CABR Native Bee Field Guide - by species</title>',
'<style>',
'body{font:14px/1.45 -apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;color:#1a1a1a;margin:24px;background:#fcfcfb}',
'h1{font-size:20px;margin:0 0 2px}p.sub{color:#6b6a66;margin:0 0 16px;font-size:13px}',
'p.note{color:#6b6a66;margin:12px 0 0;font-size:12px}',
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
'.pill.pref-sel{background:#dceee0;color:#1f6b46}.pill.pref-gen{background:#e9e7e0;color:#5a5850}.pill.pref-na{background:#f1f1f1;color:#999}',
'.iucn{display:inline-block;padding:2px 7px;border-radius:4px;font-size:11px;font-weight:700}',
'.iucn.i-en,.iucn.i-cr{background:#f3d2cc;color:#8a1c1c}.iucn.i-vu{background:#f6dcc6;color:#8a4a12}',
'.iucn.i-nt{background:#efe7cf;color:#6b5a20}.iucn.i-lc{background:#dfeae0;color:#2f6b46}',
'.iucn.i-dd,.iucn.i-ne{background:#efefef;color:#98968f}',
'</style></head><body>',
'<h1>CABR native bee field guide - by species</h1>',
'<p class="sub">One row per bee species. Peak day = circular mean of record dates; active months = 5th-95th percentile; diet = number of plant genera used; where = favoured transect(s) or an off-transect centre + buffer; status = how often the species is recorded here (rare/uncommon/common &mdash; counts all data incl. casual photos, so it is recording frequency, not true abundance). Rows in grey have &lt;10 records (peak/season are rough). Click a column header to sort.</p>',
'<p class="sub"><b>Most-recorded flowers = the plants this species was seen on most often</b>, which reflects how much each plant was blooming and sampled as much as any true preference &mdash; read it as &quot;where it was seen,&quot; not proof of what it likes best. <b>Forage preference</b> is the availability-corrected verdict (a matched month/year/method test vs the rest of the community, the same one the by-genus guide uses): &quot;Selective &rarr; plant (N&times;)&quot; = visits it well beyond availability; &quot;Generalist&quot; = visits ~ what&#39;s around; &quot;too few records to judge&quot; below 20 plant-visit records (most species &mdash; it fills in as sampling grows).</p>',
'<table id="t"><thead><tr>',
'<th>Bee</th><th class="num">Records</th>', iucn_th, '<th>Peak day</th><th>Active months</th><th>Most-recorded flowers</th><th>Diet</th><th>Forage preference</th><th>Where to find</th><th>Status</th>',
'</tr></thead><tbody>', paste(rows_html, collapse = ""), '</tbody></table>',
'<p class="note">', esc(note_txt), '</p>',
'<script>',
'(function(){var T=document.getElementById("t"),B=T.tBodies[0],ROWS=[].slice.call(B.rows),NC=T.tHead.rows[0].cells.length;',
'function pv(r,c){var x=r.cells[c];if(!x)return"";var s=x.getAttribute("data-sort");return s!==null?s:x.innerText.trim();}',
'function isnum(s){return s!==""&&/^-?[0-9,]+(\\.[0-9]+)?%?$/.test(s);}',
'var NUM=[];for(var c=0;c<NC;c++){var all=true,any=false;for(var r=0;r<ROWS.length;r++){var v=pv(ROWS[r],c);if(v==="")continue;any=true;if(!isnum(v)){all=false;break;}}NUM[c]=any&&all;}',
'var CC=-1,CD=1;[].forEach.call(T.tHead.rows[0].cells,function(h,i){h.style.cursor="pointer";h.addEventListener("click",function(){',
'if(CC===i){CD=-CD;}else{CC=i;CD=1;}var d=CD;',
'ROWS.sort(function(x,y){var a=pv(x,i),c=pv(y,i);',
'if(NUM[i])return(parseFloat(a.replace(/[^0-9.\\-]/g,""))-parseFloat(c.replace(/[^0-9.\\-]/g,"")))*d;',
'return a.localeCompare(c)*d;});',
'ROWS.forEach(function(r){B.appendChild(r);});});});})();',
'</script></body></html>')
writeLines(html, file.path(OUT_DIR, "bee_field_guide_species.html"))

# ---- 4. PNG table image (gridExtra) -----------------------------------------
if (requireNamespace("gridExtra", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE)) {
  pref_short <- sub(" \\(.*$", "", tbl$forage_pref)   # drop the "(Nx vs available)" tail for the compact image
  disp <- tbl %>% transmute(Bee = ifelse(conservation != "", paste0(bee, " *"), bee),
                            N = n_records, `Peak day` = peak_day,
                            `Active months` = active_months, `Most-recorded flowers` = top_flowers,
                            Diet = diet, `Forage preference` = pref_short,
                            `Where to find` = where_to_find, Status = status)
  if (HAVE_IUCN) disp <- dplyr::relocate(dplyr::mutate(disp, IUCN = tbl$iucn), IUCN, .after = N)
  th <- gridExtra::ttheme_minimal(
    base_size = 7,
    core = list(fg_params = list(hjust = 0, x = 0.02), bg_params = list(fill = c("#ffffff", "#f6f5f2"))),
    colhead = list(fg_params = list(hjust = 0, x = 0.02, fontface = "bold")))
  g   <- gridExtra::tableGrob(disp, rows = NULL, theme = th)
  cap <- grid::textGrob(paste(strwrap(note_txt, width = 150), collapse = "\n"),
                        x = grid::unit(0.004, "npc"), hjust = 0, just = "left",
                        gp = grid::gpar(fontsize = 7, col = "#666666", lineheight = 1.15))
  g   <- gridExtra::arrangeGrob(g, cap, ncol = 1,
                                heights = grid::unit.c(grid::unit(1, "null"), grid::unit(2.6, "lines")))
  ggplot2::ggsave(file.path(OUT_DIR, "bee_field_guide_species.png"), g,
                  width = if (HAVE_IUCN) 18.5 else 17.5, height = 0.24 * nrow(disp) + 1.5, dpi = 200, limitsize = FALSE, bg = "white")
} else message("  (gridExtra/ggplot2 not available -- skipped PNG; CSV + HTML written)")

message("Wrote bee_field_guide_species.{csv,html,png} to ", OUT_DIR)
