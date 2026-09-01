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
#   * WHERE TO FIND -- favored transect(s) or an off-transect centre + buffer
#   * STATUS        -- how often the genus is recorded here (rare / uncommon / common)
#
# Together with the species guide this covers EVERY bee record once (species guide =
# pinned records; this = the whole genus). Outputs a CSV, a styled sortable HTML
# table, and a PNG table image to data/analysis/reference/field_guide/.
#
# Run from the repo root:  Rscript scripts/analysis/bee_field_guide_genus.R
# Depends on: dplyr, stringr (+ config.R). PNG needs gridExtra + ggplot2 (optional).
# =============================================================

suppressPackageStartupMessages({ library(dplyr); library(stringr) })
if (!exists("PATHS")) source("scripts/config.R")
if (!exists("iucn_table")) source("scripts/analysis/conservation_status.R")   # shared IUCN lookups
if (!exists("plant_label")) source("scripts/analysis/plant_names.R")          # shared plant common-name labels
if (!exists("forage_preference_label")) source("scripts/analysis/forage_selectivity.R")  # shared selectivity (likes vs available)
if (!exists("scope_cap")) source("scripts/analysis/theme_beescabr.R")                     # shared scope-caption format
if (!exists("inat_photo_link")) source("scripts/analysis/inat_taxon_links.R")             # iNat logo -> taxon photo page
OUT_DIR       <- file.path(DIR_REPORT, "reference/field_guide")
SPECIES_RANKS <- c("species", "subspecies")
RARE_CUT      <- 15          # < this many records -> "rare" (rarely recorded here)
UNCOMMON_CUT  <- 50          # < this -> "uncommon"; >= this -> "common"
CLAIM_MIN     <- 50          # < this many records -> DON'T claim an interpretive column (flower breadth); reads "not enough records"
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
  taxon_id    = suppressWarnings(as.integer(df$taxon_id)),   # carried through, never re-derived from a name
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
# genera that include an IUCN at-risk species come straight from the shared module (the cache)
CONSERV_FLAGGED <- flagged_species()                                  # CR/EN/VU/NT, live from the cache
CONSERV_GENERA  <- unique(word(CONSERV_FLAGGED$scientific_name, 1))
CONSERV_LEGEND  <- "A star by a genus marks one that includes a species listed as threatened or near threatened on the IUCN Red List, based on the current Red List."
where_call <- function(d) {
  tr <- d$transect[d$transect %in% TRANSECTS]
  if (length(tr) >= 0.5 * nrow(d) && length(tr) > 0) {           # transect genus
    tt <- sort(table(tr), decreasing = TRUE); tt <- tt[tt / sum(tt) >= 0.15]
    paste(sprintf("%s (%d%%)", names(tt), round(100 * as.integer(tt) / length(tr))), collapse = ", ")
  } else ""                                                      # off-transect: no location shown here (see the occurrence map for pins)
}

gen_keys <- sort(unique(rec$genus))
rows <- lapply(gen_keys, function(k) {
  d  <- rec[rec$genus == k, ]
  pv <- d[has(d$plant_genus), ]
  fl <- sort(table(pv$plant_genus), decreasing = TRUE)
  peak <- circ_mean_doy(d$doy)
  top_plant <- if (length(fl) && nrow(pv) >= 5)
    sprintf("%s, %d%%", plant_label(names(fl)[1]), round(100 * as.integer(fl[1]) / nrow(pv))) else "-"
  top_plant_html <- if (length(fl) && nrow(pv) >= 5)
    sprintf("%s, %d%%", plant_label(names(fl)[1], sci_wrap = "<i>%s</i>"), round(100 * as.integer(fl[1]) / nrow(pv))) else "-"
  data.frame(
    genus          = k,
    n_records      = nrow(d),
    n_species      = length(unique(na.omit(d$species))),
    status         = status_call(nrow(d)),
    conservation   = if (k %in% CONSERV_GENERA) paste(CONSERV_FLAGGED$scientific_name[word(CONSERV_FLAGGED$scientific_name, 1) == k], collapse = "; ") else "",
    peak_day       = md_of(peak),
    peak_doy       = if (is.na(peak)) 999L else as.integer(round(peak)),   # hidden chronological sort key
    active_months  = active_months(d$doy),
    top_flowers      = if (length(fl)) paste(plant_label(head(names(fl), 5)), collapse = ", ") else "- (no flower records)",
    top_flowers_html = if (length(fl)) paste(plant_label(head(names(fl), 5), sci_wrap = "<i>%s</i>"), collapse = ", ") else "- (no flower records)",  # HTML: Latin italic
    n_plant_genera = length(fl),
    flower_breadth = if (nrow(d) < CLAIM_MIN) "not enough records" else breadth_call(length(fl), nrow(pv)),
    top_plant      = top_plant,
    top_plant_html = top_plant_html,
    where_to_find  = where_call(d),
    stringsAsFactors = FALSE)
})
tbl <- do.call(rbind, rows) %>% arrange(desc(n_records))
# Forage preference: does the genus FAVOR certain plants beyond what's available, or just
# visit whatever's blooming? From the shared selectivity module (same test as the web colors).
# The preferred plant reads as a common name (fall back to the Latin genus if none known).
.pref_fmt      <- function(g) { cn <- plant_common_name(g); ifelse(is.na(cn), g, cn) }
.pref_fmt_html <- function(g) { cn <- plant_common_name(g); ifelse(is.na(cn), sprintf("<i>%s</i>", g), cn) }   # HTML: italicise the Latin fallback
tbl$forage_pref      <- forage_preference_label(tbl$genus, plant_fmt = .pref_fmt)
tbl$forage_pref_html <- forage_preference_label(tbl$genus, plant_fmt = .pref_fmt_html)
write.csv(tbl %>% dplyr::select(-top_flowers_html, -top_plant_html, -forage_pref_html), file.path(OUT_DIR, "bee_field_guide_genus.csv"), row.names = FALSE)   # CSV keeps plain labels
message(sprintf("Genus field guide: %d genera (%d never yet ID'd to species; %d rare, %d uncommon, %d common)",
                nrow(tbl), sum(tbl$n_species == 0),
                sum(tbl$status == "rare"), sum(tbl$status == "uncommon"), sum(tbl$status == "common")))

# plain, public-facing provenance line (this page is public; column meanings live in the def sub-row)
scope_str <- sprintf("These counts pool netted specimens and every iNaturalist photo, not just formal survey records, across all years and the whole park. Source: iNaturalist photos and netted specimens, Cabrillo National Monument (data as of %s).",
                     bee_data_asof())
# Records/Status caveat -- same wording as the species guide (Diet -> Flower breadth here), since
# this page also pools ALL data, so those columns reflect detection/photo effort, not abundance.
status_note <- sprintf("Records and Status count all data. That means netted specimens plus every iNaturalist photo, including casual public sightings, across all years. So they show how often a genus is detected or photographed here, not a survey-controlled abundance. A showy bee near a busy trail can read as common on public photos alone, so treat rare, uncommon, and common as recording frequency rather than true density. The cut-offs are rare below %d records, uncommon %d to %d, and common %d or more. Flower breadth is only stated at %d or more records.",
                       RARE_CUT, RARE_CUT, UNCOMMON_CUT - 1, UNCOMMON_CUT, CLAIM_MIN)

# ---- 3. styled, sortable HTML table -----------------------------------------
esc <- function(x) { x <- gsub("&", "&amp;", x); x <- gsub("<", "&lt;", x); gsub(">", "&gt;", x) }
st_rank <- c(rare = 0L, uncommon = 1L, common = 2L)   # hidden sort key so Status sorts by abundance, not alphabetically
# A GENUS row needs the genus's OWN iNat id. Records carry species/subspecies ids, so
# the genus id comes from the lookup's genus-rank row -- matched on the genus column,
# not on an assembled display name (that is what used to miss Biastes and Trachusa).
.lk <- read.csv(PATHS$taxonomy_lookup, stringsAsFactors = FALSE)
.gid <- .lk[tolower(str_squish(.lk$rank)) == "genus", c("genus", "taxon_id")]
tbl$inat_tid <- .gid$taxon_id[match(tbl$genus, .gid$genus)]
rows_html <- vapply(seq_len(nrow(tbl)), function(i) {
  r <- tbl[i, ]; low <- r$status == "rare"
  tag <- if (r$n_species == 0) '<span class="cn">not yet ID&#39;d to species</span>' else ""
  cs  <- if (r$conservation != "") sprintf('<sup class="cs" title="includes at-risk: %s">*</sup>', esc(r$conservation)) else ""
  pref_cls <- if (grepl("^Selective", r$forage_pref)) "pref-sel" else if (grepl("^Generalist", r$forage_pref)) "pref-gen" else "pref-na"
  sprintf(paste0('<tr class="%s"><td class="bee"><i>%s</i>%s%s%s</td><td class="num">%d</td><td class="num">%d</td>',
                 '<td data-sort="%d"><span class="pill st-%s">%s</span></td>',
                 '<td data-sort="%d">%s</td><td>%s</td><td class="loc">%s</td>',
                 '<td>%s</td><td>%s</td><td>%s</td><td class="%s">%s</td></tr>'),
          if (low) "low" else "", esc(r$genus), cs,
          inat_photo_link(r$inat_tid, r$genus), tag, r$n_records, r$n_species,
          unname(st_rank[r$status]), r$status, r$status,
          r$peak_doy, esc(r$peak_day), esc(r$active_months), esc(r$where_to_find),
          esc(r$flower_breadth), r$top_flowers_html, r$top_plant_html,
          pref_cls, r$forage_pref_html)
}, character(1))
# frozen definition sub-row: one short "what this column means" per column, pinned under the headers
def_row <- paste0('<tr class="def"><td class="def"></td>',
  '<td class="num def">times recorded</td><td class="num def">distinct species</td>',
  '<td class="def">how often recorded</td>',
  '<td class="def">average date seen</td><td class="def">months it&rsquo;s active</td>',
  '<td class="def">favored transect(s)</td><td class="def">how many plants used</td>',
  '<td class="def">seen on most</td><td class="def">top plant and its share</td>',
  '<td class="def">favorite, availability-corrected</td></tr>')
html <- paste0(
'<!doctype html><html><head><meta charset="utf-8"><title>Cabrillo National Monument &mdash; Native Bee Field Guide (Genus)</title>',
'<style>',
bee_table_css(),                                                                # shared base table chrome (single source -- theme_beescabr.R)
'td.pref-sel{color:#1f6b46;font-weight:600}td.pref-gen{color:#7a6a2e}td.pref-na{color:#a3a099;font-style:italic}',   # genus-guide preference cell colors
bee_badge_css(BEE_DIET_BG,  BEE_DIET_FG,  function(k) paste0(".pill.", k)),      # diet pills (sp/ge/mo/na)
bee_badge_css(BEE_ABUND_BG, BEE_ABUND_FG, function(k) paste0(".pill.st-", k)),   # abundance-status pills
'a.inat img{opacity:.8}',   # size/margin are inline on the img (single source: inat_taxon_links.R)
'a.inat:hover img{opacity:1}',
'</style></head><body>',
'<div class="org">Cabrillo National Monument</div>',
'<h1>A Native Bee Genus Field Guide &#128029;</h1>',
'<div class="byline">by Brandi Sanchez</div>',
'<p class="sub">A companion to the species guide, with one row per bee genus, pooling all records at every identification level. <b style="color:#1e5a2b">Species ID&rsquo;d</b> counts the distinct species pinned within the genus, and 0 means none yet. Each column&rsquo;s meaning is noted right under its header. As on the species guide, <b style="color:#1e5a2b">most-recorded flowers</b> and <b style="color:#1e5a2b">most-used plant</b> are simply where the genus was seen most, which reflects bloom and effort as much as choice, while <b style="color:#1e5a2b">forage preference</b> corrects for what was available in the same month, year, and sampling method. Because these counts include casual photos too, <b style="color:#1e5a2b">Status</b> reflects how often a genus is recorded here, not true abundance. Click a header to sort.</p>',
'<p class="sub"><b>A Status of &ldquo;rare&rdquo; does not mean a genus is truly rare.</b> Here it usually means the genus is under-sampled, not that it is genuinely scarce.</p>',
'<div class="scope"><p class="lead">', esc(scope_str), '</p>',
'<p>', esc(status_note), '</p>',
'<p><sup class="cs">*</sup> ', esc(CONSERV_LEGEND), '</p></div>',
'<div class="tbl-wrap"><table id="t"><thead><tr>',
'<th>Genus</th><th class="num">Records</th><th class="num">Species ID&#39;d</th><th>Status</th><th>Peak day</th><th>Active months</th><th>Where to find</th><th>Flower breadth</th><th>Most-recorded flowers</th><th>Most-used plant</th><th>Forage preference</th>',
'</tr>', def_row, '</thead><tbody>', paste(rows_html, collapse = ""), '</tbody></table></div>',
'<script>',
'(function(){var T=document.getElementById("t"),B=T.tBodies[0],ROWS=[].slice.call(B.rows),NC=T.tHead.rows[0].cells.length;',
bee_sort_mark_js(),
'function pv(r,c){var x=r.cells[c];if(!x)return"";var s=x.getAttribute("data-sort");return s!==null?s:x.innerText.trim();}',
'function isnum(s){return s!==""&&/^-?[0-9,]+(\\.[0-9]+)?%?$/.test(s);}',
'var NUM=[];for(var c=0;c<NC;c++){var all=true,any=false;for(var r=0;r<ROWS.length;r++){var v=pv(ROWS[r],c);if(v==="")continue;any=true;if(!isnum(v)){all=false;break;}}NUM[c]=any&&all;}',
'var CC=-1,CD=1;[].forEach.call(T.tHead.rows[0].cells,function(h,i){h.style.cursor="pointer";h.addEventListener("click",function(){',
'if(CC===i){CD=-CD;}else{CC=i;CD=1;}var d=CD;',
'ROWS.sort(function(x,y){var a=pv(x,i),c=pv(y,i);',
'if(NUM[i])return(parseFloat(a.replace(/[^0-9.\\-]/g,""))-parseFloat(c.replace(/[^0-9.\\-]/g,"")))*d;',
'return a.localeCompare(c)*d;});',
'ROWS.forEach(function(r){B.appendChild(r);});beeMarkSort(h,d);});});})();',
'</script></body></html>')
writeLines(html, file.path(OUT_DIR, "bee_field_guide_genus.html"))

# ---- 4. PNG table image (gridExtra) -----------------------------------------
if (requireNamespace("gridExtra", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE)) {
  pref_short <- sub(" \\(.*$", "", tbl$forage_pref)              # drop the "(N x vs available)" tail for the compact image
  disp <- tbl %>% transmute(Genus = ifelse(conservation != "", paste0(genus, " *"), genus),
                            N = n_records, `Species ID'd` = n_species, Status = status,
                            `Peak day` = peak_day, `Active months` = active_months,
                            `Where to find` = where_to_find,
                            `Flower breadth` = flower_breadth, `Most-recorded flowers` = top_flowers,
                            `Most-used plant` = top_plant, `Forage preference` = pref_short)
  ff <- matrix("plain", nrow(disp), ncol(disp)); ff[, which(names(disp) == "Genus")] <- "italic"   # bee genus column italic
  th <- gridExtra::ttheme_minimal(
    base_size = 7,
    core = list(fg_params = list(hjust = 0, x = 0.02, fontface = ff), bg_params = list(fill = c(BEE_TABLE[["row_odd"]], BEE_TABLE[["row_even"]]))),
    colhead = list(fg_params = list(hjust = 0, x = 0.02, fontface = "bold")))
  g   <- gridExtra::tableGrob(disp, rows = NULL, theme = th)
  scap <- grid::textGrob(paste(strwrap(scope_str, width = 160), collapse = "\n"),   # scope caption ABOVE the table
                        x = grid::unit(0.004, "npc"), hjust = 0, just = "left",
                        gp = grid::gpar(fontsize = 7.5, fontface = "bold", col = BEE_INK$secondary, lineheight = 1.15))
  gen_note <- sprintf("Status cut-offs (all-data record counts): rare < %d, uncommon %d-%d, common >= %d. Flower breadth stated only at >= %d records; Forage preference only at >= %d plant-visit records (fewer read 'not enough records'). %s",
                      RARE_CUT, RARE_CUT, UNCOMMON_CUT - 1, UNCOMMON_CUT, CLAIM_MIN, SELECT_MIN_REC, CONSERV_LEGEND)
  cap <- grid::textGrob(paste(strwrap(gen_note, width = 170), collapse = "\n"),
                        x = grid::unit(0.004, "npc"), hjust = 0, just = "left",
                        gp = grid::gpar(fontsize = 7, col = BEE_TABLE[["subtext"]], lineheight = 1.15))
  g   <- gridExtra::arrangeGrob(scap, g, cap, ncol = 1,
                                heights = grid::unit.c(grid::unit(2.4, "lines"), grid::unit(1, "null"), grid::unit(3.4, "lines")))
  bee_ggsave(file.path(OUT_DIR, "bee_field_guide_genus.png"), g,
                  width = 17, height = 0.26 * nrow(disp) + 2.3, limitsize = FALSE, bg = "white")
} else message("  (gridExtra/ggplot2 not available -- skipped PNG; CSV + HTML written)")

message("Wrote bee_field_guide_genus.{csv,html,png} to ", OUT_DIR)
