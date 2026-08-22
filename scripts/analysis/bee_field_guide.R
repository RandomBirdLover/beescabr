# =============================================================
# analysis/bee_field_guide.R
# beescabr -- per-species FIELD-REFERENCE TABLE for the park's native bees.
#
# One row per bee species, answering (for surveyors + management):
#   * PEAK DAY      -- the single day activity centres on (circular mean of record dates)
#   * ACTIVE MONTHS -- the months that hold the bulk (5th-95th percentile) of records
#   * TOP FLOWERS   -- the 3-5 plant genera it is recorded on most
#   * DIET          -- specialist / moderate / generalist, by how many plant genera it uses
#   * WHERE TO FIND -- the transect(s) it favors, or (if mostly off-transect) a map
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
if (!exists("scope_cap")) source("scripts/analysis/theme_beescabr.R")                            # shared scope-caption format
if (!exists("inat_photo_link")) source("scripts/analysis/inat_taxon_links.R")                    # iNat logo -> taxon photo page
OUT_DIR       <- file.path(DIR_REPORT, "reference/field_guide")
SPECIES_RANKS <- c("species", "subspecies")
MIN_CONF      <- 10          # < this many records -> low-confidence flag
RARE_CUT      <- 15          # < this many records -> "rare" (rarely recorded here)
UNCOMMON_CUT  <- 50          # < this -> "uncommon"; >= this -> "common"
CLAIM_MIN     <- 50          # < this many records -> DON'T claim an interpretive column (diet); reads "not enough records"
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
  subsp       = tolower(str_squish(if ("subspecies" %in% names(df)) df$subspecies else rep("", nrow(df)))),
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
  } else ""                                                      # off-transect: no location shown here (see the occurrence map for pins)
}
mode_chr <- function(x) { x <- x[has(x)]; if (!length(x)) return("") ; names(sort(table(x), decreasing = TRUE))[1] }

make_row <- function(d, k, rank = "species", lookup = k) {
  pv <- d[has(d$plant_genus), ]
  fl <- sort(table(pv$plant_genus), decreasing = TRUE)
  peak <- circ_mean_doy(d$doy)
  # Common name from records ID'd AT THIS row's rank: a species row must show the SPECIES
  # vernacular, not a subspecies' name that happens to dominate the pooled records (e.g.
  # A. urbana = "Urbane Digger Bee", NOT its subspecies clementina's "San Clemente Digger Bee").
  cn <- mode_chr(d$common[d$taxon_rank == rank]); if (cn == "") cn <- mode_chr(d$common)
  data.frame(
    genus          = d$genus[1],
    bee            = k,
    common_name    = cn,
    n_records      = nrow(d),
    confidence     = if (nrow(d) < MIN_CONF) "low (n<10)" else "ok",
    status         = status_call(nrow(d)),
    peak_day       = md_of(peak),
    peak_doy       = if (is.na(peak)) 999L else as.integer(round(peak)),   # hidden chronological sort key
    active_months  = active_months(d$doy),
    top_flowers      = if (length(fl)) paste(plant_label(head(names(fl), 5)), collapse = ", ") else "- (no flower records)",
    top_flowers_html = if (length(fl)) paste(plant_label(head(names(fl), 5), sci_wrap = "<i>%s</i>"), collapse = ", ") else "- (no flower records)",  # HTML: Latin italic
    n_plant_genera = length(fl),
    diet           = if (nrow(d) < CLAIM_MIN) "not enough records" else diet_call(length(fl), nrow(pv)),
    where_to_find  = where_call(d),
    rank           = rank,     # species | subspecies -- subspecies get their own row, sorted under the parent
    lookup         = lookup,   # binomial used for the species-level IUCN + forage lookups
    stringsAsFactors = FALSE)
}
sp_keys <- sort(unique(rec$species))
rows <- lapply(sp_keys, function(k) make_row(rec[rec$species == k, ], k))

# Named subspecies get their OWN row (full trinomial), computed from their own records, so they
# appear in the guide sorted right under their parent species. This does NOT touch the species
# rows or any figure -- every graph still pools subspecies into the species (species-level).
rec_ss <- bind_rows(grab(spec), grab(inat)) %>%
  filter(taxon_rank == "subspecies", genus != "", epithet != "", has(subsp)) %>%
  mutate(parent = paste(genus, epithet), species = paste(genus, epithet, subsp))
ss_rows <- lapply(sort(unique(rec_ss$species)), function(k) {
  d <- rec_ss[rec_ss$species == k, ]; make_row(d, k, rank = "subspecies", lookup = d$parent[1])
})
tbl <- do.call(rbind, c(rows, ss_rows)) %>% arrange(genus, bee)

# ---- IUCN Red List status from the shared conservation module (one source) ----
# conservation_status.R reads data/checklists/iucn/iucn_status.csv (written by
# refresh_iucn_status.R); the IUCN column shows when that cache exists.
HAVE_IUCN        <- iucn_cache_exists()
# IUCN is a SPECIES-level determination -> look it up on the binomial (tbl$lookup), so a
# subspecies row inherits its parent species' Red List category (they aren't listed separately).
tbl$iucn         <- iucn_code_of(tbl$lookup)
tbl$iucn_name    <- iucn_name_of(tbl$lookup)
tbl$conservation <- conservation_label(tbl$lookup)
# Forage preference -- availability-corrected (matched month/year/method test), SPECIES level.
# Populated for species with >= SELECT_MIN_REC plant-visit records; "too few records to judge" below that.
tbl$forage_pref      <- forage_preference_label_species(tbl$bee, plant_fmt = plant_label)
tbl$forage_pref_html <- forage_preference_label_species(tbl$bee, plant_fmt = function(g) plant_label(g, sci_wrap = "<i>%s</i>"))  # HTML: Latin italic
# The forage test pools subspecies into the species, so a subspecies can't be judged on its own
# (finer) sample -- state that, consistent with its Diet column, rather than the "-" no-match.
is_ss <- tbl$rank == "subspecies"
tbl$forage_pref[is_ss]      <- "not enough records"
tbl$forage_pref_html[is_ss] <- "not enough records"
write.csv(tbl %>% dplyr::select(-top_flowers_html, -forage_pref_html, -rank, -lookup),
          file.path(OUT_DIR, "bee_field_guide_species.csv"), row.names = FALSE)   # CSV keeps plain labels + original schema
message(sprintf("Field guide: %d species%s (%d with >= %d records)",
                sum(tbl$rank == "species"),
                if (any(is_ss)) sprintf(" + %d subspecies", sum(is_ss)) else "",
                sum(tbl$confidence == "ok"), MIN_CONF))

# scope caption (same "Scope | Method | Rank | Source" format as the figure captions), shown ABOVE the table
scope_str <- sprintf("These counts pool netted specimens and every iNaturalist photo, not just formal survey records, across all years and the whole park. Source: iNaturalist photos and netted specimens, Cabrillo National Monument (data as of %s).",
                     bee_data_asof())

# ---- 3. styled, sortable HTML table -----------------------------------------
esc <- function(x) { x <- gsub("&", "&amp;", x); x <- gsub("<", "&lt;", x); gsub(">", "&gt;", x) }
diet_class <- function(s) ifelse(grepl("^Special", s), "sp", ifelse(grepl("^General", s), "ge",
                          ifelse(grepl("^Moder", s), "mo", "na")))
pref_class <- function(s) ifelse(grepl("^Selective", s), "pref-sel", ifelse(grepl("^Generalist", s), "pref-gen", "pref-na"))
st_rank <- c(rare = 0L, uncommon = 1L, common = 2L)   # hidden sort key so Status sorts by abundance, not alphabetically
# iNat taxon id per bee (checklist lookup covers specimen-only species too); a miss
# falls back to a name search on iNat inside inat_photo_link, never a dead link.
.lk <- read.csv(PATHS$taxonomy_lookup, stringsAsFactors = FALSE)
tbl$inat_tid <- .lk$taxon_id[match(tbl$bee, .lk$scientific_name)]
rows_html <- vapply(seq_len(nrow(tbl)), function(i) {
  r <- tbl[i, ]; low <- r$confidence != "ok"
  cs <- if (r$conservation != "") sprintf('<sup class="cs" title="%s">*</sup>', esc(r$conservation)) else ""
  iucn_td <- if (HAVE_IUCN) sprintf('<td class="num"><span class="iucn i-%s" title="%s">%s</span></td>',
                                    tolower(r$iucn), esc(r$iucn_name), esc(r$iucn)) else ""
  sprintf(paste0('<tr class="%s"><td class="bee"><i>%s</i>%s%s%s</td>%s<td class="num">%d</td>',
                 '<td data-sort="%d"><span class="pill st-%s">%s</span></td>',
                 '<td data-sort="%d">%s</td><td>%s</td><td class="loc">%s</td>',
                 '<td><span class="pill %s">%s</span></td><td>%s</td><td><span class="pill %s">%s</span></td></tr>'),
          if (low) "low" else "", esc(r$bee), cs,
          inat_photo_link(r$inat_tid, r$bee),
          if (has(r$common_name)) paste0('<span class="cn">', esc(r$common_name), '</span>') else "",
          iucn_td, r$n_records,
          unname(st_rank[r$status]), r$status, r$status,
          r$peak_doy, esc(r$peak_day), esc(r$active_months), esc(r$where_to_find),
          diet_class(r$diet), esc(r$diet), r$top_flowers_html,
          pref_class(r$forage_pref), r$forage_pref_html)
}, character(1))
iucn_th  <- if (HAVE_IUCN) '<th class="num">IUCN</th>' else ""
iucn_def <- if (HAVE_IUCN) '<td class="num def">Red List status</td>' else ""
# frozen definition sub-row: one short "what this column means" per column, pinned under the headers
def_row <- paste0('<tr class="def"><td class="def"></td>', iucn_def,
  '<td class="num def">times recorded</td><td class="def">how often recorded</td>',
  '<td class="def">average date seen</td><td class="def">months it&rsquo;s active</td>',
  '<td class="def">favored transect(s)</td><td class="def">how many plants it uses</td>',
  '<td class="def">seen on most</td><td class="def">favorite, availability-corrected</td></tr>')
note_txt <- if (HAVE_IUCN) {
  "The IUCN column shows each bee's Red List status. Codes: NE (Not Evaluated), DD Data Deficient, LC Least Concern, NT Near Threatened, VU Vulnerable, EN Endangered, CR Critically Endangered. Source: IUCN Red List API v4."
} else "* IUCN threatened / near-threatened species, from the last IUCN Red List pull (data/checklists/iucn/iucn_status.csv). Run refresh_iucn_status.R to populate the full IUCN column."
# Records/Status caveat -- this guide pools ALL data (no survey-only filter), so those two
# columns reflect detection/photo effort, not a survey-controlled abundance estimate.
status_note <- sprintf("Records and Status count all data. That means netted specimens plus every iNaturalist photo, including casual public sightings, across all years. So they show how often a species is detected or photographed here, not a survey-controlled abundance. A showy bee near a busy trail can read as common on public photos alone, so treat rare, uncommon, and common as recording frequency rather than true density. The cut-offs are rare below %d records, uncommon %d to %d, and common %d or more. Diet is only stated at %d or more records.",
                        RARE_CUT, RARE_CUT, UNCOMMON_CUT - 1, UNCOMMON_CUT, CLAIM_MIN)
note_txt <- paste(status_note, note_txt)
html <- paste0(
'<!doctype html><html><head><meta charset="utf-8"><title>Cabrillo National Monument &mdash; Native Bee Field Guide (Species)</title>',
'<style>',
bee_table_css(),                                                                    # shared base table chrome (single source -- theme_beescabr.R)
bee_badge_css(BEE_DIET_BG,   BEE_DIET_FG,   function(k) paste0(".pill.", k)),        # diet pills (sp/ge/mo/na)
bee_badge_css(BEE_ABUND_BG,  BEE_ABUND_FG,  function(k) paste0(".pill.st-", k)),     # abundance-status pills
bee_badge_css(BEE_FORAGE_BG, BEE_FORAGE_FG, function(k) paste0(".pill.pref-", k)),   # forage-preference pills
bee_badge_css(BEE_IUCN_BG,   BEE_IUCN_FG,   function(k) paste0(".iucn.i-", k)),      # IUCN chips
'a.inat img{opacity:.8}',   # size/margin are inline on the img (single source: inat_taxon_links.R)
'a.inat:hover img{opacity:1}',
'</style></head><body>',
'<div class="org">Cabrillo National Monument</div>',
'<h1>A Native Bee Species Field Guide &#128029;</h1>',
'<div class="byline">by Brandi Sanchez</div>',
'<p class="sub">One row per bee species, with any named subspecies on its own row, sorted under its parent. Each column&rsquo;s meaning is noted right under its header. Two columns are easy to mix up. <b style="color:#1e5a2b">Most-recorded flowers</b> is simply where a bee was seen most, which reflects what was blooming and how much people looked, not just the bee&rsquo;s choice. <b style="color:#1e5a2b">Forage preference</b> is the stronger signal, because it compares a bee&rsquo;s flower visits to the rest of the community in the same month, year, and sampling method, so a good bloom year or a photo-versus-net quirk cannot masquerade as a real preference. Grey rows have fewer than 10 records, so their peak day and season are rough. Click a header to sort.</p>',
'<p class="sub"><b>A Status of &ldquo;rare&rdquo; does not mean a bee is truly rare.</b> Here it usually means the bee is under-sampled, or that its specimen or photo records could not be identified to species.</p>',
'<div class="scope"><p class="lead">', esc(scope_str), '</p>',
'<p>', esc(note_txt), '</p></div>',
'<div class="tbl-wrap"><table id="t"><thead><tr>',
'<th>Bee</th>', iucn_th, '<th class="num">Records</th><th>Status</th><th>Peak day</th><th>Active months</th><th>Where to find</th><th>Diet</th><th>Most-recorded flowers</th><th>Forage preference</th>',
'</tr>', def_row, '</thead><tbody>', paste(rows_html, collapse = ""), '</tbody></table></div>',
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
                            N = n_records, Status = status, `Peak day` = peak_day,
                            `Active months` = active_months, `Where to find` = where_to_find,
                            Diet = diet, `Most-recorded flowers` = top_flowers,
                            `Forage preference` = pref_short)
  if (HAVE_IUCN) disp <- dplyr::relocate(dplyr::mutate(disp, IUCN = tbl$iucn), IUCN, .after = Bee)
  ff <- matrix("plain", nrow(disp), ncol(disp)); ff[, which(names(disp) == "Bee")] <- "italic"   # bee binomial column italic
  th <- gridExtra::ttheme_minimal(
    base_size = 7,
    core = list(fg_params = list(hjust = 0, x = 0.02, fontface = ff), bg_params = list(fill = c(BEE_TABLE[["row_odd"]], BEE_TABLE[["row_even"]]))),
    colhead = list(fg_params = list(hjust = 0, x = 0.02, fontface = "bold")))
  g   <- gridExtra::tableGrob(disp, rows = NULL, theme = th)
  scap <- grid::textGrob(paste(strwrap(scope_str, width = 165), collapse = "\n"),   # scope caption ABOVE the table
                        x = grid::unit(0.004, "npc"), hjust = 0, just = "left",
                        gp = grid::gpar(fontsize = 7.5, fontface = "bold", col = BEE_INK$secondary, lineheight = 1.15))
  cap <- grid::textGrob(paste(strwrap(note_txt, width = 150), collapse = "\n"),
                        x = grid::unit(0.004, "npc"), hjust = 0, just = "left",
                        gp = grid::gpar(fontsize = 7, col = BEE_TABLE[["subtext"]], lineheight = 1.15))
  g   <- gridExtra::arrangeGrob(scap, g, cap, ncol = 1,
                                heights = grid::unit.c(grid::unit(2.4, "lines"), grid::unit(1, "null"), grid::unit(2.6, "lines")))
  bee_ggsave(file.path(OUT_DIR, "bee_field_guide_species.png"), g,
                  width = if (HAVE_IUCN) 18.5 else 17.5, height = 0.24 * nrow(disp) + 2.0, limitsize = FALSE, bg = "white")
} else message("  (gridExtra/ggplot2 not available -- skipped PNG; CSV + HTML written)")

message("Wrote bee_field_guide_species.{csv,html,png} to ", OUT_DIR)
