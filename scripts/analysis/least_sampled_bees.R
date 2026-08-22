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
if (!exists("inat_photo_link")) source("scripts/analysis/inat_taxon_links.R")                        # iNat logo -> taxon photo page

OUT_DIR       <- file.path(DIR_REPORT, "coverage/least_sampled")
SPECIES_RANKS <- c("species", "subspecies")
THIN_TOTAL    <- 50          # < this many records TOTAL (both methods) -> "least sampled" (matches the report's 50-record floor)
RARE_CUT      <- 15          # < this many records -> "rare"; RARE_CUT..THIN_TOTAL -> "uncommon" (same cut-offs as the field guide's Status)
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
rec_all <- bind_rows(grab(spec, "specimen (net)"), grab(inat, "photo (iNat)")) %>% filter(has(genus))
# Genera never yet identified to species (only genus-level records) get ONE genus row each: a bee too
# rarely seen to even name is exactly what "least-sampled" means. Genera that DO have species keep their
# species-level rows (their genus-only stray records are still dropped, as before).
genus_only <- rec_all %>% group_by(genus) %>%
  summarise(has_sp = any(taxon_rank %in% SPECIES_RANKS & has(epithet)), .groups = "drop") %>%
  filter(!has_sp) %>% pull(genus)
rec <- rec_all %>%
  mutate(species = dplyr::case_when(
    genus %in% genus_only                        ~ genus,                   # genus-only bee -> keyed by genus
    taxon_rank %in% SPECIES_RANKS & has(epithet) ~ paste(genus, epithet),   # species-level
    TRUE                                         ~ NA_character_)) %>%
  filter(!is.na(species))

# ---- 2. per-species method split, keep the under-sampled -------------------
split_tbl <- rec %>% group_by(species) %>%
  summarise(net_records   = sum(method == "specimen (net)"),
            photo_records  = sum(method == "photo (iNat)"),
            total_records  = n(), .groups = "drop") %>%
  filter(total_records < THIN_TOTAL) %>%
  arrange(total_records, desc(pmin(net_records, photo_records)), species)
message(sprintf("Least-sampled bees (< %d records total): %d taxa (%d genus-only)", THIN_TOTAL,
                nrow(split_tbl), sum(split_tbl$species %in% genus_only)))

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
  mutate(coverage = coverage_of(net_records, photo_records),
         # Status uses the same cut-offs as the field guide. Every taxon here is < THIN_TOTAL
         # by definition, so Status reads "rare" or "uncommon" only, never "common".
         status   = ifelse(total_records < RARE_CUT, "rare",
                    ifelse(total_records < THIN_TOTAL, "uncommon", "common")))
# genus-only rows: mark them in the Bee column so a reader knows it isn't a species
tbl$is_genus <- tbl$species %in% genus_only
tbl$common_name[tbl$is_genus] <- "genus only, not yet identified to species"

# IUCN status (optional -- only if the conservation module + cache loaded)
tbl$iucn <- if (exists("iucn_code_of")) iucn_code_of(tbl$species) else NA_character_
tbl$conservation <- if (exists("conservation_label")) conservation_label(tbl$species) else ""
HAVE_IUCN <- exists("iucn_cache_exists") && isTRUE(try(iucn_cache_exists(), silent = TRUE))

tbl <- tbl %>% select(species, common_name, net_records, photo_records, total_records,
                      status, coverage, peak_months, active_window, top_transects, top_flowers, top_flowers_html,
                      iucn, conservation, example_url)
write.csv(tbl %>% select(-top_flowers_html), file.path(OUT_DIR, "least_sampled_bees.csv"), row.names = FALSE)   # CSV keeps the plain flower list
message(sprintf("  coverage: %d both(thin), %d photo-only, %d specimen-only",
                sum(tbl$coverage == "both (thin)"), sum(tbl$coverage == "photo-only"),
                sum(tbl$coverage == "specimen-only")))

# scope caption (same "Scope | Method | Rank | Source" format as the figure captions), shown ABOVE the table
scope_str <- sprintf("These counts pool netted specimens and every iNaturalist photo, not just formal survey records, across all years and the whole park. Source: iNaturalist photos and netted specimens, Cabrillo National Monument (data as of %s).",
                     bee_data_asof())
# Records/Status caveat -- same wording as the field guides (no Diet line; this page has no diet
# column). These columns pool ALL data, so they reflect detection/photo effort, not abundance.
status_note <- sprintf("Total and Status count all data. That means netted specimens plus every iNaturalist photo, including casual public sightings, across all years. So they show how often a species is detected or photographed here, not a survey-controlled abundance. A showy bee near a busy trail can read as common on public photos alone, so treat rare, uncommon, and common as recording frequency rather than true density. The cut-offs are rare below %d records, uncommon %d to %d, and common %d or more.",
                       RARE_CUT, RARE_CUT, THIN_TOTAL - 1, THIN_TOTAL)

# ---- 4. styled, sortable HTML table -----------------------------------------
esc <- function(x) { x <- gsub("&", "&amp;", x); x <- gsub("<", "&lt;", x); gsub(">", "&gt;", x) }
# iNaturalist-style mark (flying bird on the iNat green badge) for the "example observation" link
INAT_ICON <- sprintf('<img src="%s" width="16" height="14" alt="iNaturalist observation" style="vertical-align:-3px">', INAT_LOGO_URL)
cov_class <- function(s) ifelse(grepl("^both", s), "cb", ifelse(grepl("^photo", s), "cp", "cs"))
st_rank   <- c(rare = 0L, uncommon = 1L, common = 2L)   # hidden sort key so Status sorts by abundance, not alphabetically
.lk <- read.csv(PATHS$taxonomy_lookup, stringsAsFactors = FALSE)   # iNat taxon ids for the logo links
rows_html <- vapply(seq_len(nrow(tbl)), function(i) {
  r <- tbl[i, ]
  cs   <- if (has(r$conservation)) sprintf('<sup class="cs" title="%s">*</sup>', esc(r$conservation)) else ""
  iucn_td <- if (HAVE_IUCN) sprintf('<td class="num"><span class="iucn i-%s">%s</span></td>',
                                    tolower(ifelse(has(r$iucn), r$iucn, "ne")), esc(ifelse(has(r$iucn), r$iucn, "NE"))) else ""
  bee_td <- sprintf('<td class="bee"><i>%s</i>%s%s%s</td>', esc(r$species), cs,
                    inat_photo_link(.lk$taxon_id[match(r$species, .lk$scientific_name)], r$species),
                    if (has(r$common_name)) paste0('<span class="cn">', esc(r$common_name), '</span>') else "")
  fl <- if (has(r$example_url)) sprintf('%s <a href="%s" title="example iNaturalist observation">%s</a>', r$top_flowers_html, esc(r$example_url), INAT_ICON) else r$top_flowers_html
  cov_icon <- if (grepl("^photo", r$coverage))    ' <span class="vneed" title="Photographed but never netted, so a specimen voucher is needed. Go net one.">&#128300;</span>'       # microscope = collect a voucher
         else if (grepl("^specimen", r$coverage)) ' <span class="vneed" title="Collected as a specimen but never photographed. Go photograph one.">&#128247;</span>'  # camera = take more photos
         else ' <span class="vneed" title="Only a few of each method. More netting or more photos both help.">&#128247;&#128300;</span>'   # both (thin): camera + microscope
  sprintf(paste0('<tr>%s%s<td class="num">%d</td><td class="num">%d</td><td class="num">%d</td>',
                 '<td data-sort="%d"><span class="pill st-%s">%s</span></td>',
                 '<td style="white-space:nowrap"><span class="pill %s">%s</span>%s</td>',
                 '<td>%s</td><td>%s</td><td class="loc">%s</td><td>%s</td></tr>'),
          bee_td, iucn_td, r$net_records, r$photo_records, r$total_records,
          unname(st_rank[r$status]), r$status, r$status,
          cov_class(r$coverage), esc(r$coverage), cov_icon,
          esc(r$peak_months), esc(r$active_window), esc(r$top_transects), fl)
}, character(1))
iucn_th  <- if (HAVE_IUCN) '<th class="num">IUCN</th>' else ""
iucn_def <- if (HAVE_IUCN) '<td class="num def">Red List</td>' else ""
iucn_par <- if (HAVE_IUCN) '<p>The IUCN column shows each bee&rsquo;s Red List status. Codes: NE (Not Evaluated), DD Data Deficient, LC Least Concern, NT Near Threatened, VU Vulnerable, EN Endangered, CR Critically Endangered. Source: IUCN Red List API v4.</p>' else ""
# frozen definition sub-row: one short "what this column means" per column, pinned under the headers
def_row <- paste0('<tr class="def"><td class="def"></td>', iucn_def,
  '<td class="num def">netted</td><td class="num def">on iNat</td><td class="num def">specimen + photo</td>',
  '<td class="def">how often recorded</td><td class="def">which method(s) it&rsquo;s in</td>',
  '<td class="def">month seen most</td><td class="def">months it&rsquo;s been seen</td>',
  '<td class="def">transect seen most on</td><td class="def">plants recorded on most</td></tr>')
html <- paste0(
'<!doctype html><html><head><meta charset="utf-8"><title>Cabrillo National Monument &mdash; Least-Sampled Native Bees</title><style>',
bee_table_css(),                                                                   # shared base table chrome (single source -- theme_beescabr.R)
bee_badge_css(BEE_COVERAGE_BG, BEE_COVERAGE_FG, function(k) paste0(".pill.", k)),   # coverage pills from theme tokens
bee_badge_css(BEE_ABUND_BG, BEE_ABUND_FG, function(k) paste0(".pill.st-", k)),      # abundance-status pills (rare/uncommon), shared with the field guide
'.vneed{font-size:12px;vertical-align:middle}',
bee_badge_css(BEE_IUCN_BG, BEE_IUCN_FG, function(k) paste0(".iucn.i-", k)),         # IUCN chips from theme tokens
'a{color:#3a6b8a;text-decoration:none}',
'</style></head><body>',
'<div class="org">Cabrillo National Monument</div>',
'<h1>Least-Sampled Native Bees &#128029;</h1>',
'<div class="byline">by Brandi Sanchez</div>',
sprintf('<p class="sub">These %d native bees each have fewer than %d records, making them the park&rsquo;s least-sampled and the best targets for more survey effort. Because each has under %d records, the Status column reads rare or uncommon, never common.</p>',
        nrow(tbl), THIN_TOTAL, THIN_TOTAL),
sprintf('<p class="sub">The <b style="color:%s">Coverage</b> column tells you what each one needs. <span class="pill cb">both (thin)</span> means only a few of each method. <span class="pill cp">photo-only</span> means it has been photographed but never netted, so go net a voucher &#128300;. <span class="pill cs">specimen-only</span> means the opposite, so go photograph one &#128247;. The %s icon opens an example observation. Every column&rsquo;s meaning is noted right under its header, and you can click any header to sort.</p>',
        BEE_HTML_GREEN[["deep"]], INAT_ICON),
'<p class="sub"><b>Some of these bees may be hard to identify, not truly scarce.</b> When a photo or specimen can only be named to genus, it is not counted toward a species here, so a bee can look under-sampled when it is really just hard to tell apart. For those, a clear photo or a voucher an expert can confirm to species helps as much as finding more. So don&rsquo;t read a &ldquo;rare&rdquo; Status here as truly rare.</p>',
'<p class="sub"><b>Look beyond this list.</b> These plants and months are where people have already found these bees, not the whole picture of where they live. The bees we are still missing are out there on plants and at times no one has checked. So don&rsquo;t just search the spots listed here. Look widely.</p>',
'<div class="scope"><p class="lead">', esc(scope_str), '</p>',
sprintf('<p>%s</p>', esc(status_note)),
iucn_par,
'</div>',
'<div class="tbl-wrap"><table id="t"><thead><tr><th>Bee</th>', iucn_th, '<th class="num">Specimen</th><th class="num">Photo</th><th class="num">Total</th>',
'<th>Status</th><th>Coverage</th><th>Peak month</th><th>Active window</th><th>Where (transect)</th><th>Top flowers</th></tr>',
def_row,
'</thead><tbody>', paste(rows_html, collapse = ""), '</tbody></table></div>',
'<script>',
bee_sort_mark_js(),
'document.querySelectorAll("#t th").forEach(function(h,i){h.addEventListener("click",function(){',
'var t=h.closest("table"),b=t.tBodies[0],rows=[].slice.call(b.rows);h._d=!h._d;var d=h._d?1:-1;',
'rows.sort(function(x,y){var a=x.cells[i].innerText.trim(),c=y.cells[i].innerText.trim();',
'var na=parseFloat(a.replace(/[^0-9.\\-]/g,"")),nc=parseFloat(c.replace(/[^0-9.\\-]/g,""));',
'if(!isNaN(na)&&!isNaN(nc))return(na-nc)*d;return a.localeCompare(c)*d;});',
'rows.forEach(function(r){b.appendChild(r);});beeMarkSort(h,d);});});',
'</script></body></html>')
writeLines(html, file.path(OUT_DIR, "least_sampled_bees.html"))

# ---- 5. PNG table image (gridExtra) -----------------------------------------
if (requireNamespace("gridExtra", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE)) {
  disp <- tbl %>% transmute(
    Bee = ifelse(has(conservation), paste0(species, " *"), species),
    Specimen = net_records, Photo = photo_records, Total = total_records,
    Status = status, Coverage = coverage,
    `Peak month` = peak_months, `Active` = active_window,
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
