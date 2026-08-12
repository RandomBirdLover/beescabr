# =============================================================
# analysis/records_by_family_pie.R
# beescabr -- share of ALL bee records by FAMILY, as a pie/donut.
#
# Companion to records_per_genus_by_evidence.R, but one level up (family) and with
# NO lethal/non-lethal split -- every record counts once, coloured by family so the
# reader sees which families dominate the park's record base at a glance.
#
# Run from the repo root:  Rscript scripts/analysis/records_by_family_pie.R
# Depends on: dplyr, stringr, ggplot2 (+ config.R, theme_beescabr.R).
# =============================================================

for (pkg in c("ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    try(install.packages(pkg, repos = "https://cloud.r-project.org"), silent = TRUE)
}
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(ggplot2) })

if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_FAMILY")) source("scripts/analysis/theme_beescabr.R")   # shared house style
OUT_REPORT <- file.path(DIR_REPORT, "coverage/records_by_evidence")
dir.create(OUT_REPORT, recursive = TRUE, showWarnings = FALSE)

# the five bee families with house colours; anything else (or blank) -> "Other"
MAIN_FAM <- c("Apidae", "Halictidae", "Megachilidae", "Andrenidae", "Colletidae")
fam_of <- function(f) { f <- str_squish(f); f[is.na(f) | f == ""] <- "Other"; ifelse(f %in% MAIN_FAM, f, "Other") }

# ---- 1. pool BOTH methods (no lethal/non-lethal split); count records per family
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
famraw <- str_squish(c(spec$family, inat$family))
n_noid <- sum(is.na(famraw) | famraw == "")          # records with NO family-level ID -> excluded (was the "Other" bucket)
famraw <- famraw[!is.na(famraw) & famraw != ""]
d <- data.frame(family = fam_of(famraw)) %>%
  count(family, name = "records") %>%
  arrange(desc(records))
d <- rbind(d[d$family != "Other", , drop = FALSE], d[d$family == "Other", , drop = FALSE])   # any real non-main family slice last (none at present)
d$family <- factor(d$family, levels = d$family)
tot <- sum(d$records)
d$frac <- d$records / tot
d$lab  <- ifelse(d$frac >= 0.03, sprintf("%.0f%%", 100 * d$frac), "")   # on-slice % only where it fits; thin slivers get their % in the legend instead
d$labcol <- "white"   # on-slice % labels are always white (user preference), even on the lighter hues
write.csv(d[, c("family", "records", "frac")], file.path(OUT_REPORT, "records_by_family.csv"), row.names = FALSE)
message(sprintf("Records by family (both methods pooled): %s | total %s | excluded %d records with no family-level ID",
                paste(sprintf("%s=%d", d$family, d$records), collapse = "  "), format(tot, big.mark = ","), n_noid))

# ---- 2. donut: one slice per family, coloured by BEE_FAMILY, % on the slices ----
.li <- match(levels(d$family), d$family)   # legend shows count AND % (so thin slivers like Megachilidae still report their %)
leg <- sprintf("%s  (%s, %.0f%%)", levels(d$family), format(d$records[.li], big.mark = ","), 100 * d$frac[.li])
# Megachilidae is too thin for an on-slice %, so point a short leader out of its slice.
# Clockwise-from-top cumulative = reverse of the factor order, so a slice's start angle is
# the sum of the fracs of the families that follow it in the data (the rows below it).
mega_i   <- match("Megachilidae", as.character(d$family))
mega_y   <- (if (mega_i < nrow(d)) sum(d$frac[(mega_i + 1):nrow(d)]) else 0) + d$frac[mega_i] / 2
mega_lab <- sprintf("%.0f%%", 100 * d$frac[mega_i])
g <- ggplot(d, aes(x = 2, y = frac, fill = family)) +
  geom_col(width = 1, color = "white", linewidth = 1.2) +
  geom_text(aes(label = lab, colour = labcol, group = family), position = position_stack(vjust = 0.5),
            fontface = "bold", size = 3.6) +   # group=family keeps text stacking aligned with the bars
  scale_colour_identity() +
  coord_polar(theta = "y") +
  xlim(0.4, 2.95) +                                  # hole -> donut; extra radial room for the Megachilidae leader
  annotate("segment", x = 2.52, xend = 2.74, y = mega_y, yend = mega_y,
           colour = BEE_INK$secondary, linewidth = 0.4) +
  annotate("text", x = 2.82, y = mega_y, label = mega_lab, hjust = 0.5, vjust = 0.5,
           fontface = "bold", size = 3.3, colour = BEE_INK$primary) +
  scale_fill_manual(values = BEE_FAMILY, breaks = levels(d$family), labels = leg, name = "family") +
  annotate("text", x = 0.4, y = 0, label = sprintf("%s\nrecords", format(tot, big.mark = ",")),
           fontface = "bold", size = 4.3, lineheight = 0.95, color = BEE_INK$primary) +
  labs(title = "Bee Records by Family",
       subtitle = sprintf("%s dominate the park's bee records (~%.0f%%) -- a few families make up nearly all of them.",
                          as.character(d$family[1]), 100 * d$frac[1]),
       caption = scope_cap(scope = "all records with a family-level ID, whole park; both methods pooled (no lethal / non-lethal split)",
                           method = "lethal + non-lethal pooled", rank = "family")) +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, colour = BEE_INK$primary, margin = margin(b = 2)),
        plot.subtitle = element_text(hjust = 0.5, colour = BEE_INK$secondary, size = 10, margin = margin(b = 6)),
        plot.caption = element_text(colour = BEE_INK$secondary, size = 9, hjust = 0, margin = margin(t = 6)),
        plot.caption.position = "plot", legend.position = "right",
        legend.text = element_text(colour = BEE_INK$secondary))
bee_ggsave(file.path(OUT_REPORT, "records_by_family_pie.png"), g, width = 8, height = 5.6, bg = "white")
message("Wrote records_by_family_pie.png + records_by_family.csv to ", OUT_REPORT)
