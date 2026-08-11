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
d <- data.frame(family = fam_of(c(spec$family, inat$family))) %>%
  count(family, name = "records") %>%
  arrange(desc(records))
d <- rbind(d[d$family != "Other", , drop = FALSE], d[d$family == "Other", , drop = FALSE])   # Other slice last
d$family <- factor(d$family, levels = d$family)
tot <- sum(d$records)
d$frac <- d$records / tot
d$lab  <- ifelse(d$frac >= 0.03, sprintf("%.0f%%", 100 * d$frac), "")   # hide labels on tiny slivers
write.csv(d[, c("family", "records", "frac")], file.path(OUT_REPORT, "records_by_family.csv"), row.names = FALSE)
message(sprintf("Records by family (both methods pooled): %s | total %s",
                paste(sprintf("%s=%d", d$family, d$records), collapse = "  "), format(tot, big.mark = ",")))

# ---- 2. donut: one slice per family, coloured by BEE_FAMILY, % on the slices ----
leg <- sprintf("%s  (%s)", levels(d$family), format(d$records[match(levels(d$family), d$family)], big.mark = ","))
g <- ggplot(d, aes(x = 2, y = frac, fill = family)) +
  geom_col(width = 1, color = "white", linewidth = 1.2) +
  geom_text(aes(label = lab), position = position_stack(vjust = 0.5), color = "white",
            fontface = "bold", size = 3.6) +
  coord_polar(theta = "y") +
  xlim(0.4, 2.5) +                                   # hole -> donut
  scale_fill_manual(values = BEE_FAMILY, breaks = levels(d$family), labels = leg, name = "family") +
  annotate("text", x = 0.4, y = 0, label = sprintf("%s\nrecords", format(tot, big.mark = ",")),
           fontface = "bold", size = 4.3, lineheight = 0.95, color = BEE_INK$primary) +
  labs(title = "Bee Records by Family",
       caption = scope_cap(scope = "all records, whole park; both methods pooled (no lethal / non-lethal split)",
                           method = "lethal + non-lethal pooled", rank = "family")) +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, colour = BEE_INK$primary, margin = margin(b = 4)),
        plot.caption = element_text(colour = BEE_INK$secondary, size = 9, hjust = 0, margin = margin(t = 6)),
        plot.caption.position = "plot", legend.position = "right",
        legend.text = element_text(colour = BEE_INK$secondary))
ggsave(file.path(OUT_REPORT, "records_by_family_pie.png"), g, width = 8, height = 5.6, dpi = 200, bg = "white")
message("Wrote records_by_family_pie.png + records_by_family.csv to ", OUT_REPORT)
