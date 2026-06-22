# =============================================================
# Dorey et al. (2023) / BeeBDC — San Diego County Filter
# Created: June 21, 2026
# Author: Brandi Sanchez
#
# *** THIS IS A ONE-TIME / MANUAL ACQUISITION SCRIPT ***
# *** NOT part of the automatic native_bee_data_analysis.Rmd pipeline ***
#
# Unlike native_bee_checklist.R, clean_specimens.R, etc. (which re-run
# automatically whenever their source data changes), this script
# processes a STATIC, published global dataset that won't change
# unless Dorey et al. release a new dataset version. Re-run this
# manually only if:
#   - You want to change the geographic filter (bounding box, state, etc.)
#   - Dorey et al. publish an updated version of the dataset
#
# Data: Dorey, J.B. et al. (2023). A globally synthesised and flagged
#       bee occurrence dataset and cleaning workflow. Scientific Data.
#       Source data (figshare): https://doi.org/10.25451/flinders.21709757
#       File needed: OutputData/05_cleaned_database.csv (~6.9M rows globally)
#
# Prerequisite: Manually download 05_cleaned_database.csv from the
# figshare link above and place it in this folder before running:
#       data/reference_exports/dorey_2023/
#
# RAM note: the package's own documentation recommends ~32 GB RAM for
# the full global dataset. This script filters to the US immediately
# during the read (not after) to keep peak memory usage as low as
# possible on smaller machines.
#
# Per project data rules: this is REFERENCE CONTEXT ONLY (same
# category as GBIF). It does NOT merge into SD_inat_bee_checklist.csv
# or any CABR survey checklist — see README "Conceptualize full
# checklist architecture" TODO for the planned future merge.
#
# Output: dorey_bees_sdcounty_filtered.csv in this same folder.
# After this runs successfully, the large raw 05_cleaned_database.csv
# can be deleted to save disk space — this script's filtered output
# is all that's needed going forward.
# =============================================================

library(data.table)  # far more memory-efficient than read.csv for this size
library(dplyr)

# ------------------------------------------------------------
# STEP 1: Set the path to the raw downloaded file
# Update this if you put it somewhere other than dorey_2023/
# ------------------------------------------------------------
raw_path <- "data/reference_exports/dorey_2023/05_cleaned_database.csv"

if (!file.exists(raw_path)) {
  stop(
    "Raw file not found at '", raw_path, "'.\n",
    "Download 05_cleaned_database.csv from https://doi.org/10.25451/flinders.21709757\n",
    "and place it in data/reference_exports/dorey_2023/ before running this script."
  )
}

# ------------------------------------------------------------
# STEP 2: Read + filter to United States immediately
# Filtering during the same step (not after a full read) keeps
# peak memory lower — we never hold the full global object at once.
# ------------------------------------------------------------
cat("Reading and filtering to United States (this may take a few minutes)...\n")

dorey_us <- fread(raw_path)[country == "United States"]

cat("US records:", nrow(dorey_us), "\n")

# ------------------------------------------------------------
# STEP 3: Narrow to California / San Diego County bounding box
# Bounding box is approximate. For precision later, intersect with
# an actual SD County boundary shapefile instead (spatial/boundaries/
# is currently empty).
# ------------------------------------------------------------
dorey_sd <- dorey_us[
  stateProvince == "California" &
  decimalLatitude  >= 32.53 & decimalLatitude  <= 33.51 &
  decimalLongitude >= -117.6 & decimalLongitude <= -116.08
]

cat("San Diego County bounding box records:", nrow(dorey_sd), "\n")

# ------------------------------------------------------------
# STEP 4: Quick summary for sanity checking
# ------------------------------------------------------------
cat("\n--- DOREY ET AL. (2023) SD COUNTY SUMMARY ---\n")
cat("Total filtered records:", nrow(dorey_sd), "\n")
cat("Distinct species:      ", n_distinct(dorey_sd$species), "\n")
cat("Distinct genera:       ", n_distinct(dorey_sd$genus), "\n")

# ------------------------------------------------------------
# STEP 5: Save filtered output
# ------------------------------------------------------------
out_path <- "data/reference_exports/dorey_2023/dorey_bees_sdcounty_filtered.csv"

write.csv(dorey_sd, out_path, row.names = FALSE)

cat("\nFiltered data saved to", out_path, "\n")
cat("You can now delete the large raw 05_cleaned_database.csv if disk space matters —\n")
cat("this filtered file is all that's needed going forward.\n")
