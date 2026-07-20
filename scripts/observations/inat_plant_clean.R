# =============================================================
# observations/inat_plant_clean.R  --  STARTING FROM SCRATCH (2026-07-19)
# Previous version preserved in _to_delete/inat_plant_clean.R.bak
# It was built on the pre-reschema crosswalk (crosswalk_master / surveyors_by_year;
# columns inat_variants / category / field_id -- all renamed). Rewrite against the
# current master_crosswalk.csv + surveyor_roster.csv.
#
# PURPOSE: the cleaned output feeds ANALYSIS. NOTE: the region CHECKLISTS do NOT use this --
# they build from the RAW iNat obs clipped to each boundary box (see checklists/).
#
# TODO (rewrite): as part of cleaning the iNat PLANT data, build a QC list of problem
# observations to review/fix:
#   * MISPLACED observations -- tagged to a transect but whose GPS falls outside / too
#     far from that transect line. (The old qc_misplaced_transect.R did this; it's in
#     _to_delete/qc_misplaced_transect.R for reference.)
#   * INCORRECTLY-TAGGED observations -- carrying a wrong / unexpected survey tag.
# Write these to data/observations/inat_clean/qc/ so they can be checked & corrected.
# =============================================================
