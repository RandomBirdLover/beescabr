# Per-folder WHAT_THESE_FILES_ARE.txt ------------------------------------------
#
# data/analysis/WHAT_THESE_FILES_ARE.txt explains the tree as a whole. It cannot
# explain a folder that holds eight things at once, and a stranger opening
# phenology/ or interactions/networks/ directly never sees it.
#
# So: a short note IN the folders that hold several different analyses. NOT in
# every folder -- a folder with one analysis already explains itself through the
# <analysis>_findings.csv sitting beside its outputs, and a second file there is
# noise. FOLDER_NOTES below is the whole "where it matters" decision.
#
# GENERATED, not hand-written, because these live inside 2026_generated/ where
# nothing is hand-maintained. The blurb is declared here; the list of analyses and
# their headline results is read back from the findings files, so it cannot go
# stale when an analysis is added, renamed or retired.
#
# Pure text builder tested in tests/testthat/test-folder-readmes.R.

WRAP <- 78

# folder (relative to the season folder) -> what it is for, in one or two sentences
FOLDER_NOTES <- list(
  "coverage" = paste(
    "How well the park has been sampled, and where the holes are. Each subfolder",
    "asks one coverage question: which bees still need a photo or a voucher, which",
    "transects are under-visited, and how much of the county checklist we hold."),
  "interactions" = paste(
    "Which bees visit which plants. The networks subfolder holds the whole web and",
    "its statistics; top_plants ranks the plants that carry the most bee visits."),
  "method_comparison" = paste(
    "Netting versus photographing. Everything here is restricted to a like-for-like",
    "window so the two methods are compared on equal footing rather than on how many",
    "records each happened to produce."),
  "phenology" = paste(
    "When things happen through the year: when each bee genus is active, when the",
    "plants bloom, and when we actually surveyed. The effort files matter because a",
    "bee can only look seasonal if somebody was out looking in that season."),
  "reference" = paste(
    "Lookups and deliverables rather than analyses: the field guide, the transect",
    "maps, the conservation write-ups, and the summary tables NPS asked for."),
  "richness" = paste(
    "How many kinds of bee, and how that count depends on effort. accumulation asks",
    "whether we are still finding new ones, rarefaction compares groups at equal",
    "effort, and diversity looks at how evenly the community is spread."),
  "richness/rarefaction" = paste(
    "Richness compared at EQUAL effort. Raw counts cannot compare two groups that",
    "were not sampled equally, so every number here is levelled first. The files in",
    "this folder cover transects and years; the fair_ subfolders hold the two method",
    "and observer comparisons."),
  "richness/diversity" = paste(
    "Community structure rather than a headcount: how evenly records are spread",
    "across species (Pielou evenness), which species dominate (rank-abundance), and",
    "whether transects or years differ in composition (NMDS + PERMANOVA)."),
  "interactions/networks" = paste(
    "The bee-plant web. Matrices are the raw who-visited-what; the network figures",
    "draw it; the specialization files say which bees are picky and which are not.",
    "Genus and species versions of the same thing sit side by side."),
  "reference/nps_summary" = paste(
    "The tables NPS asked for, formatted for the report rather than for analysis:",
    "checklists at each rank, headline counts, methods, and who took part."),
  "reference/conservation" = paste(
    "The IUCN-listed bees and the plants that carry them. The rare_flower_ figures",
    "are one per rare bee, each showing the plants it was recorded on."),
  "method_comparison/yield" = paste(
    "What each method actually produced: how many records, how many taxa, and how",
    "often a record could be identified to species."),
  "coverage/records_by_evidence" = paste(
    "How strong the evidence is behind each taxon: a specimen voucher, a",
    "research-grade photo, or a photo still waiting on an ID."),
  "coverage/bee_bounties" = paste(
    "The wanted list. Which bees are still missing the kind of evidence they need,",
    "so a surveyor knows what to look for and whether to net it or photograph it.")
)

# subfolder names that repeat across the tree, explained once
SUBFOLDER_NOTES <- c(
  "website" = paste("the HTML version of this analysis, published to the public site.",
                    "Same numbers as the CSV beside it."),
  "fair_method_2021_2023" = paste("the like-for-like window for comparing lethal and",
                    "non-lethal surveys: survey records only, Mar-Oct 2021-2023."),
  "fair_observer_2024" = paste("the like-for-like window for comparing beeple with",
                    "interns: May-Sep 2024, when both groups were photographing."),
  "genus_species_webs" = "one flower-web figure per bee genus."
)

.wrap <- function(x, indent = "", exdent = NULL) {
  if (is.null(exdent)) exdent <- indent
  paste(strwrap(x, width = WRAP, prefix = indent, initial = indent), collapse = "\n")
}

.first_sentence <- function(x) {
  if (!nzchar(x)) return("")
  # a findings headline can run several sentences; the folder note wants the first
  m <- regexpr("^.*?[.!?](\\s|$)", x)
  s <- if (m > 0) regmatches(x, m) else x
  trimws(s)
}

# findings: list of list(name =, finding =). subdirs: character vector of names.
# hints: named character, subfolder -> one line describing it. A hub folder like
# coverage/ is almost nothing BUT subfolders, so a bare list of names there is
# useless; the hint is read out of the findings files inside each one.
folder_readme_text <- function(rel, what, findings, subdirs, hints = character(0)) {
  out <- c("WHAT THESE FILES ARE", strrep("=", 20), "",
           .wrap(paste0("Folder: ", rel)), "", .wrap(what), "")

  if (length(findings)) {
    out <- c(out, "THE ANALYSES IN HERE", strrep("-", 20), "")
    for (f in findings)
      out <- c(out, .wrap(f$name, indent = "  "),
                    .wrap(.first_sentence(f$finding), indent = "      "), "")
  }

  if (length(subdirs)) {
    out <- c(out, "SUBFOLDERS", strrep("-", 10), "")
    for (d in sort(subdirs)) {
      note <- if (d %in% names(SUBFOLDER_NOTES)) SUBFOLDER_NOTES[[d]]
              else if (d %in% names(hints)) unname(hints[[d]]) else ""
      out <- c(out, .wrap(paste0(d, "/"), indent = "  "))
      if (nzchar(note)) out <- c(out, .wrap(note, indent = "      "))
      out <- c(out, "")
    }
  }

  out <- c(out,
    .wrap(paste("Every file here is regenerated by",
                "Rscript scripts/run_all_analysis_pipeline.R -- nothing in this folder is",
                "hand-maintained. If a number looks wrong, the fix is in the script that",
                "wrote it.")),
    "",
    .wrap("The whole tree is explained in data/analysis/WHAT_THESE_FILES_ARE.txt."))
  paste0(paste(out, collapse = "\n"), "\n")
}

# ---- writer ------------------------------------------------------------------
# reads each folder's findings CSVs for the headline results, then writes the note
.findings_in <- function(dir) {
  fs <- list.files(dir, pattern = "_findings\\.csv$", full.names = TRUE)
  lapply(sort(fs), function(f) {
    d <- try(read.csv(f, stringsAsFactors = FALSE), silent = TRUE)
    if (inherits(d, "try-error") || !all(c("field", "value") %in% names(d))) return(NULL)
    g <- function(k) { v <- d$value[d$field == k]; if (length(v)) v[1] else "" }
    list(name = g("analysis name"), finding = g("key_finding"))
  }) |> Filter(f = Negate(is.null))
}

write_folder_readmes <- function(root) {
  n <- 0
  for (rel in names(FOLDER_NOTES)) {
    dir <- file.path(root, rel)
    if (!dir.exists(dir)) next
    subs <- basename(list.dirs(dir, recursive = FALSE))
    hints <- vapply(subs, function(d) {
      f <- .findings_in(file.path(dir, d))
      if (!length(f)) return("")
      # the analyses inside are what that subfolder IS
      paste(vapply(f, function(x) x$name, character(1)), collapse = "; ")
    }, character(1))
    txt  <- folder_readme_text(rel, FOLDER_NOTES[[rel]], .findings_in(dir), subs,
                               hints = hints[nzchar(hints)])
    writeLines(txt, file.path(dir, "WHAT_THESE_FILES_ARE.txt"))
    n <- n + 1
  }
  invisible(n)
}
