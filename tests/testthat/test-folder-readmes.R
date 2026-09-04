# A per-folder WHAT_THESE_FILES_ARE.txt, but only in folders that hold several
# different things. A folder with one analysis already explains itself through the
# findings CSV sitting beside its outputs; a note there would be noise.
source(file.path("..", "..", "scripts", "analysis", "shared", "folder_readmes.R"))

fnd <- list(
  list(name = "Bee activity by month", finding = "Most genera peak Apr-Jun. Two are autumn-only."),
  list(name = "Survey effort by month", finding = "Effort is Mar-Oct heavy and 2024-heavy."))

test_that("the note leads with what the folder is for", {
  txt <- folder_readme_text("phenology", "When bees and plants are active.", fnd, character(0))
  expect_match(txt, "WHAT THESE FILES ARE", fixed = TRUE)
  expect_match(txt, "When bees and plants are active.", fixed = TRUE)
})

test_that("it lists the analyses in the folder with their headline result", {
  txt <- folder_readme_text("phenology", "x", fnd, character(0))
  expect_match(txt, "Bee activity by month", fixed = TRUE)
  expect_match(txt, "Most genera peak Apr-Jun", fixed = TRUE)
  expect_match(txt, "Survey effort by month", fixed = TRUE)
})

test_that("a long headline is trimmed to its first sentence, not dumped whole", {
  long <- list(list(name = "A", finding = paste0("First sentence here. ", strrep("tail ", 90))))
  txt <- folder_readme_text("x", "y", long, character(0))
  expect_match(txt, "First sentence here.", fixed = TRUE)
  expect_lt(max(nchar(strsplit(txt, "\n")[[1]])), 90)
})

test_that("known subfolders are explained, not just named", {
  txt <- folder_readme_text("richness/rarefaction", "x", fnd,
                            c("fair_method_2021_2023", "fair_observer_2024"))
  expect_match(txt, "fair_method_2021_2023", fixed = TRUE)
  expect_match(txt, "lethal", ignore.case = TRUE)
  expect_match(txt, "fair_observer_2024", fixed = TRUE)
  expect_match(txt, "2024", fixed = TRUE)
})

test_that("an unknown subfolder is still named, so nothing is silently hidden", {
  txt <- folder_readme_text("x", "y", fnd, c("some_new_thing"))
  expect_match(txt, "some_new_thing", fixed = TRUE)
})

test_that("a hub folder describes each subfolder, since that is all it holds", {
  # coverage/ is 8 subfolders and almost no files: a bare list of names is the one
  # thing a note there must not be
  hints <- c(bee_bounties = "Which bees still need a photo or a voucher",
             footprint    = "How much of the park has been walked")
  txt <- folder_readme_text("coverage", "x", list(),
                            c("bee_bounties", "footprint"), hints = hints)
  expect_match(txt, "Which bees still need a photo or a voucher", fixed = TRUE)
  expect_match(txt, "How much of the park has been walked", fixed = TRUE)
})

test_that("it says the folder is regenerated, so nobody hand-edits it", {
  txt <- folder_readme_text("x", "y", fnd, character(0))
  expect_match(txt, "regenerated|rebuilt", ignore.case = TRUE)
})

test_that("no line is wide enough to wrap in a terminal", {
  txt <- folder_readme_text("richness/rarefaction", strrep("long blurb ", 20), fnd,
                            c("fair_method_2021_2023"))
  expect_lt(max(nchar(strsplit(txt, "\n")[[1]])), 90)
})

test_that("only folders holding several things are declared", {
  expect_true(all(c("coverage", "richness", "phenology") %in% names(FOLDER_NOTES)))
  # a single-analysis leaf explains itself through its findings CSV
  expect_false("coverage/footprint" %in% names(FOLDER_NOTES))
  expect_false(any(grepl("/website$", names(FOLDER_NOTES))))
})

# ---- statistical provenance ---------------------------------------------------
# A reader has to be able to see WHICH test produced a number and WHERE that test
# comes from, without opening the script.

stat_fnd <- list(
  list(name = "Rarefaction (iNEXT)", finding = "Lethal finds more species.",
       method = "iNEXT size- and coverage-based rarefaction/extrapolation, Hill q0/q1/q2"),
  list(name = "Diversity indices", finding = "TP is the most even transect.",
       method = "Shannon and Simpson diversity, Pielou evenness, NMDS + PERMANOVA"))

test_that("each analysis shows the method that produced it", {
  txt <- folder_readme_text("richness", "x", stat_fnd, character(0))
  expect_match(txt, "iNEXT size- and coverage-based", fixed = TRUE)
  expect_match(txt, "Pielou evenness", fixed = TRUE)
})

test_that("a citation is never split across a line break", {
  txt <- folder_readme_text("richness", "x", stat_fnd, character(0))
  for (cite in c("Hsieh, Ma & Chao 2016", "Pielou 1966", "Anderson 2001"))
    expect_true(any(grepl(cite, strsplit(txt, "\n")[[1]], fixed = TRUE)), info = cite)
})

test_that("the note says where each method comes from", {
  txt <- folder_readme_text("richness", "x", stat_fnd, character(0))
  expect_match(txt, "Hsieh, Ma & Chao 2016", fixed = TRUE)   # iNEXT
  expect_match(txt, "Pielou 1966", fixed = TRUE)
  expect_match(txt, "Anderson", fixed = TRUE)    # PERMANOVA
})

test_that("it only cites methods that this folder actually used", {
  txt <- folder_readme_text("x", "y", stat_fnd[1], character(0))   # iNEXT only
  expect_match(txt, "iNEXT", fixed = TRUE)
  expect_false(grepl("Anderson", txt, fixed = TRUE))   # no PERMANOVA here
  expect_false(grepl("Rayleigh", txt, fixed = TRUE))
})

test_that("a statistic computed in this repo is not dressed up as a package", {
  ray <- list(list(name = "Phenology", finding = "Most genera are seasonal.",
                   method = "circular-mean ridgelines + Rayleigh test of seasonal concentration"))
  txt <- folder_readme_text("phenology", "x", ray, character(0))
  expect_match(txt, "Rayleigh", fixed = TRUE)
  expect_match(txt, "in this repo|computed here", ignore.case = TRUE)
})

test_that("a folder with no statistics gets no sources block", {
  plain <- list(list(name = "A checklist", finding = "153 species.", method = "count of records"))
  txt <- folder_readme_text("reference", "x", plain, character(0))
  expect_false(grepl("WHERE THESE METHODS COME FROM", txt, fixed = TRUE))
})

test_that("a hub folder cites the methods used anywhere beneath it", {
  # richness/ holds no analysis of its own -- everything is in accumulation/,
  # rarefaction/, diversity/. Someone browsing the branch should still see what
  # statistics the branch rests on.
  deep <- list(list(name = "Accumulation", finding = "TP is near-complete.",
                    method = "Chao2 asymptotic richness vs observed"))
  txt <- folder_readme_text("richness", "x", list(), c("accumulation"),
                            src_findings = deep)
  expect_match(txt, "Chao 1987", fixed = TRUE)
  # but the local analyses list stays local -- it has none
  expect_false(grepl("THE ANALYSES IN HERE", txt, fixed = TRUE))
})

test_that("citations match what the installed packages themselves say", {
  # checked against citation() and the packages' own Rd, not from memory
  src <- vapply(STAT_SOURCES, function(x) x$src, character(1))
  expect_true(any(grepl("Dormann.*2009", src)))          # bipartite's citation() says 2009
  expect_false(any(grepl("Dormann.*2008", src)))
  expect_true(any(grepl("Almeida-Neto.*2008", src)))     # vegan nestedtemp.Rd
  expect_true(any(grepl("Miklos & Podani 2004", src)))   # the null the p-value rests on
})
