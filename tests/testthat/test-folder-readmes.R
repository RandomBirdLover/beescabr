# A per-folder WHAT_THESE_FILES_ARE.txt, but only in folders that hold several
# different things. A folder with one analysis already explains itself through the
# findings CSV sitting beside its outputs; a note there would be noise.
source(file.path("..", "..", "scripts", "analysis", "folder_readmes.R"))

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
