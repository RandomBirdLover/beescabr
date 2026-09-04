# MANUAL_INPUTS.md is the list of files a person must keep correct. It had drifted
# in both directions: the calendar row gave an elided path ("../beeple_calendar_windows/")
# that does not resolve, and two _manual files were missing entirely -- including
# master_crosswalk_manual.csv, the single file that decides how every tag and
# observation field is handled.
#
# A file nobody knows they maintain is a file that goes stale.
.root <- .beescabr_root()
.doc  <- paste(readLines(file.path(.root, "dev-docs", "MANUAL_INPUTS.md"), warn = FALSE),
               collapse = "\n")

test_that("every _manual file on disk is named in the doc", {
  files <- list.files(file.path(.root, "data"), pattern = "_manual.*[.]csv$",
                      recursive = TRUE)
  skip_if(!length(files), "no data/ on this machine")
  missing <- basename(files)[!vapply(basename(files),
                                     function(f) grepl(f, .doc, fixed = TRUE), logical(1))]
  expect_equal(sort(unique(missing)), character(0),
               info = paste("hand-maintained but undocumented:",
                            paste(unique(missing), collapse = ", ")))
})

test_that("every folder path the doc gives actually exists", {
  skip_if(!dir.exists(file.path(.root, "data")), "no data/ on this machine")
  paths <- unique(unlist(regmatches(.doc, gregexpr("`[a-z_]+/[a-z_/]*/`", .doc))))
  paths <- gsub("`", "", paths)
  bad <- paths[!vapply(paths, function(p)
    dir.exists(file.path(.root, "data", p)), logical(1))]
  expect_equal(sort(bad), character(0),
               info = paste("path in the doc does not exist:", paste(bad, collapse = ", ")))
})
