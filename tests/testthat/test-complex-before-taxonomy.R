# The complex must be known BEFORE identity is resolved.
#
# clean_specimens() used to assign taxon_id at line ~178 and only then work out the
# complex at line ~227. A complex supplied by the complex map -- rather than typed
# into the workbook -- therefore arrived 49 lines too late to be used, so a specimen
# the map could have placed stayed unresolved. Nothing was visibly broken because
# every complex in the workbook happens to be hand-typed; this is the trap that sets.
src("specimens/specimen_clean_helpers.R")

lookup <- data.frame(
  taxon_id = c(127741L, 900001L),
  rank     = c("genus", "complex"),
  scientific_name = c("Colletes", "Colletes hyalinus"),
  class = "Insecta", order = "Hymenoptera", family = "Colletidae",
  genus = "Colletes", subgenus = "", complex = c("", "Colletes hyalinus"),
  species = "", subspecies = "", stringsAsFactors = FALSE)

cmap <- data.frame(genus = "Colletes", species = "slevini", complex = "Colletes hyalinus",
                   complex_taxon_id = 900001L, stringsAsFactors = FALSE)

test_that("the complex map can place a specimen the workbook left blank", {
  # a row with NO hand-typed complex: the map supplies it from genus+species
  row <- data.frame(genus = "Colletes", subgenus = "", complex = NA_character_,
                    species = "slevini", subspecies = "", stringsAsFactors = FALSE)
  withcx <- match_specimen_complex(row, build_complex_lookup(cmap))
  expect_true(grepl("hyalinus", withcx$complex))
})

test_that("running the complex step FIRST lets that complex resolve an id", {
  row <- data.frame(genus = "Colletes", subgenus = "", complex = NA_character_,
                    species = "slevini", subspecies = "",
                    class = NA_character_, order = NA_character_, family = NA_character_,
                    stringsAsFactors = FALSE)
  # complex first, then identity -- the order clean_specimens() must use
  withcx <- match_specimen_complex(row, build_complex_lookup(cmap))
  withcx$species <- ""                      # a complex-only ID carries no species
  out <- fill_coarse_ids(withcx, lookup)
  expect_equal(out$taxon_id, 900001L)
  expect_equal(out$taxon_rank, "complex")
  expect_equal(out$class, "Insecta")
})

test_that("clean_specimens resolves the complex before it resolves taxonomy", {
  txt <- readLines(file.path("..", "..", "scripts", "specimens", "specimen_bee_clean.R"),
                   warn = FALSE)
  code <- txt[!grepl("^\\s*#", txt)]      # comments MENTION both; only the calls count
  cx   <- grep("match_specimen_complex\\(", code)[1]
  tax  <- grep("attach_lookup_taxonomy\\(", code)[1]
  expect_false(is.na(cx)); expect_false(is.na(tax))
  expect_lt(cx, tax)
})
