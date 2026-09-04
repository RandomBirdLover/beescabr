# The join rule, enforced by OUTCOME rather than by style.
#
# CLAUDE.md says: join on taxon_id, then fall back to the NAME, because 17 real
# bees off the county checklist have no iNaturalist taxon at all. An id-only join
# silently drops them -- they do not error, they just stop existing.
#
# Writing that down in CLAUDE.md and in two folder notes does not stop anyone
# writing match(df$taxon_id, lookup$taxon_id) and shipping it. This checks the thing
# the rule exists to protect: a bee with no id, but with evidence at Cabrillo, must
# still reach the official checklist. However a script does its join, if it drops
# those bees this fails.
.root <- file.path("..", "..")
.rd <- function(p) {
  f <- file.path(.root, p)
  if (!file.exists(f)) skip(paste("not built yet:", p))
  read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
}
blank <- function(x) is.na(x) | trimws(as.character(x)) == ""

test_that("the lookup still carries taxa that have no iNaturalist id", {
  lk <- .rd("data/reference/sd_bee_taxonomy_lookup_generated.csv")
  n  <- sum(blank(lk$taxon_id))
  # a canary: if this hits zero, either iNat published them or something is dropping
  # them upstream -- both worth knowing about
  expect_gt(n, 0)
})

test_that("an id-less bee WITH Cabrillo evidence still reaches the official checklist", {
  lk <- .rd("data/reference/sd_bee_taxonomy_lookup_generated.csv")
  ck <- .rd("data/checklists/cabr/cabr_official_native_bee_checklist_generated.csv")
  sp <- .rd("data/specimens/specimens_clean/cabr_specimen_bee_clean_generated.csv")

  # every id-less taxon that a CABR specimen actually records
  recorded <- unique(trimws(paste(sp$genus, sp$species)))
  idless   <- lk$scientific_name[blank(lk$taxon_id)]
  should   <- intersect(idless, recorded)
  skip_if(length(should) == 0, "no id-less taxon is currently recorded at CABR")

  missing <- setdiff(should, ck$scientific_name)
  expect_equal(missing, character(0),
               info = paste("id-less bees with CABR evidence dropped from the checklist:",
                            paste(missing, collapse = ", ")))
})

test_that("a checklist row is allowed to have no taxon_id", {
  # the shape of the fix: the column is nullable on purpose. A schema or a join that
  # required an id would exclude exactly these bees.
  ck <- .rd("data/checklists/cabr/cabr_official_native_bee_checklist_generated.csv")
  expect_true(any(blank(ck$taxon_id)))
})
