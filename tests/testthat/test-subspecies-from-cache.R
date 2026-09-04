# Pull the subspecies iNaturalist has ALREADY given us into the lookup.
#
# Every taxon fetched from iNat is cached whole, children included. Colletes hyalinus
# (217441) has carried its two subspecies -- gaudialis 345235 and hyalinus 1052957 --
# in the local cache the entire time. The lookup never read them, so a specimen keyed
# to gaudialis had nothing to match and no id to inherit.
#
# No network: the fetcher is injected, exactly as the API code is tested.
src("reference/taxonomy/subspecies_from_cache.R")

lookup <- data.frame(
  taxon_id = c(217441L, 127741L),
  rank     = c("species", "genus"),
  scientific_name = c("Colletes hyalinus", "Colletes"),
  class = "Insecta", order = "Hymenoptera", family = "Colletidae",
  subfamily = "Colletinae", tribe = "Colletini",
  genus = "Colletes", subgenus = "", complex = "",
  species = c("hyalinus", ""), subspecies = "", stringsAsFactors = FALSE)

fake <- function(id) {
  if (identical(id, 217441L)) list(children = list(
    list(id = 345235L,  rank = "subspecies", name = "Colletes hyalinus gaudialis"),
    list(id = 1052957L, rank = "subspecies", name = "Colletes hyalinus hyalinus"),
    list(id = 999L,     rank = "variety",    name = "Colletes hyalinus var. x")))
  else NULL
}

test_that("a species' cached subspecies children become lookup rows", {
  out <- subspecies_from_cache(lookup, fake)
  ss  <- out[out$rank == "subspecies", ]
  expect_equal(nrow(ss), 2)
  expect_setequal(ss$subspecies, c("gaudialis", "hyalinus"))
  expect_true(345235L %in% ss$taxon_id)
})

test_that("only SUBSPECIES children are taken, not varieties or forms", {
  out <- subspecies_from_cache(lookup, fake)
  expect_false(999L %in% out$taxon_id)
})

test_that("a new row inherits its parent's lineage", {
  out <- subspecies_from_cache(lookup, fake)
  g <- out[out$taxon_id == 345235L, ]
  expect_equal(g$family, "Colletidae")
  expect_equal(g$tribe, "Colletini")
  expect_equal(g$genus, "Colletes")
  expect_equal(g$species, "hyalinus")
})

test_that("existing rows are never touched or duplicated", {
  out <- subspecies_from_cache(lookup, fake)
  expect_equal(nrow(out[out$rank != "subspecies", ]), 2)
  expect_equal(sum(out$taxon_id == 217441L), 1)
})

test_that("running it twice adds nothing the second time", {
  once  <- subspecies_from_cache(lookup, fake)
  twice <- subspecies_from_cache(once, fake)
  expect_equal(nrow(once), nrow(twice))
})

test_that("a species with no cache entry is simply skipped", {
  out <- subspecies_from_cache(lookup, function(id) NULL)
  expect_equal(nrow(out), nrow(lookup))
})
