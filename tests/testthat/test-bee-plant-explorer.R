# Two directed views over the same bee x plant pairs: pick a bee and see its plant
# genera, or pick a plant genus and see the bees on it. The second direction is the
# restoration question ("what should we plant?") and a matrix hides it.

BPE_SOURCED_FOR_HELPERS <- TRUE
src("analysis/bee_plant_explorer.R")

pairs <- function() data.frame(
  bee   = c("Bombus vosnesenskii","Bombus vosnesenskii","Andrena baeriae","Halictus ligatus"),
  plant = c("Encelia","Salvia","Encelia","Encelia"),
  n     = c(9L, 2L, 1L, 4L),
  stringsAsFactors = FALSE)

test_that("bee -> plants lists every plant genus for that bee", {
  ix <- bpe_index(pairs())
  expect_setequal(ix$by_bee[["Bombus vosnesenskii"]]$name, c("Encelia", "Salvia"))
  expect_equal(ix$by_bee[["Andrena baeriae"]]$name, "Encelia")
})

test_that("plant -> bees is the reverse direction, not a rebuild", {
  ix <- bpe_index(pairs())
  expect_setequal(ix$by_plant[["Encelia"]]$name,
                  c("Bombus vosnesenskii", "Andrena baeriae", "Halictus ligatus"))
  expect_equal(ix$by_plant[["Salvia"]]$name, "Bombus vosnesenskii")
})

test_that("rows are ordered by record count, strongest evidence first", {
  ix <- bpe_index(pairs())
  expect_equal(ix$by_bee[["Bombus vosnesenskii"]]$name, c("Encelia", "Salvia"))
  expect_equal(ix$by_plant[["Encelia"]]$name,
               c("Bombus vosnesenskii", "Halictus ligatus", "Andrena baeriae"))
})

test_that("the record count rides along with every row", {
  ix <- bpe_index(pairs())
  expect_equal(ix$by_bee[["Bombus vosnesenskii"]]$n, c(9L, 2L))
  expect_equal(ix$by_plant[["Encelia"]]$n, c(9L, 4L, 1L))
})

test_that("ties break by name so the page is stable between runs", {
  p <- data.frame(bee = c("B one","B two"), plant = c("Zinnia","Zinnia"),
                  n = c(3L, 3L), stringsAsFactors = FALSE)
  expect_equal(bpe_index(p)$by_plant[["Zinnia"]]$name, c("B one", "B two"))
})

test_that("an empty pair set yields empty indexes rather than an error", {
  ix <- bpe_index(pairs()[0, ])
  expect_length(ix$by_bee, 0)
  expect_length(ix$by_plant, 0)
})

# A bee known from ONE record is not a bee with a plant preference. The page must be
# able to say so, so single-record links are marked rather than left to look equal.
test_that("evidence strength is reported per row", {
  ix <- bpe_index(pairs())
  expect_false(ix$by_bee[["Bombus vosnesenskii"]]$thin[1])   # 9 records
  expect_true(ix$by_bee[["Andrena baeriae"]]$thin[1])        # 1 record
})

# --- sidebar order -----------------------------------------------------------
# The list was alphabetical, which buries the plants that actually carry the
# data. Order by how many partner taxa each entry has, most first, so the
# widely-visited plants and the generalist bees are at the top. Ties break
# alphabetically so a rerun cannot reshuffle them.

test_that("entries are ordered by how many partners they have, most first", {
  ix <- bpe_index(data.frame(
    bee   = c("A one", "A two", "A three", "B one", "C one"),
    plant = c("Salvia", "Salvia", "Salvia",  "Rhus",  "Acmispon"),
    n     = c(4L, 2L, 1L, 9L, 3L), stringsAsFactors = FALSE))
  expect_equal(bpe_order(ix$by_plant), c("Salvia", "Acmispon", "Rhus"))
})

test_that("a tie falls back to alphabetical, so reruns are stable", {
  ix <- bpe_index(data.frame(
    bee   = c("A one", "B one"), plant = c("Zauschneria", "Acmispon"),
    n     = c(1L, 1L), stringsAsFactors = FALSE))
  expect_equal(bpe_order(ix$by_plant), c("Acmispon", "Zauschneria"))
})

test_that("it works in the bee direction too", {
  ix <- bpe_index(data.frame(
    bee   = c("Bee one", "Bee one", "Bee two"),
    plant = c("Salvia", "Rhus", "Salvia"),
    n     = c(1L, 1L, 5L), stringsAsFactors = FALSE))
  expect_equal(bpe_order(ix$by_bee), c("Bee one", "Bee two"))
})

test_that("an empty index gives an empty order, not an error", {
  expect_equal(bpe_order(list()), character(0))
})
