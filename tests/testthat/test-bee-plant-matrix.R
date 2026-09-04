# Which plant genera does each bee SPECIES visit? A presence matrix: one row per bee
# species, one column per plant genus, a check where that bee has been recorded on
# that plant. Pooled across both methods (photo + specimen) because the question is
# "what does this bee use", not "how hard did we look".

BPM_SOURCED_FOR_HELPERS <- TRUE   # helpers only; do not run the build
src("analysis/bee_plant_matrix.R")

# The reference lookup (sd_bee_taxonomy_lookup_generated.csv) is the authority: records carry a
# taxon_id, and the lookup turns it into a canonical genus + species. That is how a
# subspecies rolls up to its species -- its own row in the lookup names the parent
# species -- rather than by chopping strings.
lk <- function() data.frame(
  taxon_id        = c(10L, 11L, 12L, 13L),
  scientific_name = c("Bombus vosnesenskii","Andrena baeriae","Anthophora urbana ssp. clementina","Halictus"),
  rank            = c("species","species","subspecies","genus"),
  genus           = c("Bombus","Andrena","Anthophora","Halictus"),
  species         = c("vosnesenskii","baeriae","urbana",""),
  stringsAsFactors = FALSE)

recs <- function() data.frame(
  taxon_id    = c(10L, 10L, 11L, 13L, 99L, 12L),
  taxon_rank  = c("species","species","species","genus","species","subspecies"),
  plant_genus = c("Encelia","Salvia","Encelia","Encelia","", "Salvia"),
  stringsAsFactors = FALSE)

test_that("only species-level records with a plant become pairs", {
  p <- bpm_pairs(recs(), lk())
  # dropped: the genus-only Halictus row, and the Bombus crotchii row with no plant
  expect_setequal(p$bee, c("Bombus vosnesenskii", "Andrena baeriae", "Anthophora urbana"))
  expect_false("Halictus" %in% p$bee)
})

test_that("a subspecies is rolled up to its species", {
  p <- bpm_pairs(recs(), lk())
  expect_true("Anthophora urbana" %in% p$bee)
})

test_that("repeat visits are counted, not duplicated", {
  d <- recs(); d <- rbind(d, d[1, ])              # a second Bombus-on-Encelia record
  p <- bpm_pairs(d, lk())
  expect_equal(p$n[p$bee == "Bombus vosnesenskii" & p$plant == "Encelia"], 2L)
  expect_equal(nrow(p[p$bee == "Bombus vosnesenskii", ]), 2L)   # still 2 plants, not 3 rows
})

test_that("the matrix has every bee as a row and every plant as a column", {
  m <- bpm_matrix(bpm_pairs(recs(), lk()))
  expect_equal(nrow(m), 3L)                                  # 3 bee species
  expect_setequal(setdiff(names(m), "bee"), c("Encelia", "Salvia"))
})

test_that("cells carry the visit count, zero where never recorded", {
  m <- bpm_matrix(bpm_pairs(recs(), lk()))
  expect_equal(m$Encelia[m$bee == "Bombus vosnesenskii"], 1L)
  expect_equal(m$Salvia[m$bee == "Andrena baeriae"], 0L)     # never seen on Salvia
})

test_that("rows are ordered by how many plants the bee uses, then name", {
  m <- bpm_matrix(bpm_pairs(recs(), lk()))
  expect_equal(m$bee[1], "Bombus vosnesenskii")              # the only 2-plant bee
})

test_that("empty input gives an empty matrix rather than an error", {
  e <- recs()[0, ]
  expect_equal(nrow(bpm_pairs(e, lk())), 0L)
  expect_equal(nrow(bpm_matrix(bpm_pairs(e, lk()))), 0L)
})


# ---- unpublished bees: no taxon_id, so the NAME is the only key ---------------
# 17 checklist bees have no taxon_id because iNaturalist has not published a taxon
# for them (Lasioglossum turgiventre, pilosifrons, Z17, six Hesperapis, ...). They
# are real bees on the CABR checklist, and turgiventre carries flower records. An
# id-only join drops them silently, so the id join falls back to the name -- matched
# against the SAME reference table's scientific_name, never an arbitrary string.
# TWO id-less rows, and the decoy comes FIRST: match(NA, lookup$taxon_id) matches NA
# to NA and would silently return this row for ANY id-less record. The real lookup has
# 17 such rows, so a naive id join relabels an unpublished bee as whichever one leads.
lk_unpub <- function() rbind(lk(), data.frame(
  taxon_id        = c(NA_integer_, NA_integer_),
  scientific_name = c("Protandrena atripes", "Lasioglossum turgiventre"),
  rank            = c("species", "species"),
  genus           = c("Protandrena", "Lasioglossum"),
  species         = c("atripes", "turgiventre"),
  stringsAsFactors = FALSE))

recs_unpub <- function() rbind(recs(), data.frame(
  taxon_id    = c(NA_integer_, NA_integer_),
  taxon_rank  = c("species", "species"),
  plant_genus = c("Encelia", "Salvia"),
  stringsAsFactors = FALSE))

test_that("a bee with no taxon_id is kept by falling back to its name", {
  d <- recs_unpub(); d$bee_name <- c(rep(NA_character_, 6), rep("Lasioglossum turgiventre", 2))
  p <- bpm_pairs(d, lk_unpub(), name_col = "bee_name")
  expect_true("Lasioglossum turgiventre" %in% p$bee)
  expect_setequal(p$plant[p$bee == "Lasioglossum turgiventre"], c("Encelia", "Salvia"))
})

test_that("the name fallback only accepts names the reference table knows", {
  d <- recs_unpub(); d$bee_name <- c(rep(NA_character_, 6), "Lasioglossum turgiventre", "Bogus notabee")
  p <- bpm_pairs(d, lk_unpub(), name_col = "bee_name")
  expect_true("Lasioglossum turgiventre" %in% p$bee)
  expect_false("Bogus notabee" %in% p$bee)   # not in the lookup -> not invented
})

test_that("taxon_id still wins when a record has both an id and a name", {
  d <- recs_unpub()
  d$bee_name <- c("WRONG name", rep(NA_character_, 7))   # row 1 has taxon_id 10
  p <- bpm_pairs(d, lk_unpub(), name_col = "bee_name")
  expect_true("Bombus vosnesenskii" %in% p$bee)
  expect_false("WRONG name" %in% p$bee)
})

test_that("an id-less record is never matched to an id-less LOOKUP row", {
  # match(NA, x) matches NA to NA -- the id join must skip missing ids entirely,
  # or every unpublished bee inherits the name of the first id-less lookup row.
  d <- recs_unpub(); d$bee_name <- c(rep(NA_character_, 6), rep("Lasioglossum turgiventre", 2))
  p <- bpm_pairs(d, lk_unpub(), name_col = "bee_name")
  expect_false("Protandrena atripes" %in% p$bee)
  expect_true("Lasioglossum turgiventre" %in% p$bee)
})

test_that("an id-less record with no name at all is dropped, not mislabeled", {
  d <- recs_unpub(); d$bee_name <- NA_character_
  p <- bpm_pairs(d, lk_unpub(), name_col = "bee_name")
  expect_false("Protandrena atripes" %in% p$bee)
  expect_false(any(is.na(p$bee)))
})

# ---- above-genus "plants" are not plants ---------------------------------------
# Some records identify the flower only as Angiospermae (subphylum) or Tracheophyta
# (phylum), meaning the observer could not name the plant. Those are not plant genera
# and must not sit in a genus list, where they read as a real association and inflate
# the genus count. Rank comes from the PLANT reference table, never from the string.
pl_lk <- function() data.frame(
  taxon_id        = c(1L, 2L, 3L, 4L),
  scientific_name = c("Encelia", "Salvia", "Angiospermae", "Tracheophyta"),
  genus           = c("Encelia", "Salvia", "", ""),
  rank            = c("genus", "genus", "subphylum", "phylum"),
  stringsAsFactors = FALSE)

test_that("above-genus flower identifications are dropped", {
  d <- recs(); d$plant_genus <- c("Encelia","Angiospermae","Encelia","Encelia","","Tracheophyta")
  p <- bpm_pairs(d, lk(), plant_lookup = pl_lk())
  expect_false("Angiospermae" %in% p$plant)
  expect_false("Tracheophyta" %in% p$plant)
  expect_true("Encelia" %in% p$plant)
})

test_that("a plant the reference table does not list is kept, not silently dropped", {
  # only ABOVE-genus ranks are excluded; an unknown genus is a data gap to keep visible
  d <- recs(); d$plant_genus <- c("Encelia","Notinlookup","Encelia","Encelia","","Salvia")
  p <- bpm_pairs(d, lk(), plant_lookup = pl_lk())
  expect_true("Notinlookup" %in% p$plant)
})

test_that("without a plant lookup nothing is filtered", {
  d <- recs(); d$plant_genus <- c("Encelia","Angiospermae","Encelia","Encelia","","Salvia")
  expect_true("Angiospermae" %in% bpm_pairs(d, lk())$plant)
})
