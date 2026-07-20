library(testthat)
library(dplyr)

# The Holway BASE of the lookup now comes entirely from the cleaned reference
# table (holway_resolved); the raw sheet is never read for names. These helpers
# build the two "layer on top" inputs empty.
empty_checklist <- function() tibble::tibble(
  taxon_id=integer(0), scientific_name=character(0), common_name=character(0),
  kingdom=character(0), phylum=character(0), class=character(0), order=character(0),
  superfamily=character(0), family=character(0), subfamily=character(0), tribe=character(0),
  genus=character(0), subgenus=character(0), complex=character(0), complex_taxon_id=integer(0),
  species=character(0), subspecies=character(0))
empty_bees <- function() tibble::tibble(
  taxon_id=integer(0), scientific_name=character(0), common_name=character(0),
  genus=character(0), family=character(0), subfamily=character(0), tribe=character(0),
  subtribe=character(0))

test_that("lookup base is the reference table; iNat adds observed-only taxa + flags", {
  src("config.R"); src("reference/verify.R"); src("reference/holway.R"); src("reference/taxonomy_reference.R")

  ref <- tibble::tibble(
    taxon_id = 1L, scientific_name = "Melissodes robustior", rank = "species",
    source_sheet = "Described", qualifier = NA_character_, itis_valid = NA,
    family = "Apidae", subfamily = "Apinae", tribe = "Eucerini",
    genus = "Melissodes", subgenus = "Melissodes", species = "robustior", subspecies = NA_character_)

  # taxon 1: robustior, observed. taxon 2: identified only to subgenus Melissodes.
  # taxon 3: Agapostemon subtilior -- OBSERVED on iNat, NOT in Holway.
  checklist_sd <- tibble::tibble(
    taxon_id = c(1L, 2L, 3L),
    scientific_name = c("Melissodes robustior", "Melissodes", "Agapostemon subtilior"),
    common_name = NA_character_,
    kingdom="Animalia", phylum="Arthropoda", class="Insecta",
    order="Hymenoptera", superfamily="Apoidea",
    family=c("Apidae","Apidae","Halictidae"), subfamily=c("Apinae","Apinae","Halictinae"),
    tribe=c("Eucerini","Eucerini","Halictini"),
    genus=c("Melissodes","Melissodes","Agapostemon"), subgenus=c("Melissodes","Melissodes",NA),
    complex=NA_character_, complex_taxon_id=NA_integer_,
    species=c("robustior", NA_character_, "subtilior"), subspecies=NA_character_)

  bees <- tibble::tibble(
    taxon_id = 99L, scientific_name = "Halictidae", common_name = NA_character_,
    genus = NA_character_, family = "Halictidae",
    subfamily = NA_character_, tribe = NA_character_, subtribe = NA_character_)

  lookup <- build_bee_taxonomy_lookup(ref, checklist_sd, bees)

  expect_true(all(c("genus","species","subgenus","family") %in% lookup$rank))
  expect_true("Agapostemon subtilior" %in% lookup$scientific_name)
  # qualifier now sits right after holway_status; itis_valid right after it
  expect_equal(names(lookup)[1:10],
               c("taxon_id","scientific_name","common_name","rank","verified","holway_status",
                 "qualifier","itis_valid","in_holway","in_inat"))
  expect_true("subtribe" %in% names(lookup))

  mr <- filter(lookup, scientific_name == "Melissodes robustior")
  ag <- filter(lookup, scientific_name == "Agapostemon subtilior")
  expect_equal(nrow(mr), 1)   # reference row + iNat twin collapse to one
  expect_true(mr$verified[1]); expect_true(mr$in_holway[1])
  expect_true(mr$in_inat[1])
  expect_false(ag$verified[1]); expect_false(ag$in_holway[1])
  expect_true(ag$in_inat[1])
})

test_that("slash pick: only the resolved name reaches the lookup (no 'pensylvanicus' leak)", {
  src("config.R"); src("reference/verify.R"); src("reference/holway.R"); src("reference/taxonomy_reference.R")
  # Holway sheet had "pensylvanicus / sonorus"; the human picked sonorus, so THAT
  # is what the reference table holds. pensylvanicus never appears anywhere.
  ref <- tibble::tibble(
    taxon_id = 57690L, scientific_name = "Bombus sonorus", rank = "species",
    source_sheet = "Described", qualifier = NA_character_, itis_valid = NA,
    family = "Apidae", subfamily = "Apinae", tribe = "Bombini",
    genus = "Bombus", subgenus = "Thoracobombus", species = "sonorus", subspecies = NA_character_)
  checklist_sd <- empty_checklist()
  checklist_sd <- tibble::add_row(checklist_sd, taxon_id = 57690L,
    scientific_name = "Bombus sonorus", common_name = NA_character_,
    kingdom="Animalia", phylum="Arthropoda", class="Insecta", order="Hymenoptera",
    superfamily="Apoidea", family="Apidae", subfamily="Apinae", tribe="Bombini",
    genus="Bombus", subgenus="Thoracobombus", complex=NA_character_, complex_taxon_id=NA_integer_,
    species="sonorus", subspecies=NA_character_)
  lk <- build_bee_taxonomy_lookup(ref, checklist_sd, empty_bees())

  expect_false(any(grepl("pensylvanicus", lk$species, fixed = TRUE), na.rm = TRUE))
  son <- lk[lk$species == "sonorus" & !is.na(lk$species), ]
  expect_equal(nrow(son), 1)
  expect_equal(son$taxon_id[1], 57690L)
  expect_true(son$in_holway[1]); expect_true(son$in_inat[1])
})

test_that("tentative names: clean epithet in species, marker in the qualifier column", {
  src("config.R"); src("reference/verify.R"); src("reference/holway.R"); src("reference/taxonomy_reference.R")
  ref <- tibble::tibble(
    taxon_id = c(573509L, NA_integer_),
    scientific_name = c("Andrena annectens", "Lasioglossum pilosifrons"),
    rank = "species", source_sheet = c("Tentative", "Unpublished"),
    qualifier = c("CF", "MSN"), itis_valid = NA,
    family = c("Andrenidae","Halictidae"), subfamily = c("Andreninae","Halictinae"),
    tribe = c("Andrenini","Halictini"),
    genus = c("Andrena","Lasioglossum"), subgenus = c("Micandrena","Dialictus"),
    species = c("annectens","pilosifrons"), subspecies = NA_character_)
  lk <- build_bee_taxonomy_lookup(ref, empty_checklist(), empty_bees())

  ann <- lk[lk$species == "annectens" & !is.na(lk$species), ]
  expect_equal(ann$species[1], "annectens")          # CLEAN epithet, not "CF annectens"
  expect_equal(ann$qualifier[1], "CF")               # marker in its own column
  expect_equal(ann$subgenus[1], "Micandrena")        # subgenus preserved
  expect_equal(ann$taxon_id[1], 573509L)             # real iNat id
  expect_true(ann$in_holway[1])
  expect_false(ann$in_inat[1])                       # 0 observations -> not on iNat
  expect_false(any(grepl("CF ",  lk$species, fixed = TRUE), na.rm = TRUE))  # no raw leak
  expect_false(any(grepl("MSN ", lk$species, fixed = TRUE), na.rm = TRUE))

  pil <- lk[lk$species == "pilosifrons" & !is.na(lk$species), ]
  expect_equal(pil$qualifier[1], "MSN")              # MSN carried even when unresolved
  expect_true(is.na(pil$taxon_id[1]))
})

test_that("itis_valid propagates from the reference and sits right after qualifier", {
  src("config.R"); src("reference/verify.R"); src("reference/holway.R"); src("reference/taxonomy_reference.R")
  ref <- tibble::tibble(
    taxon_id = NA_integer_, scientific_name = c("Calliopsis anthidius","Calliopsis validus"),
    rank = "species", source_sheet = "Described", qualifier = NA_character_,
    itis_valid = c(FALSE, TRUE),
    family = "Andrenidae", subfamily = "Panurginae", tribe = "Calliopsini",
    genus = "Calliopsis", subgenus = NA_character_,
    species = c("anthidius","validus"), subspecies = NA_character_)
  lk <- build_bee_taxonomy_lookup(ref, empty_checklist(), empty_bees())
  expect_equal(names(lk)[1:8],
               c("taxon_id","scientific_name","common_name","rank","verified",
                 "holway_status","qualifier","itis_valid"))
  expect_false(lk$itis_valid[lk$species == "anthidius" & !is.na(lk$species)][1])
  expect_true( lk$itis_valid[lk$species == "validus"   & !is.na(lk$species)][1])
})

test_that("a resolved-but-unobserved Holway taxon keeps a taxon_id yet in_inat = FALSE", {
  src("config.R"); src("reference/verify.R"); src("reference/holway.R"); src("reference/taxonomy_reference.R")
  ref <- tibble::tibble(
    taxon_id = 111L, scientific_name = "Andrena annectens", rank = "species",
    source_sheet = "Tentative", qualifier = "CF", itis_valid = NA,
    family = "Andrenidae", subfamily = "Andreninae", tribe = "Andrenini",
    genus = "Andrena", subgenus = "Micandrena", species = "annectens", subspecies = NA_character_)
  lk <- build_bee_taxonomy_lookup(ref, empty_checklist(), empty_bees())   # not observed
  row <- lk[lk$rank == "species", ]
  expect_equal(row$taxon_id[1], 111L)
  expect_equal(row$scientific_name[1], "Andrena annectens")
  expect_false(row$in_inat[1])
  expect_true(row$in_holway[1])
})

test_that("no reference row is dropped: species/subspecies/complex/unresolved/genus all reach the lookup", {
  src("config.R"); src("reference/verify.R"); src("reference/holway.R"); src("reference/taxonomy_reference.R")
  ref <- tibble::tibble(
    taxon_id        = c(573509L, 313836L, 900L, NA_integer_, 57669L),
    scientific_name = c("Andrena annectens", "Ashmeadiella cactorum ssp. basalis",
                        "Andrena Osmioides Complex", "Stelis anthocopae", "Andrena"),
    rank        = c("species", "subspecies", "complex", "species", "genus"),
    source_sheet= "Described", qualifier = NA_character_, itis_valid = c(NA,NA,NA,TRUE,NA),
    family      = c("Andrenidae","Megachilidae","Andrenidae","Megachilidae","Andrenidae"),
    subfamily   = "s", tribe = "t",
    genus       = c("Andrena","Ashmeadiella","Andrena","Stelis","Andrena"),
    subgenus    = c("Micandrena","Ashmeadiella",NA,NA,NA),
    complex     = c(NA,NA,"Osmioides Complex",NA,NA),
    species     = c("annectens","cactorum",NA,"anthocopae",NA),
    subspecies  = c(NA,"basalis",NA,NA,NA))
  lk <- build_bee_taxonomy_lookup(ref, empty_checklist(), empty_bees())
  expect_true(573509L %in% lk$taxon_id)                 # resolved species
  expect_true(313836L %in% lk$taxon_id)                 # subspecies
  expect_true(900L %in% lk$taxon_id)                    # Described species that resolved to a complex
  expect_true(any(lk$rank == "complex"))
  expect_true("anthocopae" %in% lk$species)             # UNRESOLVED species still kept
  gen <- lk[lk$rank == "genus" & lk$genus == "Andrena", ]
  expect_equal(nrow(gen), 1)                            # exactly one Andrena genus row
  expect_equal(gen$taxon_id[1], 57669L)                # genus id folded from the reference
})

test_that("build_bee_taxonomy_lookup REQUIRES the reference table", {
  src("config.R"); src("reference/verify.R"); src("reference/holway.R"); src("reference/taxonomy_reference.R")
  expect_error(build_bee_taxonomy_lookup(NULL, empty_checklist(), empty_bees()), "reference")
  expect_error(build_bee_taxonomy_lookup(empty_checklist()[0, ], empty_checklist(), empty_bees()), "reference")
})

# ---- retained helper tests (merge_holway_resolved / decision map / reconcile
#      are still defined for reuse, though the builder no longer calls them) ----

test_that("merge_holway_resolved fills taxon_id + scientific_name on Holway rows only where missing", {
  src("config.R"); src("reference/verify.R"); src("reference/taxonomy_reference.R")
  deduped <- tibble::tibble(
    taxon_id        = c(NA_integer_, 500L, NA_integer_),
    scientific_name = c(NA_character_, "Andrena baeriae", NA_character_),
    rank            = c("species", "species", "genus"),
    genus           = c("Andrena", "Andrena", "Colletes"),
    species         = c("annectens", "baeriae", NA_character_),
    subspecies      = NA_character_)
  holway_resolved <- tibble::tibble(
    taxon_id        = c(111L, 222L, 333L),
    scientific_name = c("Andrena annectens", "Andrena baeriae", "Colletes"),
    rank            = c("species", "species", "genus"),
    genus           = c("Andrena", "Andrena", "Colletes"),
    species         = c("annectens", "baeriae", NA_character_))
  out <- merge_holway_resolved(deduped, holway_resolved)
  ann <- out[out$species == "annectens" & !is.na(out$species), ]
  expect_equal(ann$taxon_id[1], 111L)
  expect_equal(ann$scientific_name[1], "Andrena annectens")
  bae <- out[out$species == "baeriae" & !is.na(out$species), ]
  expect_equal(bae$taxon_id[1], 500L)
  col <- out[out$rank == "genus", ]
  expect_equal(col$taxon_id[1], 333L)
  expect_equal(col$scientific_name[1], "Colletes")
})

test_that("parse_holway_decision_map keys on the original genus+epithet", {
  src("config.R"); src("reference/verify.R"); src("reference/taxonomy_reference.R")
  d <- tibble::tibble(
    search_term = c("Calliopsis rhodophilus", "Ashmeadiella cactorum basalis", "Andrena"),
    chosen_taxon_id = c(271415L, 313836L, 50L))
  m <- parse_holway_decision_map(d)
  expect_equal(m$dm_id[m$.dkey == "calliopsis rhodophilus"], 271415L)
  expect_true("ashmeadiella cactorum basalis" %in% m$.dkey)
  expect_false(any(m$.dkey == "andrena"))
})

test_that("reconcile_lookup_dupes leaves non-duplicate and NA-taxon rows intact", {
  src("reference/taxonomy_reference.R")
  df <- tibble::tibble(
    taxon_id = c(1L, 2L, NA_integer_),
    scientific_name = c("Aa a", "Bb b", NA_character_),
    rank = "species", genus = c("Aa", "Bb", "Cc"),
    species = c("a", "b", "c"), subspecies = NA_character_,
    holway_status = "", verified = c(TRUE, TRUE, TRUE),
    in_holway = c(TRUE, FALSE, TRUE), in_inat = c(FALSE, TRUE, FALSE))
  expect_equal(nrow(reconcile_lookup_dupes(df)), 3)
})

test_that("merge_holway_resolved is a no-op when the reference table is absent/empty", {
  src("reference/taxonomy_reference.R")
  d <- tibble::tibble(taxon_id = NA_integer_, scientific_name = NA_character_,
                      rank = "species", genus = "Andrena", species = "annectens",
                      subspecies = NA_character_)
  expect_identical(merge_holway_resolved(d, NULL), d)
  expect_identical(merge_holway_resolved(d, d[0, ]), d)
})

# --- specimen additions: append leaf taxa, never fabricate a missing parent -------------
test_that("specimen_additions_to_lookup appends leaf species and never creates a missing parent", {
  src("config.R"); src("reference/verify.R"); src("reference/holway.R"); src("reference/taxonomy_reference.R")

  # lookup already holds family Colletidae + genus Colletes (+ a species) -- but NOT Zzyzxia/Andrenidae
  lookup <- tibble::tibble(
    taxon_id = c("10","11","12"),
    rank     = c("family","genus","species"),
    family   = c("Colletidae","Colletidae","Colletidae"),
    genus    = c(NA, "Colletes", "Colletes"),
    species  = c(NA, NA, "fulgidus"))

  additions <- tibble::tibble(
    taxon_id = c("900", NA),
    rank     = c("species","species"),
    family   = c("Colletidae","Andrenidae"),
    genus    = c("Colletes","Zzyzxia"),
    species  = c("phaceliae","weirdus"))

  res <- specimen_additions_to_lookup(lookup, additions)

  # both are new species -> both leaf rows appended, and NOTHING else
  expect_equal(nrow(res$added), 2L)
  expect_equal(nrow(res$lookup), nrow(lookup) + 2L)

  # Colletes phaceliae: parent genus Colletes + family Colletidae already exist -> no missing parents
  expect_false("phaceliae" %in% res$missing_parents$taxon)

  # Zzyzxia weirdus: genus Zzyzxia AND family Andrenidae are absent -> REPORTED, not created
  mp <- res$missing_parents[res$missing_parents$taxon == "weirdus", ]
  expect_true("Zzyzxia"    %in% mp$missing_parent_name)
  expect_true("Andrenidae" %in% mp$missing_parent_name)

  # crucially: NO genus/family row was fabricated for the missing parents
  expect_false(any(res$lookup$rank == "genus"  & res$lookup$genus  == "Zzyzxia"))
  expect_false(any(res$lookup$rank == "family" & res$lookup$family == "Andrenidae"))
})

test_that("specimen_additions_to_lookup skips an addition that already exists (dedupe)", {
  src("config.R"); src("reference/verify.R"); src("reference/holway.R"); src("reference/taxonomy_reference.R")
  lookup <- tibble::tibble(taxon_id = "12", rank = "species",
                           family = "Colletidae", genus = "Colletes", species = "fulgidus")
  additions <- tibble::tibble(taxon_id = "12", rank = "species",
                              family = "Colletidae", genus = "Colletes", species = "fulgidus")
  res <- specimen_additions_to_lookup(lookup, additions)
  expect_equal(nrow(res$added), 0L)
  expect_equal(nrow(res$lookup), 1L)
  expect_equal(nrow(res$missing_parents), 0L)
})

test_that("specimen_additions_to_lookup links a subgenus parent through paren/spelling variants", {
  src("config.R"); src("reference/verify.R"); src("reference/holway.R"); src("reference/taxonomy_reference.R")
  # lookup subgenus row stored bare; addition names it parenthesized -> should still count as existing
  lookup <- tibble::tibble(
    taxon_id = c("20","21"), rank = c("subgenus","genus"),
    genus = c("Chelostoma","Chelostoma"), subgenus = c("Neochelostoma", NA))
  additions <- tibble::tibble(
    taxon_id = "930", rank = "species",
    genus = "Chelostoma", subgenus = "(Neochelostoma)", species = "phaceliae")
  res <- specimen_additions_to_lookup(lookup, additions)
  expect_equal(nrow(res$added), 1L)
  # subgenus (Neochelostoma) already present -> not reported missing
  expect_false("subgenus" %in% res$missing_parents$missing_parent_rank)
})

test_that("specimen_additions_to_lookup treats the same epithet in different genera as distinct", {
  src("config.R"); src("reference/verify.R"); src("reference/holway.R"); src("reference/taxonomy_reference.R")
  # Colletes phaceliae (Colletidae) and Chelostoma phaceliae (Megachilidae) share the epithet
  # but are DIFFERENT bees -- both must be added, not collapsed to one.
  lookup <- tibble::tibble(
    taxon_id = c("1","2"), rank = c("genus","genus"),
    family = c("Colletidae","Megachilidae"), genus = c("Colletes","Chelostoma"))
  additions <- tibble::tibble(
    taxon_id = c("62587","540802"), rank = c("species","species"),
    family = c("Colletidae","Megachilidae"),
    genus = c("Colletes","Chelostoma"), species = c("phaceliae","phaceliae"))
  res <- specimen_additions_to_lookup(lookup, additions)
  expect_equal(nrow(res$added), 2L)                       # both phaceliae kept -- not deduped
  expect_setequal(res$added$genus, c("Colletes","Chelostoma"))
})
