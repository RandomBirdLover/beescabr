library(testthat)
library(dplyr)

# Plant taxonomy lookup (crosswalk-driven): union of ALL in-park plant obs (any
# observer = truth) + specimen canonical names from the crosswalk. in_park is
# judged against the broad obs. Network is injected -> runs offline.

src("reference/plant_taxonomy_lookup_build.R")

# ---- pure helpers -----------------------------------------------------------

test_that("plt_norm / plt_label_rank basics", {
  expect_equal(plt_norm("  Eriogonum   FASCICULATUM "), "eriogonum fasciculatum")
  expect_equal(plt_label_rank("Cactaceae"), "family")
  expect_equal(plt_label_rank("Madia"), "genus")
  expect_equal(plt_label_rank("Acmispon glaber"), "species")
})

test_that("plt_basic_ranks pulls the 7 ranks from a taxon + ancestors", {
  taxon <- list(id = 101, name = "Acmispon glaber", rank = "species",
                ancestors = list(list(name="Plantae",rank="kingdom"),
                                 list(name="Fabaceae",rank="family"),
                                 list(name="Acmispon",rank="genus")))
  r <- plt_basic_ranks(taxon)
  expect_equal(unname(r["family"]), "Fabaceae")
  expect_equal(unname(r["species"]), "Acmispon glaber")
})

test_that("plt_pick_plant returns a plant, or NULL if none (no moth fallback)", {
  res_ok <- list(
    list(id=9, name="Madia elegans",       rank="species", is_active=TRUE, iconic_taxon_name="Plantae"),
    list(id=8, name="Speculina madiaria",  rank="species", is_active=TRUE, iconic_taxon_name="Animalia"))
  expect_equal(plt_pick_plant(res_ok, "Madia")$id, 9)
  res_moth <- list(list(id=8, name="Speculina madiaria", rank="species", is_active=TRUE, iconic_taxon_name="Animalia"))
  expect_null(plt_pick_plant(res_moth, "Madia"))     # the old code returned the moth here
  expect_null(plt_pick_plant(list(), "x"))
})

test_that("plt_in_park: species strict, genus-level for genus taxa", {
  park <- list(taxon_ids = c("101","105"),
               sci = c("acmispon glaber","deinandra fasciculata"),
               genus = c("acmispon","deinandra"), family = c("fabaceae","asteraceae"))
  expect_true(plt_in_park(list(taxon_id="101", rank="species", scientific_name="Acmispon glaber", genus="Acmispon", family="Fabaceae"), park))
  expect_false(plt_in_park(list(taxon_id="205", rank="species", scientific_name="Deinandra conjugens", genus="Deinandra", family="Asteraceae"), park))
  expect_false(plt_in_park(list(taxon_id="201", rank="genus",   scientific_name="Madia", genus="Madia", family="Asteraceae"), park))
})

# ---- end-to-end with a mock resolver ---------------------------------------

.mk_all_taxa <- function(path) {
  write.csv(tibble(
    taxon_id = c("101","102","103","105","106"), taxon_rank = "species",
    scientific_name = c("Acmispon glaber","Eriogonum fasciculatum","Cleomella arborea",
                        "Deinandra fasciculata","Encelia californica"),
    common_name = NA_character_,
    kingdom="Plantae", phylum="Tracheophyta", class="Magnoliopsida",
    order = c("Fabales","Caryophyllales","Brassicales","Asterales","Asterales"),
    family= c("Fabaceae","Polygonaceae","Cleomaceae","Asteraceae","Asteraceae"),
    genus = c("Acmispon","Eriogonum","Cleomella","Deinandra","Encelia"),
    species = c("Acmispon glaber","Eriogonum fasciculatum","Cleomella arborea",
                "Deinandra fasciculata","Encelia californica")), path, row.names=FALSE, na=""); path
}
.mk_cw <- function(path) {
  write.csv(tibble(
    name = c("tp","Acmispon glaber","Cleomella arborea","Deinandra fasciculata","Madia","Deinandra conjugens"),
    what_for = c("transect","plant_taxon","plant_taxon","plant_taxon","plant_taxon","plant_taxon"),
    specimen_label_variants = c("Tide Pool Trail","Acmispon glaber","Cleomella arborea; Peritoma arborea",
                                "Deinandra fasciculata; Hemizonia fasciculata","Madia sp.","Deinandra conjugens")),
    path, row.names=FALSE, na=""); path
}
mock_resolve <- function(name) {
  m <- list("madia" = list(id="201", sci="Madia", rank="genus", genus="Madia", family="Asteraceae"),
            "deinandra conjugens" = list(id="205", sci="Deinandra conjugens", rank="species", genus="Deinandra", family="Asteraceae"),
            "acmispon"  = list(id="301", sci="Acmispon",  rank="genus", genus="Acmispon",  family="Fabaceae"),
            "eriogonum" = list(id="302", sci="Eriogonum", rank="genus", genus="Eriogonum", family="Polygonaceae"),
            "cleomella" = list(id="303", sci="Cleomella", rank="genus", genus="Cleomella", family="Cleomaceae"),
            "deinandra" = list(id="305", sci="Deinandra", rank="genus", genus="Deinandra", family="Asteraceae"),
            "encelia"   = list(id="306", sci="Encelia",   rank="genus", genus="Encelia",   family="Asteraceae"),
            "euphorbia misera" = list(id="401", sci="Euphorbia misera", rank="species", genus="Euphorbia", family="Euphorbiaceae"),
            "euphorbia" = list(id="402", sci="Euphorbia", rank="genus", genus="Euphorbia", family="Euphorbiaceae"),
            "bergerocactus emoryi" = list(id="501", sci="Bergerocactus emoryi", rank="species", genus="Bergerocactus", family="Cactaceae"),
            "bergerocactus" = list(id="502", sci="Bergerocactus", rank="genus", genus="Bergerocactus", family="Cactaceae"),
            "isocoma menziesii" = list(id="601", sci="Isocoma menziesii", rank="species", genus="Isocoma", family="Asteraceae"),
            "isocoma" = list(id="602", sci="Isocoma", rank="genus", genus="Isocoma", family="Asteraceae"))[[plt_norm(name)]]
  if (is.null(m)) return(tibble(input_name=name, taxon_id=NA_character_, scientific_name=NA_character_,
                                common_name=NA_character_, rank=NA_character_, kingdom=NA_character_,
                                phylum=NA_character_, class=NA_character_, order=NA_character_,
                                family=NA_character_, genus=NA_character_, species=NA_character_, resolved=FALSE))
  tibble(input_name=name, taxon_id=m$id, scientific_name=m$sci, common_name=NA_character_, rank=m$rank,
         kingdom="Plantae", phylum=NA_character_, class=NA_character_, order=NA_character_,
         family=m$family, genus=m$genus, species=if(m$rank=="species") m$sci else NA_character_, resolved=TRUE)
}

.run <- function() {
  build_plant_taxonomy_lookup(all_taxa_path = .mk_all_taxa(tempfile(fileext=".csv")),
                              crosswalk_path = .mk_cw(tempfile(fileext=".csv")),
                              cache_path = tempfile(fileext=".csv"),
                              confirmed_path = tempfile(fileext=".csv"),   # absent -> no overrides
                              forage_fn = function() character(0),         # no bee forage
                              resolve_fn = mock_resolve, write = FALSE, verbose = FALSE)
}
.mk_confirmed <- function(path) {
  write.csv(tibble(scientific_name = "Euphorbia misera", taxon_id = "401",
                   note = "obscured threatened species -- expert-confirmed"),
            path, row.names = FALSE, na = ""); path
}
.run_confirmed <- function() {
  build_plant_taxonomy_lookup(all_taxa_path = .mk_all_taxa(tempfile(fileext=".csv")),
                              crosswalk_path = .mk_cw(tempfile(fileext=".csv")),
                              cache_path = tempfile(fileext=".csv"),
                              confirmed_path = .mk_confirmed(tempfile(fileext=".csv")),
                              forage_fn = function() character(0),
                              resolve_fn = mock_resolve, write = FALSE, verbose = FALSE)
}
.run_forage <- function(names_vec) {
  build_plant_taxonomy_lookup(all_taxa_path = .mk_all_taxa(tempfile(fileext=".csv")),
                              crosswalk_path = .mk_cw(tempfile(fileext=".csv")),
                              cache_path = tempfile(fileext=".csv"),
                              confirmed_path = tempfile(fileext=".csv"),
                              forage_fn = function() names_vec,
                              resolve_fn = mock_resolve, write = FALSE, verbose = FALSE)
}

test_that("lookup unions broad obs + crosswalk specimen plants with correct flags", {
  out <- .run(); lk <- out$lookup
  expect_setequal(names(lk), PLT_LOOKUP_COLS)
  g <- function(s) lk[lk$scientific_name == s, , drop = FALSE]

  expect_true(g("Acmispon glaber")$in_cabr_park_at_all); expect_true(g("Acmispon glaber")$in_specimens)
  expect_true(g("Acmispon glaber")$in_observations)
  expect_true(g("Encelia californica")$in_cabr_park_at_all); expect_false(g("Encelia californica")$in_specimens)
  expect_true(g("Cleomella arborea")$in_specimens); expect_true(g("Cleomella arborea")$in_cabr_park_at_all)
})

test_that("every genus gets its own row with a taxon_id (genus normalization)", {
  out <- .run(); lk <- out$lookup
  g <- function(s) lk[lk$scientific_name == s, , drop = FALSE]
  for (gg in c("Acmispon","Eriogonum","Cleomella","Deinandra","Encelia")) {
    row <- g(gg)
    expect_equal(nrow(row), 1L)
    expect_equal(tolower(row$rank), "genus")
    expect_true(nzchar(row$taxon_id))          # genus row carries its own taxon_id
    expect_true(row$in_cabr_park_at_all)       # genus observed -> in park
  }
  # species row and its genus row coexist with DIFFERENT park verdicts
  expect_false(g("Deinandra conjugens")$in_cabr_park_at_all)  # species not confirmed
  expect_true(g("Deinandra")$in_cabr_park_at_all)             # genus observed
})

test_that("specimen plants not observed anywhere in the park are flagged (worklist)", {
  out <- .run(); lk <- out$lookup; wl <- out$worklist
  expect_false(lk[lk$scientific_name=="Madia",]$in_cabr_park_at_all)
  expect_true(lk[lk$scientific_name=="Madia",]$in_specimens)
  dc <- lk[lk$scientific_name=="Deinandra conjugens",]
  expect_false(dc$in_cabr_park_at_all); expect_false(dc$in_observations)
  expect_setequal(wl$scientific_name, c("Madia","Deinandra conjugens"))
})

test_that("confirmed-in-park override flips an obscured species to in_park despite no obs", {
  out <- .run_confirmed(); lk <- out$lookup
  g <- function(s) lk[lk$scientific_name == s, , drop = FALSE]
  em <- g("Euphorbia misera")
  expect_equal(nrow(em), 1L)
  expect_true(em$in_cabr_park_at_all)     # forced in-park by the curated list
  expect_false(em$in_observations)        # not in the obs truth ...
  expect_false(em$in_specimens)           # ... and not on a specimen -> override fingerprint
  expect_equal(em$taxon_id, "401")
  expect_true(g("Euphorbia")$in_cabr_park_at_all)                 # genus folded into park truth
  expect_false("Euphorbia misera" %in% out$worklist$scientific_name)  # never on the worklist
})

test_that("plt_resolve_names binds a fresh resolution onto an all-character disk cache", {
  cf <- tempfile(fileext = ".csv")   # simulate a disk cache: every column character, resolved="TRUE"
  write.csv(tibble(input_name="Acmispon glaber", taxon_id="101", scientific_name="Acmispon glaber",
                   common_name=NA, rank="species", kingdom="Plantae", phylum=NA, class=NA, order=NA,
                   family="Fabaceae", genus="Acmispon", species="Acmispon glaber", resolved="TRUE"),
            cf, row.names=FALSE, na="")
  # resolving a NEW name must not error binding a logical `resolved` onto character cache
  res <- plt_resolve_names(c("Acmispon glaber","Madia"), resolve_fn=mock_resolve, cache_path=cf, verbose=FALSE)
  expect_true("Madia" %in% res$rows$scientific_name)
  expect_true(nrow(res$cache) >= 2)
})
