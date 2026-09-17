# test-specimen-idprompt.R -- interactive specimen taxon_id resolver
# Verifies: iNat SUGGESTION picking, answer parsing, and the accept/paste/skip/stop
# prompt loop that fills blank taxon_ids in the specimen-additions table.
suppressWarnings(suppressMessages(library(testthat)))
.impl <- function() src("reference/specimen_id_prompt.R")   # src() resolves from repo root (helper.R)

fake_fetch  <- function(results) function(term) results          # iNat search stub
queue_prompt <- function(answers) { i <- 0; function(...) { i <<- i + 1; answers[min(i, length(answers))] } }

test_that("suggest_taxon prefers an exact name+rank match", {
  .impl()
  cands <- list(
    list(id = 1,      name = "Melissodes",             rank = "genus"),
    list(id = 747170, name = "Melissodes microstictus", rank = "species"),
    list(id = 2,      name = "Melissodes microsticta",  rank = "species"))
  expect_equal(suggest_taxon(cands, "Melissodes microstictus", "species")$id, 747170L)
})

test_that("suggest_taxon falls back to first same-rank hit, else NULL", {
  .impl()
  expect_equal(suggest_taxon(list(list(id = 5, name = "Colletes x", rank = "species")),
                             "Colletes phaceliae", "species")$id, 5L)
  expect_null(suggest_taxon(list(), "x", "species"))
})

test_that(".spid_parse maps answers to actions", {
  .impl()
  expect_equal(.spid_parse("",       99L)$action, "accept")
  expect_equal(.spid_parse("",       99L)$id,     99L)
  expect_equal(.spid_parse("747170", NA)$action,  "id")
  expect_equal(.spid_parse("747170", NA)$id,      747170L)
  expect_equal(.spid_parse("s",      1L)$action,  "skip")
  expect_equal(.spid_parse("x",      1L)$action,  "stop")
  expect_equal(.spid_parse("huh",    1L)$action,  "reask")
  expect_equal(.spid_parse("",       NA)$action,  "reask")   # no suggestion + Enter -> reask
})

test_that("resolver: Enter accepts the iNat suggestion", {
  .impl()
  add <- data.frame(rank = "species", scientific_name = "Melissodes microstictus",
                    taxon_id = NA_integer_, genus = "Melissodes", species = "microstictus",
                    stringsAsFactors = FALSE)
  fetch <- fake_fetch(list(list(id = 747170, name = "Melissodes microstictus", rank = "species")))
  out <- resolve_specimen_additions_interactive(add, fetch, queue_prompt(c("")), interactive_ok = TRUE)
  expect_equal(out$additions$taxon_id, 747170L)
  expect_false(out$stopped)
})

test_that("resolver: pasted id overrides, skip leaves blank, stop halts", {
  .impl()
  base <- data.frame(rank = "species", scientific_name = "Colletes phaceliae",
                     taxon_id = NA_integer_, genus = "Colletes", species = "phaceliae",
                     stringsAsFactors = FALSE)
  fetch <- fake_fetch(list(list(id = 1, name = "Colletes wrong", rank = "species")))
  expect_equal(resolve_specimen_additions_interactive(base, fetch, queue_prompt(c("179703")), TRUE)$additions$taxon_id, 179703L)
  expect_true(is.na(resolve_specimen_additions_interactive(base, fetch, queue_prompt(c("s")), TRUE)$additions$taxon_id))
  st <- resolve_specimen_additions_interactive(base, fetch, queue_prompt(c("x")), TRUE)
  expect_true(st$stopped)
})

test_that("resolver: non-interactive is a no-op", {
  .impl()
  add <- data.frame(rank = "species", scientific_name = "X y", taxon_id = NA_integer_, stringsAsFactors = FALSE)
  out <- resolve_specimen_additions_interactive(add, fake_fetch(list()), queue_prompt(c("")), interactive_ok = FALSE)
  expect_true(is.na(out$additions$taxon_id))
})

test_that("seed_additions_from_flags builds a full-lineage row for a NEW flagged taxon", {
  .impl()
  flags  <- data.frame(genus = "Melissodes", species = "microstictus", subspecies = "",
                       flag_reason = "genus+species combo not in taxonomy lookup", stringsAsFactors = FALSE)
  record <- data.frame(order = "Hymenoptera", family = "Apidae", subfamily = "Apinae",
                       tribe = "Eucerini", genus = "Melissodes", subgenus = "Eumelissodes",
                       complex = "", species = "microstictus", stringsAsFactors = FALSE)
  out <- seed_additions_from_flags(flags, record, existing = NULL)
  expect_equal(nrow(out), 1L)
  expect_equal(out$scientific_name, "Melissodes microstictus")
  expect_true(is.na(out$taxon_id))                       # blank -> the prompt fills it
  expect_equal(out$in_cabr_specimens, "TRUE")
  expect_equal(out$family, "Apidae");   expect_equal(out$subfamily, "Apinae")
  expect_equal(out$tribe, "Eucerini");  expect_equal(out$subgenus, "Eumelissodes")
  expect_equal(out$kingdom, "Animalia"); expect_equal(out$superfamily, "Apoidea")   # full parents carried
})

test_that("seed_additions_from_flags skips taxa already in additions, and genus-only (no rank below genus)", {
  .impl()
  # Lasioglossum row has NO subgenus/complex/species -> genus-only -> NOT seeded (unchanged behavior)
  flags <- data.frame(genus = c("Colletes", "Lasioglossum"), species = c("phaceliae", ""),
                      subspecies = "", stringsAsFactors = FALSE)
  record <- data.frame(genus = "Colletes", species = "phaceliae", family = "Colletidae", stringsAsFactors = FALSE)
  existing <- data.frame(scientific_name = "Colletes phaceliae", stringsAsFactors = FALSE)
  expect_equal(nrow(seed_additions_from_flags(flags, record, existing)), 0L)
})

test_that("seed_additions_from_flags seeds a complex-only and subgenus-only flag at its own rank", {
  .impl()
  flags <- data.frame(
    genus      = c("Colletes",          "Lasioglossum", "Andrena"),
    subgenus   = c("",                  "Novomelitta",  ""),
    complex    = c("Colletes simulans", "",             ""),
    species    = c("",                  "",             "baeriae"),
    subspecies = "",
    flag_reason = c("complex not in taxonomy lookup", "subgenus not in taxonomy lookup",
                    "genus+species combo not in taxonomy lookup"),
    stringsAsFactors = FALSE)
  record <- data.frame(
    genus    = c("Colletes",          "Lasioglossum", "Andrena"),
    subgenus = c("",                  "Novomelitta",  ""),
    complex  = c("Colletes simulans", "",             ""),
    species  = c("",                  "",             "baeriae"),
    order    = "Hymenoptera",
    family   = c("Colletidae", "Halictidae", "Andrenidae"),
    subfamily = "", tribe = "", stringsAsFactors = FALSE)
  out <- seed_additions_from_flags(flags, record, existing = NULL)
  cx <- out[out$rank == "complex", ]
  expect_equal(nrow(cx), 1L)
  expect_equal(cx$scientific_name, "Colletes simulans")   # bare complex name = the search term
  expect_equal(cx$complex, "Colletes simulans")
  expect_equal(cx$genus, "Colletes")
  expect_true(is.na(cx$taxon_id))                         # blank -> resolver fills (or leaves, if not on iNat)
  expect_equal(cx$species, "")                            # a complex has no species
  expect_equal(cx$family, "Colletidae")                   # parent lineage pulled by genus
  sg <- out[out$rank == "subgenus", ]
  expect_equal(nrow(sg), 1L)
  expect_equal(sg$scientific_name, "Novomelitta")
  expect_equal(sg$subgenus, "Novomelitta")
  sp <- out[out$rank == "species", ]
  expect_equal(sp$scientific_name, "Andrena baeriae")     # species path still works
})

test_that("seed_additions_from_flags strips a '(Complex) ' tag from a flagged complex name", {
  .impl()
  flags <- data.frame(genus = "Lasioglossum", subgenus = "Dialictus",
                      complex = "(Complex) Lasioglossum gemmatum", species = "", subspecies = "",
                      flag_reason = "complex not in taxonomy lookup", stringsAsFactors = FALSE)
  record <- data.frame(genus = "Lasioglossum", subgenus = "Dialictus",
                       complex = "(Complex) Lasioglossum gemmatum", species = "",
                       family = "Halictidae", stringsAsFactors = FALSE)
  out <- seed_additions_from_flags(flags, record, existing = NULL)
  expect_equal(out$rank, "complex")
  expect_equal(out$scientific_name, "Lasioglossum gemmatum")   # tag stripped -> clean iNat search term
  expect_equal(out$complex, "Lasioglossum gemmatum")
})

test_that("flag_specimen_ids keys by RANK+name so a complex and species of the same name never clump", {
  .impl()
  flags <- data.frame(
    ucsd_id  = c("186",               "221",               "500",      "673"),
    sdnhm_id = c("0",                 "45",                "0",        "0"),
    genus    = c("Colletes",          "Colletes",          "Colletes", "Melissodes"),
    subgenus = "",
    complex  = c("Colletes simulans", "Colletes simulans", "",         ""),          # rows 1-2: COMPLEX simulans
    species  = c("",                  "",                  "simulans", "moorei"),     # row 3: SPECIES simulans (same name!)
    subspecies = "", stringsAsFactors = FALSE)
  m <- flag_specimen_ids(flags)
  expect_true("complex|colletes simulans" %in% names(m))       # complex and species get DIFFERENT keys
  expect_true("species|colletes simulans" %in% names(m))
  expect_match(m[["complex|colletes simulans"]], "ucsd_id 186, 221")   # only the two complex specimens
  expect_false(grepl("500", m[["complex|colletes simulans"]]))         # the species specimen is NOT clumped in
  expect_match(m[["complex|colletes simulans"]], "sdnhm_id 45")        # 0/blank sdnhm dropped
  expect_match(m[["species|colletes simulans"]], "ucsd_id 500")        # species specimen stands alone
  expect_false(grepl("186|221", m[["species|colletes simulans"]]))     # complex specimens NOT clumped in
  expect_match(m[["species|melissodes moorei"]], "ucsd_id 673")
})

test_that("resolve_specimen_additions_interactive shows only the matching rank's specimen ids", {
  .impl()
  add <- data.frame(rank = "complex", scientific_name = "Colletes simulans",
                    taxon_id = NA_integer_, genus = "Colletes", species = "", stringsAsFactors = FALSE)
  fetch <- fake_fetch(list(list(id = 1438677, name = "Colletes simulans", rank = "complex")))
  idmap <- stats::setNames(c("ucsd_id 186, 221", "ucsd_id 500"),
                           c("complex|colletes simulans", "species|colletes simulans"))
  expect_message(   # the complex prompt shows the complex specimens (186, 221), not the species one (500)
    resolve_specimen_additions_interactive(add, fetch, queue_prompt(c("")), interactive_ok = TRUE, id_map = idmap),
    "186, 221")
})

test_that("resolve_specimen_taxa shows specimen ids from the RECORD even when re-flagging is empty", {
  .impl()
  # the real re-run case: simulans is PENDING in additions (blank id) but no longer flagged
  # (it's already in the lookup), so the flags file is empty. The specimen ids must still show,
  # sourced from the raw record -- not the (empty) flags.
  addf <- tempfile(fileext = ".csv")
  writeLines(c(
    "rank,scientific_name,taxon_id,in_holway,in_inat,in_cabr_specimens,verified,kingdom,phylum,class,order,superfamily,family,subfamily,tribe,subtribe,genus,subgenus,complex,species,subspecies,common_name",
    "complex,Colletes simulans,,FALSE,FALSE,TRUE,FALSE,Animalia,Arthropoda,Insecta,Hymenoptera,Apoidea,Colletidae,Colletinae,Colletini,,Colletes,,Colletes simulans,,,"), addf)
  flgf <- tempfile(fileext = ".csv")
  writeLines('"ucsd_id","sdnhm_id","genus","subgenus","complex","species","subspecies","flag_reason"', flgf)  # EMPTY (header only)
  record <- data.frame(ucsd_id = c(186, 221), sdnhm_id = 0, genus = "Colletes", subgenus = "",
                       complex = "Colletes simulans", species = "", subspecies = "", stringsAsFactors = FALSE)
  fetch <- function(term) if (term == "Colletes simulans") list(list(id = 1438677, name = "Colletes simulans", rank = "complex")) else list()
  expect_message(
    resolve_specimen_taxa(record, additions_path = addf, flags_path = flgf,
                          fetch_fn = fetch, prompt_fn = function(...) "", interactive_ok = TRUE, write = FALSE, verbose = FALSE),
    "ucsd_id 186, 221")
})

test_that("resolve_specimen_taxa (driver) seeds a flag + fills ids with NO type clash", {
  .impl()
  addf <- tempfile(fileext = ".csv")
  writeLines(c(
    "rank,scientific_name,taxon_id,in_holway,in_inat,in_cabr_specimens,verified,kingdom,phylum,class,order,superfamily,family,subfamily,tribe,subtribe,genus,subgenus,complex,species,subspecies,common_name",
    "species,Colletes phaceliae,,FALSE,FALSE,TRUE,FALSE,Animalia,Arthropoda,Insecta,Hymenoptera,Apoidea,Colletidae,Colletinae,Colletini,,Colletes,,,phaceliae,,"), addf)
  flgf <- tempfile(fileext = ".csv")
  writeLines(c('"ucsd_id","genus","species","subspecies","flag_reason"',
               '1388,"Melissodes","microstictus",NA,"x"'), flgf)
  record <- data.frame(order = "Hymenoptera", family = "Apidae", subfamily = "Apinae", tribe = "Eucerini",
                       genus = "Melissodes", subgenus = "Eumelissodes", complex = "", species = "microstictus",
                       stringsAsFactors = FALSE)
  fetch <- function(term)
    if (term == "Melissodes microstictus") list(list(id = 747170, name = "Melissodes microstictus", rank = "species"))
    else if (term == "Colletes phaceliae") list(list(id = 179703, name = "Colletes phaceliae", rank = "species"))
    else list()
  suppressMessages(resolve_specimen_taxa(record, additions_path = addf, flags_path = flgf,
    fetch_fn = fetch, prompt_fn = function(...) "", interactive_ok = TRUE, write = TRUE, verbose = FALSE))
  out <- read.csv(addf, stringsAsFactors = FALSE, check.names = FALSE)
  expect_equal(nrow(out), 2L)                     # existing Colletes + seeded microstictus (no bind_rows clash)
  expect_true(all(!is.na(out$taxon_id)))          # both ids filled
  expect_equal(out$tribe[out$scientific_name == "Melissodes microstictus"], "Eucerini")
})

# --- subspecies help ---------------------------------------------------------
# A trinomial that iNaturalist will not match is almost never a bee without a
# page. It is a spelling difference in the last word: the specimen data holds
# "gaudialis" 118 times, "gaudiale" 18 and "gaudiais" twice, all one subspecies
# iNaturalist publishes as 345235. The prompt said only "no match for this name",
# which reads as "this bee is not on iNaturalist" and invites a skip.
LK <- data.frame(
  scientific_name = c("Colletes hyalinus", "Colletes hyalinus gaudialis",
                      "Colletes hyalinus hyalinus", "Colletes fulgidus",
                      "(Complex) Colletes hyalinus"),
  taxon_id = c(217441L, 345235L, 1052957L, 99L, NA_integer_),
  rank = c("species", "subspecies", "subspecies", "species", "complex"),
  stringsAsFactors = FALSE)

test_that(".spid_is_subspecies_name spots a trinomial", {
  .impl()
  expect_true(.spid_is_subspecies_name("Colletes hyalinus gaudiais"))
  expect_true(.spid_is_subspecies_name("  Colletes   hyalinus   gaudiais "))
  expect_false(.spid_is_subspecies_name("Colletes hyalinus"))
  expect_false(.spid_is_subspecies_name("Colletes"))
  expect_false(.spid_is_subspecies_name("(Complex) Colletes hyalinus"))
  expect_false(.spid_is_subspecies_name(""))
  expect_false(.spid_is_subspecies_name(NA))
})

test_that(".spid_sibling_subspecies finds the subspecies iNat does have", {
  .impl()
  sib <- .spid_sibling_subspecies("Colletes hyalinus gaudiais", LK)
  expect_equal(nrow(sib), 2L)
  expect_true(all(c(345235L, 1052957L) %in% sib$taxon_id))
})

test_that(".spid_sibling_subspecies excludes the name itself, other species, and id-less rows", {
  .impl()
  sib <- .spid_sibling_subspecies("Colletes hyalinus gaudialis", LK)
  expect_equal(sib$taxon_id, 1052957L)                     # its own row dropped
  expect_false(any(grepl("fulgidus", sib$scientific_name)))  # different species
  expect_false(any(is.na(sib$taxon_id)))                     # the complex row has no id
})

test_that(".spid_sibling_subspecies returns nothing when the species is unknown", {
  .impl()
  expect_equal(nrow(.spid_sibling_subspecies("Andrena prunorum foo", LK)), 0L)
  expect_equal(nrow(.spid_sibling_subspecies("Colletes hyalinus", LK)), 0L)
  expect_equal(nrow(.spid_sibling_subspecies("Colletes hyalinus foo", NULL)), 0L)
})

test_that(".spid_subspecies_help says the word subspecies and offers the near match", {
  .impl()
  ln <- paste(.spid_subspecies_help("Colletes hyalinus gaudiais", LK), collapse = " ")
  expect_match(ln, "subspecies")
  expect_match(ln, "Colletes hyalinus gaudialis", fixed = TRUE)
  expect_match(ln, "345235", fixed = TRUE)
  expect_match(ln, "spelling")
})

test_that(".spid_subspecies_help still explains the rank with no near match", {
  .impl()
  ln <- paste(.spid_subspecies_help("Andrena prunorum foo", LK), collapse = " ")
  expect_match(ln, "subspecies")
  expect_match(ln, "Andrena prunorum", fixed = TRUE)   # points at the parent species
  expect_false(grepl("345235", ln, fixed = TRUE))
})

test_that(".spid_subspecies_help is silent for a plain species", {
  .impl()
  expect_length(.spid_subspecies_help("Colletes hyalinus", LK), 0L)
})
