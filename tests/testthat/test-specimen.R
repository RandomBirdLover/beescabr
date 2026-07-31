library(testthat)
library(dplyr)

test_that("standardize_specimen_names fixes casing", {
  src("specimens/specimen_clean_helpers.R")
  df <- tibble::tibble(genus = c("ANDRENA", "melissodes"),
                       species = c("Robustior", "SUBTILIOR"),
                       subspecies = c(NA, "FOO"))
  out <- standardize_specimen_names(df)
  expect_equal(out$genus, c("Andrena", "Melissodes"))
  expect_equal(out$species, c("robustior", "subtilior"))
  expect_equal(out$subspecies, c(NA, "foo"))
})

test_that(".parse_specimen_date_vec converts Excel serials and parses normal dates", {
  src("specimens/specimen_clean_helpers.R")
  out <- .parse_specimen_date_vec(c("2021-04-24", "44310", "", NA_character_))
  expect_equal(out[1], as.Date("2021-04-24"))
  expect_equal(out[2], as.Date("2021-04-24"))   # 44310 = Excel serial -> 2021-04-24
  expect_true(is.na(out[3]))                     # blank -> NA
  expect_true(is.na(out[4]))                     # NA -> NA
})

test_that("parse_specimen_dates fills date/year from an Excel-serial date cell", {
  src("specimens/specimen_clean_helpers.R")
  out <- parse_specimen_dates(tibble::tibble(date = c("2021-04-24", "44310")))
  expect_equal(out$date_clean, as.Date(c("2021-04-24", "2021-04-24")))
  expect_equal(out$year, c(2021, 2021))
})

test_that(".parse_specimen_date_vec ignores numbers outside the Excel-serial window", {
  src("specimens/specimen_clean_helpers.R")
  # 100 is not a plausible collection-date serial -> NOT converted via Excel origin
  expect_true(is.na(suppressWarnings(.parse_specimen_date_vec("100"))))
})

test_that("sbc_bee_situation maps flower_visited / ground / aerial like the iNat side", {
  src("specimens/specimen_clean_helpers.R")
  df <- tibble::tibble(
    flower_visited  = c("Encelia californica", NA_character_, NA_character_, NA_character_),
    method_or_plant = c("ex. Encelia californica", "Ground Grab", "Aerial Net", ""))
  expect_equal(sbc_bee_situation(df), c("on_flower", "on_ground", "aerial", NA))
})

test_that("sbc_bee_situation: 'On ground' -> on_ground, 'In air' -> aerial, blank method -> NA", {
  src("specimens/specimen_clean_helpers.R")
  df <- tibble::tibble(flower_visited = NA_character_,
                       method_or_plant = c("On ground", "In air", NA_character_))
  expect_equal(sbc_bee_situation(df), c("on_ground", "aerial", NA))
})

test_that("fill_specimen_taxonomy coalesces blanks from the lookup only", {
  src("specimens/specimen_clean_helpers.R")
  df <- tibble::tibble(genus = "Colletes", species = "hyalinus", subspecies = NA_character_,
                       family = "", subfamily = NA_character_, tribe = "KeepMe")
  lk <- tibble::tibble(genus = "Colletes", species = "hyalinus", subspecies = NA_character_,
                       family = "Colletidae", subfamily = "Colletinae", tribe = "Colletini")
  out <- fill_specimen_taxonomy(df, lk)
  expect_equal(out$family, "Colletidae")     # blank filled
  expect_equal(out$subfamily, "Colletinae")  # NA filled
  expect_equal(out$tribe, "KeepMe")          # present value kept
})

test_that("compute_taxonomy_flags flags unknown genus and unknown genus+species", {
  src("specimens/specimen_clean_helpers.R")
  df <- tibble::tibble(
    ucsd_id = 1:3, sdnhm_id = 0,
    genus = c("Andrena", "Augochorella", "Andrena"),   # Augochorella = typo
    species = c("baeriae", "pomoniella", "notaspecies"),
    subspecies = NA_character_)
  known_genera <- c("Andrena")
  known_gs <- tibble::tibble(genus = "Andrena", species = "baeriae")
  flags <- compute_taxonomy_flags(df, known_genera, known_gs)
  expect_true("Augochorella" %in% flags$genus)
  expect_true(any(flags$flag_reason == "genus not in taxonomy lookup"))
  expect_true(any(flags$flag_reason == "genus+species combo not in taxonomy lookup"))
  # Andrena baeriae is known -> not flagged
  expect_false(any(flags$genus == "Andrena" & flags$species == "baeriae"))
})

test_that("build_known_names surfaces known subgenera + complexes from the lookup (tag stripped)", {
  src("specimens/specimen_clean_helpers.R")
  tax_check <- tibble::tibble(
    genus    = c("Lasioglossum", "Colletes",  "Melissodes"),
    species  = c("",             "",          "moorei"),
    subgenus = c("Dialictus",    "",          "Eumelissodes"),
    complex  = c("",             "(Complex) Colletes americanus", ""))
  inat_species <- tibble::tibble(genus = character(), species = character())
  kn <- build_known_names(tax_check, inat_species)
  expect_true(any(kn$subgenera$genus == "Lasioglossum" & kn$subgenera$subgenus == "Dialictus"))
  expect_true(any(kn$complexes$genus == "Colletes" & tolower(kn$complexes$complex) == "colletes americanus"))
  expect_false(any(grepl("\\(Complex\\)", kn$complexes$complex)))   # tag stripped for matching
})

test_that("compute_taxonomy_flags flags a subgenus-only and complex-only ID absent from the lookup", {
  src("specimens/specimen_clean_helpers.R")
  df <- tibble::tibble(
    ucsd_id  = 1:5, sdnhm_id = 0,
    genus    = c("Colletes",          "Lasioglossum", "Lasioglossum", "Colletes",  "Andrena"),
    subgenus = c("",                  "Novomelitta",  "Dialictus",    "",          ""),          # Novomelitta absent, Dialictus known
    complex  = c("Colletes simulans", "",             "",             "",          ""),          # simulans absent
    species  = c("",                  "",             "",             "",          "baeriae"),   # Andrena baeriae known
    subspecies = NA_character_)
  known_genera <- c("Colletes", "Lasioglossum", "Andrena")
  known_gs     <- tibble::tibble(genus = "Andrena", species = "baeriae")
  known_subg   <- tibble::tibble(genus = "Lasioglossum", subgenus = "Dialictus")       # Novomelitta NOT here
  known_cx     <- tibble::tibble(genus = "Colletes", complex = "Colletes americanus")  # simulans NOT here
  flags <- compute_taxonomy_flags(df, known_genera, known_gs, known_subg, known_cx)
  expect_true(all(c("subgenus", "complex") %in% names(flags)))                          # coarse cols carried for seeding
  expect_true(any(flags$complex == "Colletes simulans" & grepl("complex", flags$flag_reason)))
  expect_true(any(flags$subgenus == "Novomelitta"     & grepl("subgenus", flags$flag_reason)))
  expect_false(any(flags$subgenus == "Dialictus"))    # known subgenus -> NOT flagged
  expect_false(any(flags$species == "baeriae"))       # known species  -> NOT flagged
})

test_that("compute_taxonomy_flags: coarse flags require a KNOWN genus (unknown genus dominates)", {
  src("specimens/specimen_clean_helpers.R")
  df <- tibble::tibble(ucsd_id = 1L, sdnhm_id = 0,
                       genus = "Notagenus", subgenus = "Whatever", complex = "", species = "",
                       subspecies = NA_character_)
  flags <- compute_taxonomy_flags(df, known_genera = character(), known_genus_species = tibble::tibble(genus = character(), species = character()),
                                  known_subgenera = tibble::tibble(genus = character(), subgenus = character()),
                                  known_complexes = tibble::tibble(genus = character(), complex = character()))
  expect_equal(nrow(flags), 1L)
  expect_true(grepl("genus not in", flags$flag_reason))     # reported as unknown-genus, not unknown-subgenus
})

test_that("add_qc_flags marks missing fields", {
  src("specimens/specimen_clean_helpers.R")
  df <- tibble::tibble(latitude = c(1, NA), longitude = c(1, 2), date = c(Sys.Date(), NA),
                       sdnhm_id = c("A", ""), ucsd_id = c("U", "V"), genus = c("Andrena", ""))
  out <- add_qc_flags(df)
  expect_equal(out$missing_latlong, c(FALSE, TRUE))
  expect_equal(out$missing_date, c(FALSE, TRUE))
  expect_equal(out$missing_sdnhm_id, c(FALSE, TRUE))
  expect_equal(out$missing_genus, c(FALSE, TRUE))
})

test_that("detect_duplicate_ids catches dup ucsd and sdnhm (ignoring 0/NA)", {
  src("specimens/specimen_clean_helpers.R")
  df <- tibble::tibble(
    ucsd_id  = c(1, 1, 2, 3),
    sdnhm_id = c(10, 20, 0, 0))
  dups <- detect_duplicate_ids(df)
  expect_true(1 %in% dups$ucsd_id)          # duplicated ucsd
  expect_false(any(dups$sdnhm_id == 0))     # zeros never flagged
})

test_that("detect_duplicate_ids ignores blank/NA ucsd_id but still reports sdnhm dups on those rows", {
  src("specimens/specimen_clean_helpers.R")
  df <- tibble::tibble(
    ucsd_id  = c(NA, NA, "", ""),           # all un-assigned -> must NOT be flagged as ucsd dups
    sdnhm_id = c(50, 50, 0, 99))            # 50,50 is a genuine sdnhm duplicate
  dups <- detect_duplicate_ids(df)
  expect_false(any(dups$duplicate_reason == "duplicate ucsd_id"))     # blanks not false-flagged
  expect_equal(sum(dups$duplicate_reason == "duplicate sdnhm_id"), 2) # both sdnhm=50 rows kept
  expect_true(all(dups$sdnhm_id == 50))
})

test_that("match_specimen_complex prefixes and gates on species", {
  src("specimens/specimen_clean_helpers.R")
  df <- tibble::tibble(genus = c("Diadasia", "Diadasia"), species = c("australis", NA),
                       complex = NA_character_, complex_taxon_id = NA)
  lk <- tibble::tibble(genus = "diadasia", species = "australis",
                       complex_match = "Diadasia australis", complex_taxon_id_match = 42)
  out <- match_specimen_complex(df, lk)
  expect_equal(out$complex[1], "(Complex) Diadasia australis")
  expect_equal(out$complex_taxon_id[1], 42)
  expect_true(is.na(out$complex[2]))        # genus-only -> no complex
})

test_that("match_specimen_complex preserves a hand-typed complex when there is no species-level map hit", {
  src("specimens/specimen_clean_helpers.R")
  # A complex-only ID (blank species) carries the collector's hand-entered complex -- it must
  # SURVIVE (tagged "(Complex) "), not be wiped to NA. The old else-branch forced NA on every
  # row without a species+map hit, silently discarding a determination the collector made
  # (e.g. 'Colletes simulans', a complex not in the lookup).
  df <- tibble::tibble(
    genus            = c("Colletes", "Lasioglossum", "Diadasia", "Colletes"),
    species          = c(NA_character_, NA_character_, "australis", NA_character_),
    complex          = c("Colletes simulans", "(Complex) Lasioglossum gemmatum", NA_character_, NA_character_),
    complex_taxon_id = NA)
  lk <- tibble::tibble(genus = "diadasia", species = "australis",
                       complex_match = "Diadasia australis", complex_taxon_id_match = 42L)
  out <- match_specimen_complex(df, lk)
  expect_equal(out$complex[1], "(Complex) Colletes simulans")      # bare hand-typed -> tagged + kept
  expect_equal(out$complex[2], "(Complex) Lasioglossum gemmatum")  # already tagged -> unchanged (idempotent)
  expect_equal(out$complex[3], "(Complex) Diadasia australis")     # species map hit still wins
  expect_equal(out$complex_taxon_id[3], 42L)
  expect_true(is.na(out$complex[4]))                               # genus-only, no complex typed -> stays NA
})

test_that("build_old_scientific_name handles the three blank cases", {
  src("specimens/specimen_clean_helpers.R")
  df <- tibble::tibble(old_genus_name = c(NA, "Andrena", "Andrena"),
                       old_species_name = c(NA, NA, "prunorum"))
  out <- build_old_scientific_name(df)
  expect_true(is.na(out$old_scientific_name[1]))
  expect_equal(out$old_scientific_name[2], "Andrena")
  expect_equal(out$old_scientific_name[3], "Andrena prunorum")
})

# standard vocabulary: skip|s|continue|c|go|ok|y|yes -> continue ; stop|x|halt|fix|n|no -> stop ;
# a bare Enter / unrecognized input RE-ASKS (never guesses). A fake that feeds a queue of
# answers lets us assert the re-ask loop.
.fake_answers <- function(xs) { i <- 0L; function(...) { i <<- i + 1L; xs[[min(i, length(xs))]] } }

test_that("resolve_flag_gate: clean / continue / stop (standard vocab)", {
  src("specimens/specimen_clean_helpers.R")
  expect_equal(resolve_flag_gate(0, interactive_ok = TRUE), "clean")
  expect_equal(resolve_flag_gate(3, interactive_ok = FALSE), "continue")   # non-interactive skips
  expect_equal(resolve_flag_gate(3, interactive_ok = TRUE, prompt_fn = function(...) "skip"), "continue")
  expect_equal(resolve_flag_gate(3, interactive_ok = TRUE, prompt_fn = function(...) "stop"), "stop")
  expect_equal(resolve_flag_gate(3, interactive_ok = TRUE, prompt_fn = function(...) "y"), "stop")       # y = stop & fix
  expect_equal(resolve_flag_gate(3, interactive_ok = TRUE, prompt_fn = function(...) "n"), "continue")   # n = continue
  expect_equal(resolve_flag_gate(3, interactive_ok = TRUE, prompt_fn = function(...) ""),  "continue")   # Enter = default continue
})

test_that("resolve_review_gate: clean / batch-continue / standard skip vs stop", {
  src("specimens/specimen_clean_helpers.R")
  none <- data.frame(label = "x", count = 0L, file = "f.csv", stringsAsFactors = FALSE)
  some <- data.frame(label = c("taxonomy", "dupes"), count = c(2L, 1L),
                     file = c("a.csv", "b.csv"), stringsAsFactors = FALSE)
  expect_equal(resolve_review_gate(none, "review", interactive_ok = TRUE),  "clean")     # nothing flagged
  expect_equal(resolve_review_gate(some, "review", interactive_ok = FALSE), "continue")  # batch: log + go
  expect_equal(resolve_review_gate(some, "review", interactive_ok = TRUE, prompt_fn = function(...) "skip"), "continue")
  expect_equal(resolve_review_gate(some, "review", interactive_ok = TRUE, prompt_fn = function(...) "stop"), "stop")
  # Enter is the default -> continue; only genuine garbage RE-ASKS until valid
  expect_equal(resolve_review_gate(some, "review", interactive_ok = TRUE, prompt_fn = function(...) ""), "continue")
  suppressMessages(
    expect_equal(resolve_review_gate(some, "review", interactive_ok = TRUE,
                                     prompt_fn = .fake_answers(c("junk", "y"))), "stop"))
  # non-blocking (iNat): a heads-up only -- always continues, whatever is typed
  expect_equal(resolve_review_gate(some, "review", interactive_ok = TRUE, prompt_fn = function(...) "", blocking = FALSE), "continue")
})

test_that("drop_non_native_apis removes honey bees (Apis), keeps natives + NA genus", {
  src("specimens/specimen_clean_helpers.R")
  df  <- tibble(genus = c("Apis", "apis", "Bombus", NA, "Andrena"),
                species = c("mellifera", "mellifera", "x", "y", "z"))
  out <- drop_non_native_apis(df)
  expect_equal(nrow(out), 3L)                                   # both Apis rows dropped
  expect_false(any(tolower(out$genus) == "apis", na.rm = TRUE))
  expect_true(all(c("Bombus", "Andrena") %in% out$genus) && any(is.na(out$genus)))
})

test_that("flag_raw_clutter tags non-ID'd and missing rows", {
  src("specimens/specimen_clean_helpers.R")
  df <- tibble::tibble(ucsd_id = 1:4,
                       genus = c("Andrena", "", "Bombus", NA_character_),
                       missing_specimen = c("N", "N", "Y", "Y"))
  out <- flag_raw_clutter(df)
  expect_equal(nrow(out), 3)                              # row1 (Andrena, N) is clean
  expect_equal(out$clutter_reason[out$ucsd_id == 2], "needs_id")
  expect_equal(out$clutter_reason[out$ucsd_id == 3], "missing")
  expect_equal(out$clutter_reason[out$ucsd_id == 4], "needs_id; missing")
})

test_that("transect_variant_map + match_plot_transect map plot text to TP/UPMON/BST", {
  src("specimens/specimen_clean_helpers.R")
  cw <- tibble::tibble(
    name     = c("tp", "upmon", "bst", "ot", "feeding"),
    what_for = c("transect", "transect", "transect", "transect", "behavior"),
    specimen_label_variants = c("Cabrillo NM: Tide Pool Trail; TPT1; TP2",
                                "Cabrillo NM: UpMon; Upr. Mnumnt.",
                                "Cabrillo NM: Bayside Trail; BST",
                                NA_character_, NA_character_))
  vm <- transect_variant_map(cw)
  expect_true(all(c("TP", "UPMON", "BST") %in% vm$transect))
  expect_false("OT" %in% vm$transect)            # no specimen variants -> not in the map
  plots <- c("Cabrillo NM: Upr. Mnumnt.", "Cabrillo NM: Tide Pool Trail 1",
             "Cabrillo NM: BST", "Cabrillo NM", NA_character_)
  expect_equal(match_plot_transect(plots, vm), c("UPMON", "TP", "BST", NA, NA))
})

test_that("attach_lookup_taxonomy fills taxon_id + higher ranks, keeps specimen names", {
  src("specimens/specimen_clean_helpers.R")
  # row1: genus+species ID; row2: family-ONLY ID (blank genus); row3: truly blank
  df <- tibble::tibble(
    genus      = c("Andrena", NA_character_, NA_character_),
    species    = c("baeriae", NA_character_, NA_character_),
    subspecies = NA_character_,
    subgenus   = c("Callandrena", NA_character_, NA_character_),
    complex    = NA_character_,
    family     = c(NA_character_, "Andrenidae", NA_character_),
    subfamily  = NA_character_, tribe = NA_character_)
  lk <- tibble::tibble(
    taxon_id = c(123L, 999L), rank = c("species", "family"),
    scientific_name = c("Andrena baeriae", "Andrenidae"), common_name = NA_character_,
    kingdom = "Animalia", phylum = "Arthropoda", subphylum = "Hexapoda", class = "Insecta",
    subclass = "Pterygota", order = "Hymenoptera", suborder = "Apocrita", infraorder = "Aculeata",
    superfamily = "Apoidea", family = c("Andrenidae", "Andrenidae"), epifamily = "Anthophila",
    subfamily = c("Andreninae", NA_character_), tribe = c("Andrenini", NA_character_),
    subtribe = NA_character_, genus = c("Andrena", NA_character_),
    subgenus = c("Callandrena", NA_character_), complex = NA_character_,
    species = c("baeriae", NA_character_), subspecies = NA_character_)
  out <- attach_lookup_taxonomy(df, lk)
  expect_equal(out$taxon_id[1], 123L)
  expect_equal(out$taxon_rank[1], "species")
  expect_equal(out$scientific_name[1], "Andrena baeriae")
  expect_equal(out$kingdom[1], "Animalia")
  expect_equal(out$family[1], "Andrenidae")   # was NA -> filled from lookup
  expect_equal(out$genus[1], "Andrena")       # specimen's own kept
  expect_equal(out$species[1], "baeriae")
  # family-ONLY specimen now resolves to the family taxon at its own rank
  expect_equal(out$taxon_id[2], 999L)
  expect_equal(out$taxon_rank[2], "family")
  expect_equal(out$scientific_name[2], "Andrenidae")
  expect_equal(out$family[2], "Andrenidae")   # its own family column is kept
  # truly-blank specimen (no genus, no family, nothing) must NOT grab a spurious id
  expect_true(is.na(out$taxon_id[3]))
  expect_true(is.na(out$scientific_name[3]))
})

# fill_coarse_ids(): a specimen ID'd only to a COARSE below-genus rank (genus,
# subgenus, or a named species-complex, with a BLANK species) must resolve to the
# lookup NODE AT THAT RANK -- never collapse onto an arbitrary child complex. This is
# the coarse-ID fabrication bug: a blank-species ID joined on (genus, species) alone
# matched every same-genus blank-species lookup row and grabbed the first (a complex).

test_that("fill_coarse_ids resolves genus/subgenus to their own node, not a child complex", {
  src("specimens/specimen_clean_helpers.R")
  df <- tibble::tibble(
    genus      = c("Colletes", "Lasioglossum", "Andrena"),
    subgenus   = c(NA_character_, "Dialictus", NA_character_),
    complex    = NA_character_,
    species    = NA_character_, subspecies = NA_character_,
    taxon_id   = NA_integer_, taxon_rank = NA_character_,
    scientific_name = NA_character_, common_name = NA_character_)
  # complex/other children listed BEFORE the genus/subgenus node -- the file-order that
  # made distinct() pick the wrong row.
  lk <- tibble::tibble(
    rank     = c("complex", "genus", "complex", "subgenus", "complex", "genus"),
    genus    = c("Colletes", "Colletes", "Lasioglossum", "Lasioglossum", "Andrena", "Andrena"),
    subgenus = c(NA_character_, NA_character_, "Dialictus", "Dialictus", "Melandrena", NA_character_),
    complex  = c("(Complex) Colletes americanus", NA_character_,
                 "(Complex) Lasioglossum gemmatum", NA_character_,
                 "(Complex) Andrena osmioides", NA_character_),
    species = NA_character_, subspecies = NA_character_,
    taxon_id = c(1438678L, 127741L, 1450287L, 126545L, 1258784L, 57669L),
    scientific_name = c("Colletes americanus", "Colletes", "Lasioglossum gemmatum",
                        "Dialictus", "Andrena osmioides", "Andrena"),
    common_name = NA_character_)
  out <- fill_coarse_ids(df, lk)
  expect_equal(out$taxon_id[1], 127741L)   # Colletes genus-only -> genus, NOT americanus complex
  expect_equal(out$taxon_rank[1], "genus")
  expect_equal(out$scientific_name[1], "Colletes")
  expect_equal(out$taxon_id[2], 126545L)   # Dialictus subgenus-only -> subgenus, NOT gemmatum complex
  expect_equal(out$taxon_rank[2], "subgenus")
  expect_equal(out$taxon_id[3], 57669L)    # Andrena genus-only -> genus, NOT osmioides complex
  expect_equal(out$taxon_rank[3], "genus")
})

test_that("fill_coarse_ids matches a named complex when it IS in the lookup", {
  src("specimens/specimen_clean_helpers.R")
  df <- tibble::tibble(genus = "Lasioglossum", subgenus = "Dialictus",
                       complex = "Lasioglossum gemmatum", species = NA_character_, subspecies = NA_character_,
                       taxon_id = NA_integer_, taxon_rank = NA_character_,
                       scientific_name = NA_character_, common_name = NA_character_)
  lk <- tibble::tibble(rank = c("complex", "subgenus"),
                       genus = "Lasioglossum", subgenus = "Dialictus",
                       complex = c("(Complex) Lasioglossum gemmatum", NA_character_),
                       species = NA_character_, subspecies = NA_character_,
                       taxon_id = c(1450287L, 126545L),
                       scientific_name = c("Lasioglossum gemmatum", "Dialictus"), common_name = NA_character_)
  out <- fill_coarse_ids(df, lk)
  expect_equal(out$taxon_id[1], 1450287L)   # exact complex wins over its subgenus parent
  expect_equal(out$taxon_rank[1], "complex")
})

test_that("fill_coarse_ids rolls an unrecognised complex back to its parent (never a sibling)", {
  src("specimens/specimen_clean_helpers.R")
  # 'Colletes simulans' complex is NOT in the lookup; must roll back to the Colletes
  # genus node -- NOT be fabricated onto the americanus complex that happens to share genus.
  df <- tibble::tibble(genus = "Colletes", subgenus = NA_character_,
                       complex = "Colletes simulans", species = NA_character_, subspecies = NA_character_,
                       taxon_id = NA_integer_, taxon_rank = NA_character_,
                       scientific_name = NA_character_, common_name = NA_character_)
  lk <- tibble::tibble(rank = c("complex", "genus"),
                       genus = "Colletes", subgenus = NA_character_,
                       complex = c("(Complex) Colletes americanus", NA_character_),
                       species = NA_character_, subspecies = NA_character_,
                       taxon_id = c(1438678L, 127741L),
                       scientific_name = c("Colletes americanus", "Colletes"), common_name = NA_character_)
  out <- fill_coarse_ids(df, lk)
  expect_equal(out$taxon_id[1], 127741L)    # rolled back to genus parent
  expect_equal(out$taxon_rank[1], "genus")
})

test_that("fill_coarse_ids leaves species-level and blank-genus rows untouched", {
  src("specimens/specimen_clean_helpers.R")
  df <- tibble::tibble(genus = c("Melissodes", NA_character_), subgenus = c("Eumelissodes", NA_character_),
                       complex = NA_character_, species = c("moorei", NA_character_), subspecies = NA_character_,
                       taxon_id = NA_integer_, taxon_rank = NA_character_,
                       scientific_name = NA_character_, common_name = NA_character_)
  lk <- tibble::tibble(rank = "genus", genus = "Melissodes", subgenus = NA_character_, complex = NA_character_,
                       species = NA_character_, subspecies = NA_character_, taxon_id = 52781L,
                       scientific_name = "Melissodes", common_name = NA_character_)
  out <- fill_coarse_ids(df, lk)
  expect_true(is.na(out$taxon_id[1]))   # has a species -> NOT coarse; must NOT be demoted to genus
  expect_true(is.na(out$taxon_id[2]))   # blank genus -> left for fill_above_genus_ids
})

test_that("attach_lookup_taxonomy does NOT fabricate a complex for a coarse (genus/subgenus-only) ID", {
  src("specimens/specimen_clean_helpers.R")
  # end-to-end repro: complex rows precede the genus/subgenus node in the lookup (real file order)
  df <- tibble::tibble(
    genus      = c("Colletes", "Lasioglossum", "Melissodes"),
    subgenus   = c(NA_character_, "Dialictus", "Eumelissodes"),
    complex    = NA_character_,
    species    = c(NA_character_, NA_character_, "moorei"),
    subspecies = NA_character_,
    family = NA_character_, subfamily = NA_character_, tribe = NA_character_)
  lk <- tibble::tibble(
    taxon_id = c(1438678L, 127741L, 1450287L, 126545L, 346685L),
    rank     = c("complex", "genus", "complex", "subgenus", "species"),
    scientific_name = c("Colletes americanus", "Colletes", "Lasioglossum gemmatum",
                        "Dialictus", "Melissodes moorei"),
    common_name = NA_character_,
    kingdom = "Animalia", phylum = "Arthropoda", subphylum = "Hexapoda", class = "Insecta",
    subclass = "Pterygota", order = "Hymenoptera", suborder = "Apocrita", infraorder = "Aculeata",
    superfamily = "Apoidea",
    family = c("Colletidae", "Colletidae", "Halictidae", "Halictidae", "Apidae"),
    epifamily = "Anthophila", subfamily = NA_character_, tribe = NA_character_, subtribe = NA_character_,
    genus = c("Colletes", "Colletes", "Lasioglossum", "Lasioglossum", "Melissodes"),
    subgenus = c(NA_character_, NA_character_, "Dialictus", "Dialictus", "Eumelissodes"),
    complex = c("(Complex) Colletes americanus", NA_character_, "(Complex) Lasioglossum gemmatum",
                NA_character_, NA_character_),
    species = c(NA_character_, NA_character_, NA_character_, NA_character_, "moorei"),
    subspecies = NA_character_)
  out <- attach_lookup_taxonomy(df, lk)
  expect_equal(out$taxon_id[1], 127741L)    # Colletes genus-only -> genus, never americanus complex
  expect_equal(out$taxon_rank[1], "genus")
  expect_equal(out$taxon_id[2], 126545L)    # Dialictus subgenus-only -> subgenus, never gemmatum complex
  expect_equal(out$taxon_rank[2], "subgenus")
  expect_equal(out$taxon_id[3], 346685L)    # species-level moorei still resolves exactly (regression guard)
  expect_equal(out$taxon_rank[3], "species")
})

# keep_bee_specimens(): the specimen record carries wasp/fly bycatch and fully-
# unidentified rows; the bee cleaning script keeps ONLY the seven bee families
# (Anthophila). Bee-ness is a FAMILY test -- apoid wasps share superfamily Apoidea.

test_that("keep_bee_specimens keeps bee families, drops wasps/flies/unidentified", {
  src("specimens/specimen_clean_helpers.R")
  df <- tibble::tibble(
    ucsd_id = 1:6,
    genus   = c("Lasioglossum", "Apis", "Tiphia", NA, NA, "Andrena"),
    family  = c("Halictidae", "Apidae", "Tiphiidae", "", NA_character_, "andrenidae"),
    order   = c("Hymenoptera", "Hymenoptera", "Hymenoptera", "Diptera", NA, "Hymenoptera"))
  out <- keep_bee_specimens(df)
  expect_equal(sort(out$ucsd_id), c(1L, 2L, 6L))       # 3 bee-family rows (case-insensitive)
  expect_true(all(tolower(out$family) %in% tolower(BEE_FAMILIES)))
  expect_false("Tiphia" %in% out$genus)                # wasp dropped
})

test_that("keep_bee_specimens covers all seven Anthophila families, superfamily-blind", {
  src("specimens/specimen_clean_helpers.R")
  df <- tibble::tibble(family = c("Andrenidae", "Apidae", "Colletidae", "Halictidae",
                                  "Megachilidae", "Melittidae", "Stenotritidae", "Crabronidae"))
  out <- keep_bee_specimens(df)
  expect_equal(nrow(out), 7)                            # all 7 bee families kept
  expect_false("Crabronidae" %in% out$family)          # apoid WASP (shares Apoidea) dropped
})

test_that("keep_bee_specimens fails CLOSED (stops) when there is no family column", {
  src("specimens/specimen_clean_helpers.R")
  df <- tibble::tibble(genus = c("Apis", "Tiphia"))
  expect_error(keep_bee_specimens(df), "no `family` column")   # halt, never pass non-bees through
})

test_that("keep_bee_specimens matches the family column name case-insensitively", {
  src("specimens/specimen_clean_helpers.R")
  df <- tibble::tibble(Family = c("Halictidae", "Tiphiidae"), genus = c("Lasioglossum", "Tiphia"))
  out <- keep_bee_specimens(df)
  expect_equal(nrow(out), 1)                            # bee kept via capitalized `Family` column
  expect_equal(out$genus, "Lasioglossum")
})

# fill_above_genus_ids(): a specimen ID'd only above genus (e.g. tribe Halictini,
# blank genus) gets its id/rank/scientific_name from the lookup's row at that rank.

test_that("fill_above_genus_ids resolves a tribe-only specimen (finest rank wins)", {
  src("specimens/specimen_clean_helpers.R")
  df <- tibble::tibble(
    genus = c(NA_character_, "Andrena"),
    family = c("Halictidae", "Andrenidae"),
    subfamily = c("Halictinae", NA_character_),
    tribe = c("Halictini", NA_character_),
    subtribe = NA_character_,
    taxon_id = c(NA_integer_, 123L),               # row 2 already resolved by genus join
    taxon_rank = c(NA_character_, "genus"),
    scientific_name = c(NA_character_, "Andrena"),
    common_name = NA_character_)
  lk <- tibble::tibble(
    rank = c("tribe", "family", "genus"),
    tribe = c("Halictini", NA_character_, NA_character_),
    family = c("Halictidae", "Halictidae", "Andrenidae"),
    subfamily = c("Halictinae", NA_character_, NA_character_),
    genus = c(NA_character_, NA_character_, "Andrena"),
    taxon_id = c(335597L, 49707L, 123L),
    scientific_name = c("Halictini", "Halictidae", "Andrena"),
    common_name = NA_character_)
  out <- fill_above_genus_ids(df, lk)
  expect_equal(out$taxon_id[1], 335597L)             # tribe (finest), not family 49707
  expect_equal(out$taxon_rank[1], "tribe")
  expect_equal(out$scientific_name[1], "Halictini")
  expect_equal(out$taxon_id[2], 123L)                # genus-resolved row untouched
})

test_that("fill_above_genus_ids leaves an ambiguous rank+name (>1 id) blank", {
  src("specimens/specimen_clean_helpers.R")
  df <- tibble::tibble(genus = NA_character_, family = "Halictidae", tribe = "Halictini",
                       taxon_id = NA_integer_, taxon_rank = NA_character_,
                       scientific_name = NA_character_, common_name = NA_character_)
  lk <- tibble::tibble(rank = c("tribe", "tribe"), tribe = c("Halictini", "Halictini"),
                       family = "Halictidae",
                       taxon_id = c(335597L, 1597678L),      # two ids -> ambiguous
                       scientific_name = c("Halictini", "Halictina"), common_name = NA_character_)
  out <- fill_above_genus_ids(df, lk)
  expect_true(is.na(out$taxon_id[1]))                # never guess
})

test_that("fill_above_genus_ids does NOT coarsen a genus-or-finer specimen", {
  src("specimens/specimen_clean_helpers.R")
  # genus present but its species absent from the lookup -> taxon_id must STAY NA,
  # NOT fall back to the family id.
  df <- tibble::tibble(genus = "Andrena", species = "notinlookup", family = "Andrenidae",
                       tribe = NA_character_, subtribe = NA_character_,
                       taxon_id = NA_integer_, taxon_rank = NA_character_,
                       scientific_name = NA_character_, common_name = NA_character_)
  lk <- tibble::tibble(rank = "family", family = "Andrenidae", tribe = NA_character_,
                       taxon_id = 111L, scientific_name = "Andrenidae", common_name = NA_character_)
  out <- fill_above_genus_ids(df, lk)
  expect_true(is.na(out$taxon_id[1]))                # has_genus -> skipped
})

# mask_out_of_park_flowers(): specimen rows whose flower isn't in the park (flower_in_park == FALSE)
# get flower_visited -> "flower - angiosperm" with plant identity cleared; raw label preserved.
test_that("mask_out_of_park_flowers hides not-in-park plants as 'flower - angiosperm'", {
  src("specimens/specimen_bee_clean.R")
  df <- data.frame(
    flower_visited     = c("Deinandra conjugens", "Encelia californica", "unresolved"),
    flower_visited_raw = c("Deinandra conjugens", "Encelia californica", "unresolved"),
    flower_taxon_id    = c("58821", "50000", NA),
    flower_in_park     = c(FALSE, TRUE, NA),
    plant_genus        = c("Deinandra", "Encelia", NA),
    plant_species      = c("Deinandra conjugens", "Encelia californica", NA),
    stringsAsFactors = FALSE)
  out <- mask_out_of_park_flowers(df)
  expect_equal(out$flower_visited[1], "flower - angiosperm")                       # FALSE -> masked
  expect_true(all(is.na(c(out$flower_taxon_id[1], out$plant_genus[1], out$plant_species[1]))))
  expect_equal(out$flower_visited_raw[1], "Deinandra conjugens")                   # raw label preserved
  expect_equal(out$flower_visited[2], "Encelia californica")                       # TRUE untouched
  expect_equal(out$flower_visited[3], "unresolved")                                # NA untouched
})
