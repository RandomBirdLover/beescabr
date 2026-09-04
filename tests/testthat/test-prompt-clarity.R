# Taro's feedback on the cleaning prompts, verbatim: "what is this asking me to
# do? look it up in iNat? how do I know what the number is? what do you mean
# validate it in ITIS if it has a taxa name change."
#
# Every one of those is answerable, and the answers were in dev-docs. But the
# person hitting these is at a terminal at 9pm, not reading dev-docs. A prompt
# that needs a second document open is a broken prompt.
#
# So each one must carry, in the terminal, at the moment it asks:
#   * a link to click,
#   * what the answer keys mean,
#   * what the question is actually deciding.
src("reference/taxonomy/holway_reference_build.R")
src("reference/prompts/specimen_id_prompt.R")
src("reference/prompts/verify_prompt.R")

.said <- function(expr) {
  out <- character(0)
  withCallingHandlers(expr, message = function(m) {
    out <<- c(out, conditionMessage(m)); invokeRestart("muffleMessage")
  })
  paste(out, collapse = "")
}

test_that("the ITIS question gives a link and says what y and n do", {
  txt <- .said(itis_disposition("Lasioglossum turgiventre",
                                prompt_fn = function(p) { .p <<- p; "n" }))
  expect_match(txt, "itis.gov", fixed = TRUE)
  expect_match(txt, "Lasioglossum+turgiventre", fixed = TRUE)  # the search is pre-filled
  expect_match(txt, "valid", ignore.case = TRUE)               # what to look for
  expect_match(paste(txt, .p), "y", fixed = TRUE)
  expect_match(paste(txt, .p), "n", fixed = TRUE)
})

test_that("the ITIS question explains what ITIS is", {
  txt <- .said(itis_disposition("Andrena chalybaea", prompt_fn = function(p) "n"))
  expect_match(txt, "taxonom", ignore.case = TRUE)   # names it as a taxonomy database
})

test_that("the taxon_id question shows where to find the number", {
  add <- data.frame(scientific_name = "Andrena chalybaea", rank = "species",
                    genus = "Andrena", species = "chalybaea",
                    taxon_id = NA_integer_, stringsAsFactors = FALSE)
  txt <- .said(resolve_specimen_additions_interactive(
    add, fetch_fn = function(nm) list(), prompt_fn = function(p) { .q <<- p; "s" }))
  both <- paste(txt, .q)
  expect_match(both, "inaturalist.org", fixed = TRUE)
  expect_match(both, "Andrena+chalybaea", fixed = TRUE)   # the search is pre-filled
  expect_match(both, "taxon_id", fixed = TRUE)
})

test_that("the verify pass opens by saying what it is asking", {
  needs <- data.frame(taxon_id = 345235L, scientific_name = "Colletes hyalinus",
                      rank = "subspecies", verified = FALSE, stringsAsFactors = FALSE)
  txt <- .said(resolve_verification_interactive(needs, prompt_fn = function(p) ""))
  expect_match(txt, "San Diego", fixed = TRUE)
  expect_match(txt, "VERIFICATION.md", fixed = TRUE)   # where the full answer lives
})

# There are two prompt passes and they ask different questions. Told apart, they
# are obvious; run back to back with no labels, they look like the same question
# asked twice, and the reader cannot tell why they are being asked again.
#   pass 1  which iNaturalist taxon IS this bee?      -> gets a taxon_id
#   pass 2  is that taxon really found in San Diego?  -> accepts or rejects it
test_that("pass 1 says it is pass 1, and what pass 2 will do", {
  add <- data.frame(scientific_name = "Andrena chalybaea", rank = "species",
                    genus = "Andrena", species = "chalybaea",
                    taxon_id = NA_integer_, stringsAsFactors = FALSE)
  txt <- .said(resolve_specimen_additions_interactive(
    add, fetch_fn = function(nm) list(), prompt_fn = function(p) "s"))
  expect_match(txt, "PASS 1", fixed = TRUE)
  expect_match(txt, "PASS 2", fixed = TRUE)   # says what comes next
})

test_that("pass 2 says it is pass 2, and what pass 1 already did", {
  needs <- data.frame(taxon_id = 345235L, scientific_name = "Colletes hyalinus",
                      rank = "subspecies", verified = FALSE, stringsAsFactors = FALSE)
  txt <- .said(resolve_verification_interactive(needs, prompt_fn = function(p) ""))
  expect_match(txt, "PASS 2", fixed = TRUE)
  expect_match(txt, "PASS 1", fixed = TRUE)
})

test_that("the ITIS question is not mislabelled as one of the two passes", {
  txt <- .said(itis_disposition("Andrena chalybaea", prompt_fn = function(p) "n"))
  expect_false(grepl("PASS 1|PASS 2", txt))
})

# resolve_missing_ids.R already searched iNaturalist for every checklist bee it
# could not resolve and cached the verdict in resolved_missing_ids.csv
# ("not_found_or_ambiguous"). PASS 1 did not read that file, so it asked about
# those bees on every run -- Lasioglossum turgiventre among them -- and the only
# correct answer was always `s`. Asking a question you have already answered is
# how a person learns to stop reading the prompts.
test_that("PASS 1 does not ask about taxa the resolver already ruled out", {
  cache <- tempfile(fileext = ".csv")
  write.csv(data.frame(key = "species|lasioglossum turgiventre|57678",
                       taxon_id = NA, status = "not_found_or_ambiguous"),
            cache, row.names = FALSE)
  on.exit(unlink(cache), add = TRUE)

  add <- data.frame(scientific_name = "Lasioglossum turgiventre", rank = "species",
                    genus = "Lasioglossum", species = "turgiventre",
                    taxon_id = NA_integer_, stringsAsFactors = FALSE)
  asked <- 0L
  txt <- .said(resolve_specimen_additions_interactive(
    add, fetch_fn = function(nm) list(), known_missing_path = cache,
    prompt_fn = function(p) { asked <<- asked + 1L; "s" }))
  expect_equal(asked, 0L)
  expect_match(txt, "already", ignore.case = TRUE)   # says why it was skipped
})

test_that("PASS 1 still asks about a taxon the resolver has not ruled out", {
  cache <- tempfile(fileext = ".csv")
  write.csv(data.frame(key = "species|something else|1", taxon_id = NA,
                       status = "not_found_or_ambiguous"), cache, row.names = FALSE)
  on.exit(unlink(cache), add = TRUE)
  add <- data.frame(scientific_name = "Andrena chalybaea", rank = "species",
                    genus = "Andrena", species = "chalybaea",
                    taxon_id = NA_integer_, stringsAsFactors = FALSE)
  asked <- 0L
  resolve_specimen_additions_interactive(add, fetch_fn = function(nm) list(),
    known_missing_path = cache, prompt_fn = function(p) { asked <<- asked + 1L; "s" })
  expect_equal(asked, 1L)
})
