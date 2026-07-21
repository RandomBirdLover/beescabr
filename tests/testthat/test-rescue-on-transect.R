library(testthat)
library(dplyr)
library(sf)

# fpi_surveyday_transect(): per surveyor-day, the resolved (majority) transect from KEEP obs.
# fpi_rescue_on_transect(): flip an untagged (flag) obs to a survey when it is by that surveyor,
#   on that surveyor's survey day, and within 50 m of the SAME transect they worked that day.

test_that("fpi_surveyday_transect returns one resolved transect per surveyor-day from keep obs", {
  src("project_info/rescue_on_transect_surveys.R")
  m <- tibble(
    obs_id=c("1","2","3","9"), observer=c("a","a","a","b"),
    observed_on=as.Date(c("2026-04-06","2026-04-06","2026-04-06","2026-04-06")),
    status=c("keep","keep","flag","keep"),
    transect=c("TP","TP","NA","BST"),
    surveyor_type=c("beeple","beeple",NA,"beeple"), survey_year=c("2026","2026",NA,"2026"))
  out <- fpi_surveyday_transect(m)
  expect_equal(out$resolved_transect[out$observer=="a"], "TP")   # from a's two keep TP obs
  expect_equal(out$resolved_transect[out$observer=="b"], "BST")
  expect_false("flag" %in% out$resolved_transect)                # the flag obs never votes
})

test_that("fpi_rescue_on_transect rescues on-transect untagged obs on a survey day, not others", {
  src("project_info/rescue_on_transect_surveys.R")
  # a synthetic TP transect line near CABR (WGS84); distances come back in metres
  line <- st_sfc(st_linestring(rbind(c(-117.2400,32.6700), c(-117.2390,32.6700))), crs=4326)
  tsf  <- st_sf(Name="TP", T="TP", geometry=line)

  m <- tibble(
    obs_id      = c("k1","f_on","f_off","f_notday"),
    observer    = c("a","a","a","a"),
    observed_on = as.Date(c("2026-04-06","2026-04-06","2026-04-06","2026-05-01")),
    status      = c("keep","flag","flag","flag"),
    transect    = c("TP", NA, NA, NA),
    surveyor_type = c("beeple", NA, NA, NA),
    survey_year = c("2026", NA, NA, NA),
    status_reason = NA_character_)
  coords <- tibble(
    obs_id   = c("k1","f_on","f_off","f_notday"),
    latitude = c(32.67000, 32.670045, 32.67200, 32.670045),   # f_on ~5m, f_off ~220m, f_notday ~5m but wrong day
    longitude= c(-117.2395, -117.2395, -117.2395, -117.2395))

  out <- fpi_rescue_on_transect(m, coords, tsf, off_m = 50)

  # f_on: on TP, same survey day -> rescued
  expect_equal(out$status[out$obs_id=="f_on"], "keep")
  expect_equal(out$survey_source[out$obs_id=="f_on"], "inferred_on_transect")
  expect_equal(out$transect[out$obs_id=="f_on"], "TP")
  expect_equal(out$surveyor_type[out$obs_id=="f_on"], "beeple")

  # f_off: too far from the transect -> untouched
  expect_equal(out$status[out$obs_id=="f_off"], "flag")

  # f_notday: on the line but NOT on a survey day for this surveyor -> untouched
  expect_equal(out$status[out$obs_id=="f_notday"], "flag")

  # the real tagged obs keeps its status and is marked tag-based
  expect_equal(out$status[out$obs_id=="k1"], "keep")
  expect_equal(out$survey_source[out$obs_id=="k1"], "tag")
})

test_that("fpi_rescue_on_transect handles numeric obs_id (iNat ids) without a join-type error", {
  src("project_info/rescue_on_transect_surveys.R")
  line <- st_sfc(st_linestring(rbind(c(-117.2400,32.6700), c(-117.2390,32.6700))), crs=4326)
  tsf  <- st_sf(Name="TP", T="TP", geometry=line)
  m <- tibble(obs_id=c(101,102), observer=c("a","a"),
              observed_on=as.Date(c("2026-04-06","2026-04-06")),
              status=c("keep","flag"), transect=c("TP",NA),
              surveyor_type=c("beeple",NA), survey_year=c("2026",NA), status_reason=NA_character_)
  coords <- tibble(obs_id=c(101,102), latitude=c(32.67,32.670045), longitude=c(-117.2395,-117.2395))
  out <- fpi_rescue_on_transect(m, coords, tsf, off_m=50)
  expect_equal(out$survey_source[out$obs_id=="102"], "inferred_on_transect")
})
