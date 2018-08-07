library(SAE)

context("Evaluates the subfunctions in saeInferenceCensusData.R")

test_that("Check if output of bootstrap.location.effect.R is correct", {
  expect_equal(nrow(bootstrap.location(
    location_effect = as.data.frame(
      cbind("location_effect" = c(50, 100, 300, 70), "location" = c(1,2,3,4))),
    n_loc = 4,
    n_obs_cens = 100,
    n_bootstr = 50)), 100)
  expect_equal(ncol(bootstrap.location(
    location_effect = as.data.frame(
      cbind("location_effect" = c(50, 100, 300, 70), "location" = c(1,2,3,4))),
    n_loc = 4,
    n_obs_cens = 100,
    n_bootstr = 50)), 50)
})


