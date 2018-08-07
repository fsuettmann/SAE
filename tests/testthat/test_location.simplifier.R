library(SAE)

context("Testing of the function location.simplifier.r")

test_that("the function works correctly with a numerical vector of correct length specified", {
  expect_equal(location.simplifier(
    surv_data = data.frame(a = c(1,2,3,4), b = c(10,20,30,40)), location = c(8,6,2,4), n_obs_survey1 = 4),
    c(4,3,1,2))
  expect_equal(location.simplifier(
    surv_data = data.frame(a = c(1,2,3,4), b = c(10,20,30,40)), location = c(5,6,7,8), n_obs_survey1 = 4),
    c(1,2,3,4))
  expect_equal(length(location.simplifier(
    surv_data = data.frame(a = c(1,2,3,4), b = c(10,20,30,40)), location = c(5,6,7,8), n_obs_survey1 = 4)), 4)
  expect_error(length(location.simplifier(
    surv_data = data.frame(a = c(1,2,3,4), b = c(10,20,30,40)), location = c(5,6,7), n_obs_survey1 = 4)),
    "location has to be a string indicating one of the variabeles in the data set or a vector of the same length as the surveydata")
})

test_that("the function works correctly with a string vector of correct length specified", {
  expect_equal(location.simplifier(
    surv_data = data.frame(a = c(1,2,3,4), b = c(10,20,30,40)), location = c("x", "z", "m", "d"), n_obs_survey1 = 4),
    c(3,4,2,1))
  expect_equal(length(location.simplifier(
    surv_data = data.frame(a = c(1,2,3,4), b = c(10,20,30,40)), location = c("x", "z", "m", "d"), n_obs_survey1 = 4)), 4)
  expect_error(length(location.simplifier(
    surv_data = data.frame(a = c(1,2,3,4), b = c(10,20,30,40)), location = c("x", "z", "m"), n_obs_survey1 = 4)),
    "location has to be a string indicating one of the variabeles in the data set or a vector of the same length as the surveydata")
})

test_that("the function works correctly with the name of a variable in the dataset specified", {
  expect_equal(location.simplifier(
    surv_data = data.frame(a = c(1,2,3,4), b = c(10,20,30,40)), location = "b", n_obs_survey1 = 4),
    c(1,2,3,4))
  expect_equal(length(location.simplifier(
    surv_data = data.frame(a = c(1,2,3,4), b = c(10,20,30,40)), location = "b", n_obs_survey1 = 4)), 4)
  expect_error(length(location.simplifier(
    surv_data = data.frame(a = c(1,2,3,4), b = c(10,20,30,40)), location = "c", n_obs_survey1 = 4)),
    "String that was specified as location is not the name of one of the variables in the survey data set.")
})

test_that("the function works correctly with a matrix", { # nicht hier testen, sondern bei der Checkerfunktion.
  expect_equal(location.simplifier(
    surv_data = data.frame(a = c(1,2,3,4), b = c(10,20,30,40)), location = matrix(c(5,6,7,8), ncol = 1), n_obs_survey1 = 4),
    c(1,2,3,4))
})
