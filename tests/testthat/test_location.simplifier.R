library(SAE)

context("Testing of the function location.simplifier.r")

test_that("the location from the surveydata is incorrect", {
  expect_equal(length(location.simplifier(
    surv_data = data.frame(a = c(1,2,3,4), b = c(10,20,30,40)), location = c(5,6,7,8))), 4)
  expect_equal(length(location.simplifier(
    surv_data = data.frame(a = c(1,2,3,4), b = c(10,20,30,40)), location = "b")), 4)
  expect_error(length(location.simplifier(
    surv_data = data.frame(a = c(1,2,3,4), b = c(10,20,30,40)), location = c(5,6,7))),
    "location has to be a string or a vector of the same length as the surveydata")
})
