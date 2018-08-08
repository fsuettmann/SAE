library(SAE)

context("Checks if all the objects in the general sae function body interact in the right way")

surveydata <-  data.frame(a = c(1,2,4,5,6,7), b = c(2,3,4,1,4,1), c= c(2,4,3,1,5,7))
censusdata <-  data.frame(a = c(1,2,6,7,5,1,39,6), b = c(2,3,6,3,4,7,2,8), c= c(2,4,5,1,8,9,6,4))
model <- a ~ b + c
location_survey <- "c"


test_that("the function works", {
  expect_equal(length(sae(model, surveydata , censusdata, location_survey, n_boot = 5, welfare.function = identity)),8)

})


test_that("different welfare functions can be introduced", {
  expect_equal(length(sae(model, surveydata , censusdata, location_survey, n_boot = 5, welfare.function = function(x){2*x})),8)
  expect_equal(length(sae(model, surveydata , censusdata, location_survey, n_boot = 5, welfare.function = function(x){log(x^2 + 2)})),8)
})


