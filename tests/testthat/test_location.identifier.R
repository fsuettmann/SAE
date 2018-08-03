library(SAE)

context("Testing of the function location identifier")

test_that("The location identifier works with numbers", {
  expect_equal(location.simplifier(c("A", "b", "1")), c(1,2,3))
})


# unsinniger Test, weil der Code nicht kompliziert ist?

