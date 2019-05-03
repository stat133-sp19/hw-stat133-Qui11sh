context("Check main functions")

test_that("check bin_choose has proper args and returns correct result", {

  expect_equal(bin_choose(10, 5), 252)
  expect_equal(bin_choose(5, 1:5),  c(5, 10, 10, 5, 1))
  expect_error(bin_choose(5, 10),  "invalid value of k")
  expect_error(bin_choose(-5, 2),  "invalid value of k")
})

test_that("check bin_probability has proper args and returns corrct results", {

  expect_equal(bin_probability(3, 5, .5), 0.3125)
  expect_equal(bin_probability(2, 5, .5), 0.3125)
  expect_equal(bin_probability(4:6, 10, .25), c(0.1459980, 0.0583992, 0.0162220))
})

test_that("check bin_distribution has proper args and returns correct results", {

  expect_error(bin_distribution(3, 3))
  expect_equal(head(bin_distribution(3, .3)$probability), c(0.343, 0.441, 0.189, 0.027))
  expect_equal(head(bin_distribution(3, .3)$success), 0:3)

})

test_that("check bin_cumulative has proper args and returns correct results", {

  expect_error(bin_cumulative(3, 3))
  expect_equal(head(bin_cumulative(3, .2)$cumulative), c(0.512, 0.896, 0.992, 1.000))
  expect_equal(head(bin_distribution(3, .3)$probability), c(0.343, 0.441, 0.189, 0.027))
  expect_equal(head(bin_cumulative(3, .2)$success), 0:3)
})
