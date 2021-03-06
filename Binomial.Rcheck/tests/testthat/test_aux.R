context("Check Aux functions")

test_that("aux_mean works", {

  expect_equal(aux_mean(3, 1),3)
  expect_equal(aux_mean(10.5, .5), 5.25)
  expect_equal(aux_mean(20, .25), 5)
})


test_that("aux_variance works", {

  expect_equal(aux_variance(1, 1), 0)
  expect_equal(aux_variance(1, .5), 0.25)
  expect_equal(aux_variance(6, 0.25), 1.125)
})

test_that("aux_mode works", {

  expect_equal(aux_mode(1, 1), 2)
  expect_equal(aux_mode(1, .5), 1)
  expect_equal(aux_mode(6, 0.25), 2)
})

test_that("aux_skewness works", {

  expect_equal(aux_skewness(100, 0.3), 0.08728716)
  expect_equal(aux_skewness(1, .5), 0)
  expect_equal(aux_skewness(80, 0.45), 0.02247333)
})

test_that("aux_kurtosis works", {

  expect_equal(aux_kurtosis(100, 0.3), -0.01238095)
  expect_equal(aux_kurtosis(15, .3), -0.08253968)
  expect_equal(aux_kurtosis(6, 0.25), -0.1111111)
})
