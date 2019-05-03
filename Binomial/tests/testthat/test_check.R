context("Test checkers")

test_that("check_trials arguments", {
  
  expect_true(check_trials(15))
  expect_error(check_trials(-15), "invalid trial value")
  expect_error(check_trials(1.5), "invalid trial value")
})

test_that("check_prob arguments",{
  
  expect_true(check_prob(.5))
  expect_error(check_prob(5), "invalid prob value")
  expect_error(check_prob(-.5), "invalid prob value")
})

test_that("check_success fails with invalid args", {
  
  expect_true(check_success(2, 5))
  expect_error(check_success(5, 2), "invalid success value")
  expect_error(check_success(-2, 5), "invalid success value")
})