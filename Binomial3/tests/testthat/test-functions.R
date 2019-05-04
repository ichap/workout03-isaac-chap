

context("Check binomial arguments")
library(testthat)

test_that("check_prob", {
  expect_true(check_prob(0.5))
  expect_error(check_prob(3), 'prob has to be a number betwen 0 and 1')
  expect_type(check_prob(p), "logical")
})

test_that("check_trials", {
  expect_true(check_trials(10))
  expect_length(check_trials(10), 1)
  expect_type(check_trials(10), "logical")
})


test_that("check_success", {
  expect_true(check_success(5, 10))
  expect_length(check_success(5, 10), 1)
  expect_type(check_success(5, 10), "logical")
})

test_that("aux_mean", {
  expect_lte(aux_mean(10, 0.5), 5)
  expect_gte(aux_mean(10, 0.5), 0)
  expect_type(aux_mean(10, 0.5), "double")
})


test_that("aux_variance", {
  expect_lte(aux_variance(10, 0.5), 10)
  expect_gte(aux_variance(10, 0.5), 0)
  expect_type(aux_variance(10, 0.5), "double")
})

test_that("aux_mode", {
  expect_lte(aux_mode(10, 0.5), 10)
  expect_gte(aux_mode(10, 0.5), 0)
  expect_type(aux_mode(10, 0.5), "double")
})

test_that("aux_skewness", {
  expect_equal(aux_skewness(10, 0.5), 0)
  expect_length(aux_skewness(10, 0.5), 1)
  expect_type(aux_skewness(10, 0.5), "double")
})

test_that("aux_kurtosis", {
  expect_equal(aux_kurtosis(10, 0.5), -0.2)
  expect_length(aux_kurtosis(10, 0.5), 1)
  expect_type(aux_kurtosis(10, 0.5), "double")
})

test_that("bin_choose", {
  expect_length(bin_choose(5, 2), 1)
  expect_type(bin_choose(5, 2), "double")
  expect_equal(bin_choose(5, 2), 10)
})

test_that("bin_probability", {
  expect_length(bin_probability(2, 5, 0.5), 1)
  expect_type(bin_probability(2, 5, 0.5), "double")
  expect_equal(bin_probability(2, 5, 0.5), 0.3125)
})

test_that("bin_distribution", {
  expect_length(bin_distribution(5, 0.5), 2)
  expect_type(bin_distribution(5, 0.5), "list")
  expect_error(bin_distribution(5, 2), 'prob has to be a number betwen 0 and 1')
})

test_that("bin_cumulative", {
  expect_length(bin_cumulative(5, 0.5), 3)
  expect_type(bin_cumulative(5, 0.5), "list")
  expect_error(bin_cumulative(5, 2), 'prob has to be a number betwen 0 and 1')
})
