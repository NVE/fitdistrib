# Testing the GEV functions

tolerance <- 0.1  # 10% tolerance on parameter estimation accuracy

test <- gev_mle(evd::rgev(10000, loc=0, scale=1, shape=0))

test_that("gev_mle returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 3)
  expect_length(test$se, 3)
  expect_true(abs(test$estimate[1]) < tolerance)
  expect_true(test$estimate[2] < 1 * (1 + tolerance)  && test$estimate[2] > 1 * (1 - tolerance))
  expect_true(abs(test$estimate[3]) < tolerance)
})

test <- gev_Lmom(evd::rgev(10000, loc=0, scale=1, shape=0))

test_that("gev_Lmom returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 3)
  expect_length(test$se, 3)
  expect_true(abs(test$estimate[1]) < tolerance)
  expect_true(test$estimate[2] < 1 * (1 + tolerance)  && test$estimate[2] > 1 * (1 - tolerance))
  expect_true(abs(test$estimate[3]) < tolerance)
})

test <- gev_mom(evd::rgev(10000, loc=0, scale=1, shape=0))

test_that("gev_mom returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 3)
  expect_length(test$se, 3)
  expect_true(abs(test$estimate[1]) < tolerance)
  expect_true(test$estimate[2] < 1 * (1 + tolerance)  && test$estimate[2] > 1 * (1 - tolerance))
  expect_true(abs(test$estimate[3]) < tolerance)
})

