# Testing the gamma functions

param <- c(100, 1)
tolerance <- 0.2  # 20% tolerance on parameter estimation accuracy
expected_min <- param * (1 - tolerance)
expected_max <- param * (1 + tolerance)

test <- gamma_mle(rgamma(1000, shape=param[1], scale=param[2]))

test_that("gamma_mle returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 2)
  expect_length(test$se, 2)

  expect_true(test$estimate[1] < expected_max[1]  && test$estimate[1] > expected_min[1])
  expect_true(test$estimate[2] < expected_max[2]  && test$estimate[2] > expected_min[2])
})

test <- gamma_Lmom(rgamma(1000, shape=param[1], scale=param[2]))

test_that("gamma_Lmom returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 2)
  expect_length(test$se, 2)

  expect_true(test$estimate[1] < expected_max[1]  && test$estimate[1] > expected_min[1])
  expect_true(test$estimate[2] < expected_max[2]  && test$estimate[2] > expected_min[2])
})

test <- gamma_mom(rgamma(1000, shape=param[1], scale=param[2]))

test_that("gamma_mom returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 2)
  expect_length(test$se, 2)

  expect_true(test$estimate[1] < expected_max[1]  && test$estimate[1] > expected_min[1])
  expect_true(test$estimate[2] < expected_max[2]  && test$estimate[2] > expected_min[2])
})
