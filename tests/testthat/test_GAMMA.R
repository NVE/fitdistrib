# Testing the gamma functions

tolerance <- 0.1  # 10% tolerance on parameter estimation accuracy

test <- gamma_mle(rgamma(1000, shape=0, scale=1))

test_that("gamma_mle returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 2)
  expect_length(test$se, 2)
  expect_true(abs(test$estimate[1]) < tolerance)
  expect_true(test$estimate[2] < 1 * (1 + tolerance)  && test$estimate[2] > 1 * (1 - tolerance))
})

test <- gamma_Lmom(rgamma(1000, shape=0, scale=1))

test_that("gamma_Lmom returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 2)
  expect_length(test$se, 2)
  expect_true(abs(test$estimate[1]) < tolerance)
  expect_true(test$estimate[2] < 1 * (1 + tolerance)  && test$estimate[2] > 1 * (1 - tolerance))
})

test <- gamma_mom(rgamma(1000, shape=0, scale=1))

test_that("gamma_mom returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 2)
  expect_length(test$se, 2)
  expect_true(abs(test$estimate[1]) < tolerance)
  expect_true(test$estimate[2] < 1 * (1 + tolerance)  && test$estimate[2] > 1 * (1 - tolerance))
})
