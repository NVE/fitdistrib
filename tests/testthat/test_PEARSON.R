# Testing the pearson functions

tolerance <- 0.1  # 10% tolerance on parameter estimation accuracy

test <- pearson_mle(nsRFA::rand.gamma(1000, 500, 100, 10))

test_that("pearson_mle returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 3)
  expect_length(test$se, 3)
  expect_true(abs(test$estimate[1]) < tolerance)
  expect_true(test$estimate[2] < 1 * (1 + tolerance)  && test$estimate[2] > 1 * (1 - tolerance))
  expect_true(test$estimate[3] < 1 * (1 + tolerance)  && test$estimate[2] > 1 * (1 - tolerance))
})

test <- pearson_Lmom(nsRFA::rand.gamma(1000, 500, 100, 10))

test_that("pearson_Lmom returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 3)
  expect_length(test$se, 3)
  expect_true(abs(test$estimate[1]) < tolerance)
  expect_true(test$estimate[2] < 1 * (1 + tolerance)  && test$estimate[2] > 1 * (1 - tolerance))
  expect_true(test$estimate[3] < 1 * (1 + tolerance)  && test$estimate[2] > 1 * (1 - tolerance))
})

test <- pearson_mom(nsRFA::rand.gamma(1000, 500, 100, 10))

test_that("pearson_mom returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 3)
  expect_length(test$se, 3)
  expect_true(abs(test$estimate[1]) < tolerance)
  expect_true(test$estimate[2] < 1 * (1 + tolerance)  && test$estimate[2] > 1 * (1 - tolerance))
  expect_true(test$estimate[3] < 1 * (1 + tolerance)  && test$estimate[2] > 1 * (1 - tolerance))
})
