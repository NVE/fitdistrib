# Testing the GUMBEL functions

tolerance <- 0.1  # 10% tolerance on parameter estimation accuracy

test <- gumbel_mle(evd::rgumbel(1000, loc=0, scale=1))

test_that("gumbel_mle returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 2)
  expect_length(test$se, 2)
  expect_true(abs(test$estimate[1]) < tolerance)
  expect_true(test$estimate[2] < 1 * (1 + tolerance)  && test$estimate[2] > 1 * (1 - tolerance))
})

test <- gumbel_mle(evd::rgumbel(1000, loc=0, scale=1))

test_that("gumbel_Lmom returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 2)
  expect_length(test$se, 2)
  expect_true(abs(test$estimate[1]) < tolerance)
  expect_true(test$estimate[2] < 1 * (1 + tolerance)  && test$estimate[2] > 1 * (1 - tolerance))
})

test <- gumbel_mle(evd::rgumbel(1000, loc=0, scale=1))

test_that("gumbel_mom returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 2)
  expect_length(test$se, 2)
  expect_true(abs(test$estimate[1]) < tolerance)
  expect_true(test$estimate[2] < 1 * (1 + tolerance)  && test$estimate[2] > 1 * (1 - tolerance))
})
