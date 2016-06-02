# Testing the GUMBEL functions


param <- c(0.1, 1)
tolerance <- 0.2  # 20% tolerance on parameter estimation accuracy
expected_min <- param * (1 - tolerance)
expected_max <- param * (1 + tolerance)


test <- gumbel_mle(evd::rgumbel(1000, loc=param[1], scale=param[2]))

test_that("gumbel_mle returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 2)
  expect_length(test$se, 2)

  expect_true(test$estimate[1] < expected_max[1]  && test$estimate[1] > expected_min[1])
  expect_true(test$estimate[2] < expected_max[2]  && test$estimate[2] > expected_min[2])
})

test <- gumbel_mle(evd::rgumbel(1000, loc=param[1], scale=param[2]))

test_that("gumbel_Lmom returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 2)
  expect_length(test$se, 2)

  expect_true(test$estimate[1] < expected_max[1]  && test$estimate[1] > expected_min[1])
  expect_true(test$estimate[2] < expected_max[2]  && test$estimate[2] > expected_min[2])
})

test <- gumbel_mle(evd::rgumbel(1000, loc=param[1], scale=param[2]))

test_that("gumbel_mom returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 2)
  expect_length(test$se, 2)

  expect_true(test$estimate[1] < expected_max[1]  && test$estimate[1] > expected_min[1])
  expect_true(test$estimate[2] < expected_max[2]  && test$estimate[2] > expected_min[2])
})
