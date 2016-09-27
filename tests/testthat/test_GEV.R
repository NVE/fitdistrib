# Testing the GEV functions

# bla bla to see if git sees it

param <- c(0.1, 1, 0.1)
tolerance <- 0.5  # 20% tolerance on parameter estimation accuracy
expected_min <- param * (1 - tolerance)
expected_max <- param * (1 + tolerance)


test <- gev_mle(evd::rgev(1000, loc=param[1], scale=param[2], shape=param[3]))

test_that("gev_mle returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 3)
  expect_length(test$se, 3)

  expect_true(test$estimate[1] < expected_max[1]  && test$estimate[1] > expected_min[1])
  expect_true(test$estimate[2] < expected_max[2]  && test$estimate[2] > expected_min[2])
  expect_true(test$estimate[3] < expected_max[3]  && test$estimate[3] > expected_min[3])
})

test <- gev_Lmom(evd::rgev(1000, loc=param[1], scale=param[2], shape=param[3]))

test_that("gev_Lmom returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 3)
  expect_length(test$se, 3)

  expect_true(test$estimate[1] < expected_max[1]  && test$estimate[1] > expected_min[1])
  expect_true(test$estimate[2] < expected_max[2]  && test$estimate[2] > expected_min[2])
  expect_true(test$estimate[3] < expected_max[3]  && test$estimate[3] > expected_min[3])
})

# test <- gev_mom(evd::rgev(1000, loc=param[1], scale=param[2], shape=param[3]))
#
# test_that("gev_mom returns reasonable estimates", {
#   expect_length(test, 2)
#   expect_length(test$estimate, 3)
#   expect_length(test$se, 3)
#
#   expect_true(test$estimate[1] < expected_max[1]  && test$estimate[1] > expected_min[1])
#   expect_true(test$estimate[2] < expected_max[2]  && test$estimate[2] > expected_min[2])
#   expect_true(test$estimate[3] < expected_max[3]  && test$estimate[3] > expected_min[3])
# })

