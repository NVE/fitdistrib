# Testing the GL functions

param <- c(100, 10, 0.1)
tolerance <- 0.5  # 20% tolerance on parameter estimation accuracy
expected_min <- param * (1 - tolerance)
expected_max <- param * (1 + tolerance)

# test <- gl_mle(nsRFA::rand.genlogis(1000, param[1],param[2],param[3]))
#
# test_that("gl_mle returns reasonable estimates", {
#   expect_length(test, 2)
#   expect_length(test$estimate, 3)
#   expect_length(test$se, 3)
#
#   expect_true(test$estimate[1] < expected_max[1]  && test$estimate[1] > expected_min[1])
#   expect_true(test$estimate[2] < expected_max[2]  && test$estimate[2] > expected_min[2])
#   expect_true(test$estimate[3] < expected_max[3]  && test$estimate[3] > expected_min[3])
# })

test <- gl_Lmom(nsRFA::rand.genlogis(1000, param[1],param[2],param[3]))

test_that("gl_Lmom returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 3)
  expect_length(test$se, 3)

  expect_true(test$estimate[1] < expected_max[1]  && test$estimate[1] > expected_min[1])
  expect_true(test$estimate[2] < expected_max[2]  && test$estimate[2] > expected_min[2])
  expect_true(test$estimate[3] < expected_max[3]  && test$estimate[3] > expected_min[3])
})

test <- gl_mom(nsRFA::rand.genlogis(1000, param[1],param[2],param[3]))

test_that("gl_mom returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 3)
  expect_length(test$se, 3)

  expect_true(test$estimate[1] < expected_max[1]  && test$estimate[1] > expected_min[1])
  expect_true(test$estimate[2] < expected_max[2]  && test$estimate[2] > expected_min[2])
  expect_true(test$estimate[3] < expected_max[3]  && test$estimate[3] > expected_min[3])
})
