# Testing the GL functions

tolerance <- 0.1  # 10% tolerance on parameter estimation accuracy

test <- gl_mle(nsRFA::rand.genlogis(1000, 0, 1, 1))

test_that("gl_mle returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 3)
  expect_length(test$se, 3)
  expect_true(abs(test$estimate[1]) < tolerance)
  expect_true(test$estimate[2] < 1 * (1 + tolerance)  && test$estimate[2] > 1 * (1 - tolerance))
  expect_true(test$estimate[3] < 1 * (1 + tolerance)  && test$estimate[2] > 1 * (1 - tolerance))
})

test <- gl_Lmom(nsRFA::rand.genlogis(1000, 0, 1, 1))

test_that("gl_Lmom returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 3)
  expect_length(test$se, 3)
  expect_true(abs(test$estimate[1]) < tolerance)
  expect_true(test$estimate[2] < 1 * (1 + tolerance)  && test$estimate[2] > 1 * (1 - tolerance))
  expect_true(test$estimate[3] < 1 * (1 + tolerance)  && test$estimate[2] > 1 * (1 - tolerance))
})

test <- gl_mom(nsRFA::rand.genlogis(1000, 0, 1, 1))

test_that("gl_mom returns reasonable estimates", {
  expect_length(test, 2)
  expect_length(test$estimate, 3)
  expect_length(test$se, 3)
  expect_true(abs(test$estimate[1]) < tolerance)
  expect_true(test$estimate[2] < 1 * (1 + tolerance)  && test$estimate[2] > 1 * (1 - tolerance))
  expect_true(test$estimate[3] < 1 * (1 + tolerance)  && test$estimate[2] > 1 * (1 - tolerance))
})
