# Testing the gamma functions
# Problem: the functions gamma_Lmom and gamma_mom do not return the standard error
# So the 95% confidence interval was replaced by +-20% accuracy

param <- c(100, 1)
CI <- 2  # +-2 times the Std_error to get 95% confidence interval

random_distrib <- rgamma(1000, shape=param[1], scale=param[2])

####
test <- gamma_mle(random_distrib)

test_that("gamma_mle return list of correct length", {
  expect_length(test$estimate, 2)
  expect_length(test$se, 2)
})

test_that("gamma_mle returns reasonable estimates", {

  expected_min <- param * (1 - CI * test$se)
  expected_max <- param * (1 + CI * test$se)

  #   expected_min <- param * (1 - 0.2)
  #   expected_max <- param * (1 + 0.2)

  expect_true(test$estimate[1] < expected_max[1]  && test$estimate[1] > expected_min[1])
  expect_true(test$estimate[2] < expected_max[2]  && test$estimate[2] > expected_min[2])
})


####
test <- gamma_Lmom(random_distrib)

test_that("gamma_Lmom return list of correct length", {
  expect_length(test$estimate, 2)
  expect_length(test$se, 2)
})

test_that("gamma_Lmom returns reasonable estimates", {

#   expected_min <- param * (1 - CI * test$se)
#   expected_max <- param * (1 + CI * test$se)

    expected_min <- param * (1 - 0.2)
    expected_max <- param * (1 + 0.2)

  expect_true(test$estimate[1] < expected_max[1]  && test$estimate[1] > expected_min[1])
  expect_true(test$estimate[2] < expected_max[2]  && test$estimate[2] > expected_min[2])
})

####
test <- gamma_mom(random_distrib)

test_that("gamma_Lmom return list of correct length", {
  expect_length(test$estimate, 2)
  expect_length(test$se, 2)
})

test_that("gamma_Lmom returns reasonable estimates", {

  #   expected_min <- param * (1 - CI * test$se)
  #   expected_max <- param * (1 + CI * test$se)

  expected_min <- param * (1 - 0.2)
  expected_max <- param * (1 + 0.2)

  expect_true(test$estimate[1] < expected_max[1]  && test$estimate[1] > expected_min[1])
  expect_true(test$estimate[2] < expected_max[2]  && test$estimate[2] > expected_min[2])
})






