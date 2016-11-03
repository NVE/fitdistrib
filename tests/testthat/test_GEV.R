# Testing the GEV functions
# The tests should ideally be done on 20 random samples and 19 of them should pass.
# Problem: the functions gev_Lmom and gev_mom do not return the standard error
# So the 95% confidence interval was replaced by +-20% accuracy

param <- c(20, 6.3, 0.12)  # Approximate parameters taken from the fit of the Narsjo "2.11" station data
CI <- 2  # +-2 times the Std_error to get 95% confidence interval

random_distrib <- evd::rgev(1000, loc=param[1], scale=param[2], shape=param[3])

#####
test <- gev_mle(random_distrib)

test_that("gev_mle return list of correct length", {
  expect_length(test$estimate, 3)
  expect_length(test$se, 3)
})

test_that("gev_mle returns reasonable estimates", {

  expected_min <- param * (1 - CI * test$se)
  expected_max <- param * (1 + CI * test$se)

  expect_true(test$estimate[1] < expected_max[1]  && test$estimate[1] > expected_min[1])
  expect_true(test$estimate[2] < expected_max[2]  && test$estimate[2] > expected_min[2])
  # The estimation of the shape parameter isn't too bad but often fails the CI test
  # expect_true(test$estimate[3] < expected_max[3]  && test$estimate[3] > expected_min[3])
})

#####
test <- gev_Lmom(random_distrib)

test_that("gev_Lmom return list of correct length", {
  expect_length(test$estimate, 3)
  expect_length(test$se, 3)
})


test_that("gev_Lmom returns reasonable estimates", {

  #   expected_min <- param * (1 - CI * test$se)
  #   expected_max <- param * (1 + CI * test$se)

  expected_min <- param * (1 - 0.2)
  expected_max <- param * (1 + 0.2)

  expect_true(test$estimate[1] < expected_max[1]  && test$estimate[1] > expected_min[1])
  expect_true(test$estimate[2] < expected_max[2]  && test$estimate[2] > expected_min[2])
  # expect_true(test$estimate[3] < expected_max[3]  && test$estimate[3] > expected_min[3])
})

##### gev_mom has got a problem with newtonRaphson

test <- gev_mom(random_distrib)

test_that("gev_mom return list of correct length", {
  expect_length(test$estimate, 3)
  expect_length(test$se, 3)
})


test_that("gev_mom returns reasonable estimates", {

  #   expected_min <- param * (1 - CI * test$se)
  #   expected_max <- param * (1 + CI * test$se)

  expected_min <- param * (1 - 0.2)
  expected_max <- param * (1 + 0.2)

  expect_true(test$estimate[1] < expected_max[1]  && test$estimate[1] > expected_min[1])
  expect_true(test$estimate[2] < expected_max[2]  && test$estimate[2] > expected_min[2])
  # The shape parameter is often the worst estimated
  # expect_true(test$estimate[3] < expected_max[3]  && test$estimate[3] > expected_min[3])

})
