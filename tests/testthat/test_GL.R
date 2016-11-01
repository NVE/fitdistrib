# Testing the GL functions
# Problem: fitting functions for GL do not return the standard error
# So the 95% confidence interval was replaced by +-20% accuracy

param <- c(1000, 100, 0.1)
CI <- 2  # +-2 times the Std_error to get 95% confidence interval

random_distrib <- nsRFA::rand.genlogis(1000, param[1],param[2],param[3])

####
test <- gl_mle(random_distrib)

 test_that("gl_mle return list of correct length", {
   expect_length(test$estimate, 3)
   expect_length(test$se, 3)
 })

 test_that("gl_mle returns reasonable estimates", {

#    expected_min <- param * (1 - CI * test$se)
#    expected_max <- param * (1 + CI * test$se)

   expected_min <- param * (1 - 0.2)
   expected_max <- param * (1 + 0.2)

   expect_true(test$estimate[1] < expected_max[1]  && test$estimate[1] > expected_min[1])
   expect_true(test$estimate[2] < expected_max[2]  && test$estimate[2] > expected_min[2])
   expect_true(test$estimate[3] < expected_max[3]  && test$estimate[3] > expected_min[3])
 })


####
test <- gl_Lmom(random_distrib)

test_that("gl_Lmom return list of correct length", {
  expect_length(test$estimate, 3)
  expect_length(test$se, 3)
})

test_that("gl_Lmom returns reasonable estimates", {

  #    expected_min <- param * (1 - CI * test$se)
  #    expected_max <- param * (1 + CI * test$se)

  expected_min <- param * (1 - 0.2)
  expected_max <- param * (1 + 0.2)

  expect_true(test$estimate[1] < expected_max[1]  && test$estimate[1] > expected_min[1])
  expect_true(test$estimate[2] < expected_max[2]  && test$estimate[2] > expected_min[2])
  expect_true(test$estimate[3] < expected_max[3]  && test$estimate[3] > expected_min[3])
})

####
test <- gl_mom(random_distrib)

test_that("gl_mom return list of correct length", {
  expect_length(test$estimate, 3)
  expect_length(test$se, 3)
})

test_that("gl_mom returns reasonable estimates", {

  #    expected_min <- param * (1 - CI * test$se)
  #    expected_max <- param * (1 + CI * test$se)

  expected_min <- param * (1 - 0.2)
  expected_max <- param * (1 + 0.2)

  expect_true(test$estimate[1] < expected_max[1]  && test$estimate[1] > expected_min[1])
  expect_true(test$estimate[2] < expected_max[2]  && test$estimate[2] > expected_min[2])
  expect_true(test$estimate[3] < expected_max[3]  && test$estimate[3] > expected_min[3])
})


