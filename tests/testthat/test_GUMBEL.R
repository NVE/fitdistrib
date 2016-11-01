# Testing the GUMBEL functions
# Problem: the function gumbel_Lmom does not return the standard error
# So the 95% confidence interval was replaced by +-20% accuracy

param <- c(5, 0.5)
CI <- 2  # +-2 times the Std_error to get 95% confidence interval

random_distrib <- evd::rgumbel(1000, loc=param[1], scale=param[2])

####
test <- gumbel_mle(random_distrib)

test_that("gumbel_mle return list of correct length", {
  expect_length(test$estimate, 2)
  expect_length(test$se, 2)
})

test_that("gumbel_mle returns reasonable estimates", {

  expected_min <- param * (1 - CI * test$se)
  expected_max <- param * (1 + CI * test$se)

#   expected_min <- param * (1 - 0.2)
#   expected_max <- param * (1 + 0.2)

  expect_true(test$estimate[1] < expected_max[1]  && test$estimate[1] > expected_min[1])
  expect_true(test$estimate[2] < expected_max[2]  && test$estimate[2] > expected_min[2])
})


####
test <- gumbel_Lmom(random_distrib)

test_that("gumbel_Lmom return list of correct length", {
  expect_length(test$estimate, 2)
  expect_length(test$se, 2)
})

test_that("gumbel_Lmom returns reasonable estimates", {

  #    expected_min <- param * (1 - CI * test$se)
  #    expected_max <- param * (1 + CI * test$se)

  expected_min <- param * (1 - 0.2)
  expected_max <- param * (1 + 0.2)

  expect_true(test$estimate[1] < expected_max[1]  && test$estimate[1] > expected_min[1])
  expect_true(test$estimate[2] < expected_max[2]  && test$estimate[2] > expected_min[2])
})



# plot4server  <- function(dat, param, distr.index = 1) {
#   # plot fitted probability density function to estimated empirical pdf
#   # Returns nothing, saves nothing
#   xmax <- max(dat)*1.2
#   xmin <- min(dat)
#   x <- seq(0, xmax, xmax / 100)
#   # distr <- distr.name[distr.index]
#
#   # Distribution specific y vector
#   # PB: there is some logic erro with the NA management here. The app works, but this could be improved
#   if(distr.index == 1 && all(is.na(param)) == FALSE)   y <- evd::dgumbel(x, param[1], param[2])
#   if(distr.index == 2 && all(is.na(param)) == FALSE)    y <- dgamma(x, param[1], param[2])
#   if(distr.index == 3 && all(is.na(param)) == FALSE)      y <- evd::dgev(x, param[1], param[2], param[3])
#   if(distr.index == 4 && all(is.na(param)) == FALSE)       y <- f.genlogis(x, param[1], param[2], param[3])
#   if(distr.index == 5 && all(is.na(param)) == FALSE)  y <- f.gamma(x, param[1], param[2], param[3])
#
#   ymax <- max( max(na.omit(y)), max(na.omit(density(dat)$y)) ) * 1.1
#
#   # Plotting input dat, this is common to all distributions
#   #   hist(dat, xlab = "Flood discharge (m3/s)",ylab = "Probability density",freq = FALSE,
#   #        breaks = seq(0, xmax, xmax / 15), col = "gray", main = NULL, xlim = c(0, xmax), ylim = c(0, ymax))
#   #   par(new = TRUE)
#   plot(x, y, xlim = c(xmin, xmax), ylim = c(0, ymax), type = "l", lwd = 2, col = "black", xlab = "", ylab = "")
#   par(new = TRUE)
#   plot(density(dat), main = "Density distribution and data histogramm",
#        xlim = c(xmin, xmax), ylim = c(0, ymax), lty = 1, lwd = 3, col = "blue", xlab = "", ylab = "")
#
#   legend("topright", inset = .05, c("Model","Empirical" ), col = c("black","blue"),lty = c(1, 1),lwd=c(2, 3),
#          merge = TRUE, bg = "gray90")
# }
#
# plot4server(random_distrib, param)
# plot4server(random_distrib, test$estimate)
