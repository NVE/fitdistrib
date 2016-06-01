# PEARSON.R
# All function related to fitting the Pearson III distribution

#' Fitting the pearson distribution with MLE
#' @description Function to fit the pearson distribution with the maximum likelihood method
#' @param dat the data that needs fitting (i.e. flood data)
#' @return param Estimated parameters and standard error returned as a list($estimate, $se)
#' @importFrom nsRFA ML_estimation
#' @export
#'
#' @examples pearson_mle(nsRFA::rand.gamma(1000, 500, 100, 10))
pearson_mle <- function(dat) {
# Fit PEARSON III distribution with Maximum Likelihood Estimator (nsRFA package)
# Returns param as a list($estimate, $se)

  param <- list(estimate = c(NA, NA, NA), se = c(NA, NA, NA))
  if (length(dat) >= GLOBAL_min_years_data) {

    param$estimate <- ML_estimation (dat, dist = "P3")
    param$se <- c(NA, NA, NA)  # Standard error is not yet implemented
    invisible(param)
  } else {
    print(paste("Warning: this station has less than ", GLOBAL_min_years_data," years of data. Use another method!",
                  collapse="",sep=""))
    invisible(param)
    }
}

#' Fitting the pearson distribution with Lmom
#' @description Function to fit the pearson distribution with the linear moment method
#' @param dat the data that needs fitting (i.e. flood data)
#' @return param Estimated parameters and standard error returned as a list($estimate, $se).
#' Standard error is not yet implemented
#' @importFrom nsRFA par.gamma
#' @export
#'
#' @examples pearson_Lmom(nsRFA::rand.gamma(1000, 500, 100, 10))
pearson_Lmom <- function(dat) {
# Fit PEARSON III distribution with the Lmoments
# Returns param as a list($estimate, $se)

  param <- list(estimate = c(NA, NA, NA), se = c(NA, NA, NA))
  if (length(dat) >= GLOBAL_min_years_data) {
    dat.mom <- Lmoments(dat)
    # ADD FAILSAFE
    fitted_param <- par.gamma(dat.mom[1], dat.mom[2], dat.mom[4])  # dat.mom[3] is not the skewness, it is the CV
    # Creating the returning list
    param$estimate <- c(fitted_param$xi, fitted_param$beta, fitted_param$alfa)
    param$se <- c(NA, NA, NA) # Standard error is not yet implemented
    invisible(param)
  } else {
    print(paste("Warning: this station has less than ",GLOBAL_min_years_data," years of data. Use another method!",
                  collapse="",sep=""))
    invisible(param)
    }
}

#' Fitting the pearson distribution with mom
#' @description Function to fit the pearson distribution with the ordinary moments method
#' @param dat the data that needs fitting (i.e. flood data)
#' @return param Estimated parameters and standard error returned as a list($estimate, $se)
#' Standard error is not yet implemented
#' @importFrom nsRFA moment_estimation
#' @export
#'
#' @examples pearson_mom(nsRFA::rand.gamma(1000, 500, 100, 10))
pearson_mom <- function(dat) {
  # Fit PEARSON III distribution with method of moments (nsRFA package)
  # Returns param as a list($estimate, $se)

  param <- list(estimate = c(NA, NA, NA), se = c(NA, NA, NA))
  if (length(dat) >= GLOBAL_min_years_data) {
    param$estimate <- moment_estimation(dat, dist = "P3")  # Standard error is not yet implemented
    invisible(param)
  } else {
    print(paste("Warning: this station has less than ", GLOBAL_min_years_data," years of data. Use another method!",
                collapse="",sep=""))
    invisible(param)
  }
}

#' Fitting the pearson distribution with Bayesian inference
#' @description Function to fit the pearson distribution with BayesianMCMC method
#' WE assume that the shape parameter only has a prior with mean zero and standard deviation 0.2 (dnorm(x[3], 0, 0.2))
#' @param dat the data that needs fitting (i.e. flood data)
#' @return param Estimated parameters and standard error returned as a list($estimate, $se)
#' @export
#'
#' @examples pearson_bayes(xxx(10000, loc=0, scale=1, shape=0))
pearson_bayes <- function(dat) {
# Fit PEARSON distribution with the Bayesian method

  param <- list(estimate = c(NA, NA, NA), se = c(NA, NA, NA))

  if (length(dat) >= GLOBAL_min_years_data) {
    # Prior for Bayes
    myprior <- function (x) {
      # x = vector of parameter values: c(location, scale, shape)
      # I assume the shape parameter only has a prior with mean zero and standard deviation 0.2
      dnorm(x[3], 0, 0.2)
    }

    fail_safe <- failwith(NULL, BayesianMCMC)
    bayes <- fail_safe(dat, nbpas = 5000, nbchaines = 3, confint = c(0.05, 0.95), dist = "P3")
        # PB Doesn't work or yields poor results with a prior
    # bayes <- fail_safe(dat, nbpas = 5000, nbchaines = 3, confint = c(0.05, 0.95), dist = "P3",apriori=myprior)

    if (is.null(bayes) == TRUE) {
      print("Warning: the function BayesianMCMC failed in pearson_bayes")
      invisible(param)
    } else {

      ## Addition to return parameters
      #   # Solution 1
      param$estimate <- bayes$parametersML

#       # Solution 2
#       param$estimate[1] <- mean(as.vector(bayes$parameters[, 1, 1:3]))
#       param$estimate[2] <- mean(as.vector(bayes$parameters[, 2, 1:3]))
#       param$estimate[3] <- mean(as.vector(bayes$parameters[, 3, 1:3]))

      param$se[1] <- sd(as.vector(bayes$parameters[, 1, 1:3]))
      param$se[2] <- sd(as.vector(bayes$parameters[, 2, 1:3]))
      param$se[3] <- sd(as.vector(bayes$parameters[, 3, 1:3]))

      invisible(param)
    }
  } else {
    print(paste("Warning: this station has less than ", GLOBAL_min_years_data," years of data. Use another method!",
                collapse = "", sep = ""))
    invisible(param)
  }
}

#' Calculating the posterior predictive distribution
#' @description Function to calculate the posterior predictive distribution after calling pearson_bayes
#' @param (mmrp, mupars, spars, kpars) parameters returned by pearson_bayes. mupars, spars, kpars are the ensemble of param$estimate
#' @return param Estimated parameters and standard error returned as a list($estimate, $se)
#' @importFrom nsRFA invF.gamma
#' @export
#'
#' @examples Needs example
get_posterior_PEARSON <- function(mmrp,mupars,spars,kpars) {
# Function for calculating the posterior predictive distribution

  qqsample1 <- sapply(seq(length(mupars)), function(st) {
    mean_temp <- mupars[st]
    st_temp <- spars[st]
    k_temp <- kpars[st]
    invF.gamma(F = (1 - 1 / mmrp), mean_temp, st_temp, k_temp)
  }, simplify = "array")
  # 1 is for collums only 0.5 to return the median
  qqr <- apply(qqsample1, 1, quantile, 0.5)
}
