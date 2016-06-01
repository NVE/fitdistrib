# GAMMA.R
# All function related to fitting the Gamma distribution

#' Fitting the gamma distribution with MLE
#' @description Function to fit the gamma distribution with the maximum likelihood method
#' @param dat the data that needs fitting (i.e. flood data)
#' @return param Estimated parameters (2) and standard error returned as a list($estimate, $se)
#' @importFrom MASS fitdistr
#' @export
#'
#' @examples gamma_mle(rgamma(1000, loc=0, scale=1))
gamma_mle <- function(dat) {
# Fit GAMMA distribution with Maximum Likelihood Estimator
# Returns param as a list($estimate, $se)

  param <- list(estimate = c(NA, NA), se = c(NA, NA))
  if (length(dat) >= GLOBAL_min_years_data) {

    fail_safe <- failwith(NULL, fitdistr)
    temp.param <- fail_safe(dat, "gamma")
    if (!is.null(temp.param)) {
      param$estimate <- temp.param$estimate
      param$se <- temp.param$sd
      invisible(param)
    } else {
      print("Warning: the function fitdistr failed in gamma_mle")
      invisible(param)
    }

  } else {
    print(paste("Warning: this station has less than ", GLOBAL_min_years_data," years of data. Use another method!",
                  collapse="",sep=""))
    invisible(param)
  }
}

#' Fitting the gamma distribution with Lmom
#' @description Function to fit the gamma distribution with the linear moment method
#' @param dat the data that needs fitting (i.e. flood data)
#' @return param Estimated parameters (2) and standard error returned as a list($estimate, $se)
#' @importFrom fitdistrplus mmedist
#' @export
#'
#' @examples gamma_Lmom(rgamma(1000, loc=0, scale=1))
gamma_Lmom <- function(dat) {
# Fit GAMMA distribution with Lmoment estimator
# Returns param as a list($estimate, $se)

  param <- list(estimate = c(NA, NA), se = c(NA, NA))
  if (length(dat) >= GLOBAL_min_years_data) {
    # param <- mmedist(dat,"gamma")

    fail_safe <- failwith(NULL, mmedist)
    temp.param <- fail_safe(dat, "gamma")
    if (!is.null(temp.param)) {
      # Standard error is not yet implemented
      param$estimate <- temp.param$estimate
      invisible(param)
    } else {
      print("Warning: The function fitdistr failed in gamma_mle")
      param <- list(estimate = c(NA, NA), se = c(NA, NA))
      invisible(param)
    }

  } else {
    print(paste("Warning: this station has less than ",GLOBAL_min_years_data," years of data. Use another method!",
                  collapse="",sep=""))
    invisible(param)
  }
}

#' Fitting the gamma distribution with mom
#' @description Function to fit the gamma distribution with the ordinary moments method
#' @param dat the data that needs fitting (i.e. flood data)
#' @return param Estimated parameters (2) and standard error returned as a list($estimate, $se)
#' @export
#'
#' @examples gamma_mom(rgamma(1000, loc=0, scale=1))
gamma_mom <- function(dat) {
# Fit GAMMA distribution with ordinary moment estimator
# Returns param as a list($estimate, $se)

  param <- list(estimate = c(NA, NA), se = c(NA, NA))
  if (length(dat) >= GLOBAL_min_years_data) {
    sigma <- moments(dat)[2]
    mu <-  moments(dat)[1]
    param$estimate <- c()
    param$estimate[2] <- mu / sigma^2
    param$estimate[1] <- mu^2 / sigma^2
    # Standard error is not yet implemented
    invisible(param)
  } else {
    print(paste("Warning: this station has less than ",GLOBAL_min_years_data," years of data. Use another method!",
                  collapse="",sep=""))
    invisible(param)
  }
}

#' Fitting the gamma distribution with Bayesian inference
#' @description Function to fit the gamma distribution with bayes method
#' This is a dummy function because this method has not been implemented yet
#' @param dat the data that needs fitting (i.e. flood data)
#' @return param Estimated parameters and standard error returned as a list($estimate, $se)
#' @export
#'
#' @examples gamma_bayes(rgamma(1000, loc=0, scale=1))
gamma_bayes <- function(dat) {

  param <- list(estimate = c(NA, NA), se = c(NA, NA))
  if (length(dat) >= GLOBAL_min_years_data) {
    print("Returning NAs. This function has not been implemented yet!")
    invisible(param)
  } else {
  print(paste("Warning: this station has less than ", GLOBAL_min_years_data," years of data. Use another method!",
              collapse = "", sep = ""))
  invisible(param)
  }
}
