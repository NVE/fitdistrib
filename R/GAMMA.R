# GAMMA.R
# All function related to fitting the Gamma distribution

#' Fitting the gamma distribution with MLE
#' @description Function to fit the gamma distribution with the maximum likelihood method
#' @param dat the data that needs fitting (i.e. flood data)
#' @return param Estimated parameters (2) and standard error returned as a list($estimate, $se)
#' @importFrom MASS fitdistr
#' @export
#'
#' @examples library(FlomKartShinyApp)
#' estimate = gamma_mle(test_data)
#' FlomKartShinyApp::plot4server(test_data, param = estimate$estimate, distr = 2)
gamma_mle <- function(dat) {
# Fit GAMMA distribution with Maximum Likelihood Estimator
# Returns param as a list($estimate, $se)

  param <- list(estimate = c(NA, NA), se = c(NA, NA))
  if (length(dat) >= 1) {

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
    print(paste("Warning: this station has less than ", 1," years of data. Use another method!",
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
#' @examples library(FlomKartShinyApp)
#' estimate = gamma_Lmom(test_data)
#' FlomKartShinyApp::plot4server(test_data, param = estimate$estimate, distr = 2)
gamma_Lmom <- function(dat) {
# Fit GAMMA distribution with Lmoment estimator
# Returns param as a list($estimate, $se)

  param <- list(estimate = c(NA, NA), se = c(NA, NA))
  if (length(dat) >= 1) {
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
    print(paste("Warning: this station has less than ",1," years of data. Use another method!",
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
#' @examples library(FlomKartShinyApp)
#' estimate = gamma_mom(test_data)
#' FlomKartShinyApp::plot4server(test_data, param = estimate$estimate, distr = 2)
gamma_mom <- function(dat) {
# Fit GAMMA distribution with ordinary moment estimator
# Returns param as a list($estimate, $se)

  param <- list(estimate = c(NA, NA), se = c(NA, NA))
  if (length(dat) >= 1) {
    sigma <- moments(dat)[2]
    mu <-  moments(dat)[1]
    param$estimate <- c()
    param$estimate[2] <- mu / sigma^2
    param$estimate[1] <- mu^2 / sigma^2
    # Standard error is not yet implemented
    invisible(param)
  } else {
    print(paste("Warning: this station has less than ",1," years of data. Use another method!",
                  collapse="",sep=""))
    invisible(param)
  }
}

#' Fitting the gamma distribution with Bayesian inference
#' @description Function to fit the gamma distribution with BayesianMCMC method
#' @param dat the data that needs fitting (i.e. flood data)
#' @return param Estimated parameters and standard error returned as a list($estimate, $se)
#' @export
#'
#' @examples library(FlomKartShinyApp)
#' estimate = gamma_bayes(test_data)
#' FlomKartShinyApp::plot4server(test_data, param = estimate$estimate, distr = 5)
gamma_bayes <- function(dat) {
  # Fit Gamma distribution with the Bayesian method
  
  param <- list(estimate = c(NA, NA), se = c(NA, NA))
  
  if (length(dat) >= 1) {
    # Prior for Bayes
    myprior <- function (x) {
      # x = vector of parameter values: c(location, scale, shape)
      # I assume the shape parameter only has a prior with mean zero and standard deviation 0.2
      dnorm(x[1], 0, 0.0001)
    }
    ptemp<-gamma_Lmom(dat)$estimate
    pstart<-c(0,1/ptemp[2],ptemp[1])    
    fail_safe <- failwith(NULL, BayesianMCMC)
    bayes <- fail_safe(dat, nbpas = 5000, nbchaines = 3, confint = c(0.05, 0.95), dist = "P3", parameters0 = pstart)
    # PB Doesn't work or yields poor results with a prior
    # bayes <- fail_safe(dat, nbpas = 5000, nbchaines = 3, confint = c(0.05, 0.95), dist = "P3",apriori=myprior)
    
    if (is.null(bayes) == TRUE) {
      print("Warning: the function BayesianMCMC failed in pearson_bayes")
      invisible(param)
    } else {
      
      ## Addition to return parameters
      #   # Solution 1
      param$estimate <- bayes$parametersML[c(3,2)]
      param$estimate[2]<-1/param$estimate[2]
      
      #       # Solution 2
      #       param$estimate[1] <- mean(as.vector(bayes$parameters[, 1, 1:3]))
      #       param$estimate[2] <- mean(as.vector(bayes$parameters[, 2, 1:3]))
      #       param$estimate[3] <- mean(as.vector(bayes$parameters[, 3, 1:3]))
      
      param$se[1] <- sd(as.vector(bayes$parameters[, 3, 1:3]))
      param$se[2] <- sd(1/as.vector(bayes$parameters[, 2, 1:3]))
#      param$se[3] <- sd(as.vector(bayes$parameters[, 3, 1:3]))
      
      invisible(param)
    }
  } else {
    print(paste("Warning: this station has less than ", 1," years of data. Use another method!",
                collapse = "", sep = ""))
    invisible(param)
  }
}
