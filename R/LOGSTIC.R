# LOGIOSTIC.R
# All function related to fitting the Logistics distribution

#' Fitting the Logistic distribution with MLE
#'
#' @param xdat
#' @param ydat
#' @param mul
#' @param sigl
#' @param mulink
#' @param siglink
#' @param shlink
#' @param muinit
#' @param siginit
#' @param show
#' @param method
#' @param maxit
#' @param ...
#'
#' @description Function to fit the Logistic distribution with the maximum likelihood method.
#' This function was copied from Kolbjorn's initial file
#' @return param Estimated parameters and standard error returned as a list($estimate, $se)
#' Standard error is not yet implemented
#' @importFrom plyr failwith
#' @importFrom stats optim
#' @export
#'
#' @examples library(FlomKartShinyApp)
#' estimate = logist_mle(test_data)
#' FlomKartShinyApp::plot4server(test_data, param = estimate$estimate, distr = 4)
logist_mle <- function (xdat, ydat = NULL, mul = NULL, sigl = NULL,
                    mulink = identity, siglink = identity,
                    muinit = NULL, siginit = NULL, show = FALSE,
                    method = "Nelder-Mead", maxit = 10000, ...) {

#  show="FALSE"  # HACK FLO. Let's make sure we dont print anything
  z <- list(estimate = c(NA, NA), se = c(NA, NA))  # HACK FLO

  if (length(xdat) >= 1) {
    # z <- list()
    npmu <- length(mul) + 1
    npsc <- length(sigl) + 1
    z$trans <- FALSE
    dat.Lmom <- Lmoments(xdat)
    par.init<-c(dat.Lmom[1],dat.Lmom[2])
    in2 <- par.init[2]
    in1 <- par.init[1]
    if (is.null(mul)) {
      mumat <- as.matrix(rep(1, length(xdat)))

      if (is.null(muinit))
        muinit <- in1
    } else {
      z$trans <- TRUE
      mumat <- cbind(rep(1, length(xdat)), ydat[, mul])

      if (is.null(muinit))
        muinit <- c(in1, rep(0, length(mul)))
    }
    if (is.null(sigl)) {
      sigmat <- as.matrix(rep(1, length(xdat)))

      if (is.null(siginit))
        siginit <- in2
    } else {
      z$trans <- TRUE
      sigmat <- cbind(rep(1, length(xdat)), ydat[, sigl])

      if (is.null(siginit))
        siginit <- c(in2, rep(0, length(sigl)))
    }


    z$model <- list(mul, sigl)
    z$link <- deparse(substitute(c(mulink, siglink)))
    init <- c(muinit, siginit)
    gl.lik <- function(a) {
	  options(warn=-1)
      mu <- mulink(mumat %*% (a[1:npmu]))
      sc <- siglink(sigmat %*% (a[seq(npmu + 1, length = npsc)]))
  
      y <- (xdat - mu) / sc

	  options(warn=0)
      if (any(is.na(y)) || any(sc <= 0))
        return(10^6)
      sum(log(sc)) - sum(y)+2*sum(log(1+exp(y)))
    }

    #   x <- optim(init, gl.lik, hessian = TRUE, method = method,  # Original code commented by FKB
    #       control = list(maxit = maxit, ...))

    fail_safe <- failwith(NULL, optim)  # START FKB HACK
    x <- fail_safe(init, gl.lik, hessian = TRUE, method = method, control = list(maxit = maxit, ...))

    if (is.null(x) == TRUE) {
      print("Warning: The function optim failed in gl_mle")
      invisible(param)
    } else {
      ## End hack FKB


      z$conv <- x$convergence
      mu <- mulink(mumat %*% (x$par[1:npmu]))
      sc <- siglink(sigmat %*% (x$par[seq(npmu + 1, length = npsc)]))
      z$nllh <- x$value
      z$dat <- xdat
      if (z$trans) {
        z$dat <- -log(as.vector(((xdat - mu))/sc))
      }
      # z$mle replaced by z$estimate to integrate with the other functions
      z$estimate <- x$par



      # z$cov <- solve(x$hessian)  # initially this way
       z$cov <- tryCatch(solve(x$hessian), finally = "Warning: could not solve Hessian")  # Protection FLO
      # z$cov <- try(solve(x$hessian))  # TO FIX!!!!!!!!!!!!!!!!
      # z$se <- sqrt(diag(z$cov))  # initially this way
       z$se <- try(sqrt(diag(z$cov)))  # TO FIX!!!!!!!!!!!!!!!!


      z$vals <- cbind(mu, sc)
      if (show) {
        if (z$trans)
          print(z[c(2, 3)])
        else print(z[4])
        if (!z$conv)
          print(z[c(5, 7, 9)])
      }
      # class(z) <- "gev.fit"  # Commented by FLO
      invisible(z)
    }
  }
  else {
    print(paste("Warning: this station has less than ",1," years of data. Use another method!",
                collapse = "",sep = ""))
    invisible(param)
  }
}


#' Fitting the Logistic distribution with Lmom
#' @description Function to fit the Logistics distribution with the linear moments method
#' @param dat the data that needs fitting (i.e. flood data)
#' @return param Estimated parameters and standard error returned as a list($estimate, $se)
#' @export
#'
#' @examples library(FlomKartShinyApp)
#' estimate = logist_Lmom(test_data)
#' FlomKartShinyApp::plot4server(test_data, param = estimate$estimate, distr = 4)
logist_Lmom <- function(dat) {
  param <- list(estimate = c(NA, NA), se = c(NA, NA))
  if (length(dat) >= 1) {
    dat.Lmom <- Lmoments(dat)
    # ADD FAILSAFE?
    # Creating the returning list. Standard error is not yet implemented
    param$estimate <- c(dat.Lmom[1], dat.Lmom[2])
    return(param)
    } else {
      print(paste("Warning: This station has less than ", 1," years of data. Use another method!",
                  collapse = "",sep = ""))
      invisible(param)
    }
}

#' Fitting the Logistic distribution with mom
#' @description Function to fit the Logistics distribution with the ordinary moments method
#' @param dat the data that needs fitting (i.e. flood data)
#' @return param Estimated parameters and standard error returned as a list($estimate, $se).
#' Standard error is not yet implemented
#' @export
#'
#' @examples library(FlomKartShinyApp)
#' estimate = logistic_mom(test_data)
#' FlomKartShinyApp::plot4server(test_data, param = estimate$estimate, distr = 4)
logist_mom <- function(dat) {

  param <- list(estimate = c(NA, NA, NA), se = c(NA, NA, NA))  # HACK FLO
  if (length(dat) >= 1) {
    smom <-  moments(dat)

  # Creating the returning list. Standard error is not yet implemented
  param$estimate <- c(smom[1], smom[2]/(pi*sqrt(1/3)))
  return(param)
  } else {
    print(paste("Warning: this station has less than ",1," years of data. Use another method!",
                  collapse="",sep=""))
    invisible(param)
    }
}

#' Fitting the Logistic distribution with Bayesian inference
#' @description Function to fit the Logistics distribution with BayesianMCMC method
#' We assume that the shape parameter only has a prior with mean zero and standard deviation 0.2 (dnorm(x[3], 0, 0.2))
#' @param dat the data that needs fitting (i.e. flood data)
#' @return param Estimated parameters and standard error returned as a list($estimate, $se)
#' @importFrom plyr failwith
#' @importFrom stats dnorm
#' @importFrom stats sd
#' @export
#'
#' @examples library(FlomKartShinyApp)
#' estimate = logistic_bayes(test_data)
#' FlomKartShinyApp::plot4server(test_data, param = estimate$estimate, distr = 4)
logist_bayes <- function(dat) {
# Fit GL distribution with the Bayesian method
  param <- list(estimate = c(NA, NA), se = c(NA, NA))

  if (length(dat) >= 1) {
    # Prior for Bayes
    myprior <- function (x) {
      # x = vector of parameter values: c(location, scale, shape)
      # I assume the shape parameter only has a prior with mean zero and standard deviation 0.2
      dnorm(x[3], 0, 0.0001)
    }
    pstart<-c(Lmoments(dat)[c(1,2)],0)
    fail_safe <- failwith(NULL, BayesianMCMC)
    bayes <- fail_safe(dat, nbpas = 5000, nbchaines = 3,
                       confint = c(0.05, 0.95), dist = "GENLOGIS", apriori = myprior, parameters0 = pstart)

    if (is.null(bayes) == TRUE) {
      print("Warning: the function BayesianMCMC failed in gl_bayes")
      invisible(param)
    } else {

      ## Addition to return parameters
      #   # Solution 1
      param$estimate <- bayes$parametersML[1:2]

#       # Solution 2
#       param$estimate[1] <- mean(as.vector(bayes$parameters[, 1, 1:3]))
#       param$estimate[2] <- mean(as.vector(bayes$parameters[, 2, 1:3]))
#       param$estimate[3] <- mean(as.vector(bayes$parameters[, 3, 1:3]))

      param$se[1] <- sd(as.vector(bayes$parameters[, 1, 1:3]))
      param$se[2] <- sd(as.vector(bayes$parameters[, 2, 1:3]))

      invisible(param)
    }
  } else {
    print(paste("Warning: this station has less than ", 1," years of data. Use another method!",
                collapse = "", sep = ""))
    invisible(param)
  }
}

#' Calculating the posterior predictive distribution for Logistic
#' @description Function to calculate the posterior predictive distribution after calling gev_bayes
#' @param (mmrp, mupars, spars, kpars) parameters returned by gev_bayes. mupars, spars, kpars are the ensemble of param$estimate
#' @return param Estimated parameters and standard error returned as a list($estimate, $se)
#' @importFrom stats quantile
#' @export
#'
#' @examples
get_posterior_logist <- function(mmrp, mupars, spars) {

  qqsample1 <- sapply(seq(length(mupars)), function(st) {
    mean_temp <- mupars[st]
    st_temp <-spars[st]
    qlogis(F = (1 - 1 / mmrp), mean_temp, st_temp)
    }, simplify = "array")
  # 1 is for collums only 0.5 to return the median
  qqr <- apply(qqsample1, 1, quantile, 0.5)
}
