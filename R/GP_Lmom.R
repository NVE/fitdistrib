#' Title
#'
#' @param dat
#' @param threshold
#'
#' @return
#' @export
#'
#' @examples
gp_Lmom <- function(dat, threshold = NA) {

  param <- list(estimate = c(NA, NA, NA), se = c(NA, NA, NA))
  if (length(dat) >= 1) {
    if (is.na(threshold)){


      dat.Lmom <- Lmoments(dat)

      fail_safe <- failwith(NULL, par.genpar)
      fitted.param <- fail_safe(dat.Lmom[1], dat.Lmom[2], dat.Lmom[4])

      if (is.null(fitted.param) == TRUE) {
        print("Warning: the function par.genpar failed in gp_Lmom")
        invisible(param)
      } else {
        # Creating the returning list
        param$estimate <- c(fitted.param$xi, fitted.param$alfa, fitted.param$k)
        # Standard error is not yet implemented
        invisible(param)
      }
    }else {
      #put as a failsafe, the parameter estimation above sometimes gave location parameter lower than threshold
      dat2<-dat-threshold
      dat.Lmom <- Lmoments(dat2)
      t2 = dat.Lmom[2] / dat.Lmom[1]
      param$estimate[1] <- threshold
      param$estimate[3] <- 2-1/t2
      param$estimate[2] <- dat.Lmom[1]*(1/t2-1)
      invisible(param)
    }
  }

  else {
    print(paste("Warning: this station has less than ", 1," years of data. Use another method!",
                collapse = "", sep = ""))
    invisible(param)
  }
}
