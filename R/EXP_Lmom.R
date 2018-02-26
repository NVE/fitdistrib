#' Title
#'
#' @param dat
#' @param threshold
#'
#' @return
#' @export
#'
#' @examples
exp_Lmom <- function(dat, threshold = NA){

  param <- list(estimate = c(NA, NA), se = c(NA, NA))
  if(length(dat) >= 1){
    if(is.na(threshold)){
      dat.mom <- Lmoments(dat)
      param$estimate <- invisible(as.numeric(par.exp(dat.mom[1], dat.mom[2])))
      invisible(param)
    } else{
      dat2 <- dat-threshold
      dat.Lmom <- Lmoments(dat2)
      param$estimate[1] <- threshold
      param$estimate[2] <- 2*dat.Lmom[2]
      invisible(param)
    }
  }
  else {
    print(paste("Warning:this station has les than ",1,"years of data, use another method", collapse = "", sep = ""))
  }
}
