#' Te quantile function of the SinInverseWeibull probability distribution.
#' @export
#'
#' @param p vector of probabilities.
#' @param alpha Alpha parameter.
#' @param theta Theta parameter.
#' @param lower Lower parameter.
#' @param log.p Log.p parameter.
#' @return A vector with n observations of the SinInverseWeibull distribution.
#' @examples
#' qsiniw(0.5,1,1,TRUE,FALSE)
#' qsiniw(0.5,1,0.1,TRUE,FALSE)

qsiniw<-function(p,alpha,theta,lower = TRUE,log.p = FALSE){
  if (log.p == TRUE) {
    if (lower == TRUE){
      log((-log((2/pi)*asin(p))/alpha)^(-1/theta))
    }else{
      log((-log((2/pi)*asin(1-p))/alpha)^(-1/theta))
    }
  } else {
    if (lower == TRUE){
      (-log((2/pi)*asin(p))/alpha)^(-1/theta)
    }else{
      (-log((2/pi)*asin(1-p))/alpha)^(-1/theta)
    }
  }

}
