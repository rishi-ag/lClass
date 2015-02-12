#----------------------------------------------------------------------
# genBVN
# ----------------------------------------------------------------------
#' Creates realizations of a normal Bi-variate random variable
#'
#' 
#' @param n Number of realizations.
#' @param seed Set seed.
#' @param muXY Vector of two numbers that represent means of features.
#' @param sigmaXY Variance Covariance Matrix
#' @return Realizations of n bivariate normal random variable with means muXY and 
#' variance covariance sigmaXY.
#' @export
#' @import assertthat
#' @import mvtnorm
#' @examples
#' rho  <- -0.2
#' sdX <- 2
#' sdY <- 8
#' muXY <- c(0,1)
#' sigmaXY <- sigmaXY(rho, sdX, sdY)
#' genBVN (n = 1, seed = 1234, muXY, sigmaXY)

genBVN <- function(n = 1, seed = 1234, muXY=c(0,1), sigmaXY=diag(2)) {
  assert_that(is.number(n), is.number(seed), is.number(muXY[1]), is.number(muXY[2]))
  assert_that(nrow(sigmaXY) == 2, ncol(sigmaXY) == 2)
  
  if(!is.na(seed)) set.seed(seed)
  rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
  return(rdraws)
}
