
#----------------------------------------------------------------------
# #sigmaXY
# ----------------------------------------------------------------------
#' Creates a Variance Covariance Matrix 
#'
#' Given correlations and standard deviations of both features the function returns
#' a variance covariance matrix
#' 
#' @param rho A number belonging to \code{[-1,1]} which indicate the
#' correlation between two features of the three classes to simulate.
#' @param sdX Standard deviaton of first feature
#' @param sdY Standard deviaton of first feature
#' @return Variance Covariance matrix
#' @export
#' @import assertthat 
#' @examples
#' rho  <- -0.2
#' sdX <- 2
#' sdY <- 10
#' sigmaXY(rho, sdX, sdY)

#Create variance covariance matrix
sigmaXY <- function(rho, sdX, sdY) {
  assert_that(is.number(rho), is.number(sdX), is.number(sdY))
  assert_that(rho >= -1, rho <= 1)
  
  covTerm <- rho * sdX * sdY
  VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 
                     2, 2, byrow = TRUE)
  return(VCmatrix)
}