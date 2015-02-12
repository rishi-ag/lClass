#----------------------------------------------------------------------
# numToChar
# ----------------------------------------------------------------------
2#' Classify k-nearest neighbour vector
#'  
#' A numerical vector of probabilities of a point belonging to a class of cats 
#' dogs or maggots. The output is the character label of the class with the highest
#' probability  
#' 
#' @param classVector A vector of numerical class predictions for new data points.
#' @return Vector of character labels of classes
#' @export
#' @import assertthat
#' @examples
#' num.class <- c(.73, 0.5, 0.98)
#' numToChar(num.class)

numToChar <- function(classVector) {
  
  assert_that(is.number(classVector[1]), is.number(classVector[2]), is.number(classVector[3]))
  
  if(which.max(classVector) ==1) {return ("Cats")}
  else if (which.max(classVector) == 2){ return ("Dogs")} 
  else {return ("Maggots")}
  }

