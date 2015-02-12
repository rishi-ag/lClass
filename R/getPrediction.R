
#----------------------------------------------------------------------
# getPrediction
# ----------------------------------------------------------------------
#' Classifies data points to ine of three animal classes
#'
#' Function takes input of points and classifies them into one of three classes:
#' Cats, Dogs and Maggots.
#' 
#' @param animalsDF A data frame of simulated points representing three classes 
#' along with their classes and dummy categorical variables corresponding to each class.
#' @return List of predicted labels along with regression information of the three animal classes
#' @export
#' @import assertthat
#' @examples
#' noAnimals <- c(200, 200, 200)
#' rho  <- c(-0.2, 0.8, 0.02)
#' sdXY <- list(c(2, 8), c(3.5, 12), c(1, 3))
#' muXY <- list(c(6, 30), c(12, 50), c(4,10))
#' animalsDF <- animals(rho, sdXY, muXY, noAnimals)
#' predictionList <- getPrediction(animalsDF)

getPrediction <- function(animalsDF){
  areYouACat <- lm(labCats ~ weight + height, data = animalsDF)
  areYouADog <- lm(labDogs ~ weight + height, data = animalsDF)
  areYouAMaggot <- lm(labMaggots ~ weight + height, data = animalsDF)
  
  predict <- cbind(predict(areYouACat), predict(areYouADog), predict(areYouAMaggot))
  predictLabel <- apply(predict, 1, numToChar)
  return(list(predictLabel, areYouACat, areYouADog, areYouAMaggot))
}
