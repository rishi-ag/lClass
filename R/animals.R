
#----------------------------------------------------------------------
# Animals
# ----------------------------------------------------------------------
#' #Create a dataset for classification
#'
#' @param rho A vector of 3 numbers belonging to \code{[-1,1]} which indicate the
#' correlation between two features of the three classes to simulate.
#' @param sdXY  A list of 3 vectors which indicate the standard deviation of features
#' of a class.
#' @param muXY A list of 3 vectors which indicate the mean of features
#' of a class.
#' @param noSim A vector of 3 numbers of no of simulations desired for each class
#' @param seed Specifies seed. Default Value = 1234
#' @return Data Frame of simulations with their classes and coded dummy variables
#' @export
#' @import assertthat
#' @examples
#' noAnimals <- c(200, 200, 200)
#' rho  <- c(-0.2, 0.8, 0.02)
#' sdXY <- list(c(2, 8), c(3.5, 12), c(1, 3))
#' muXY <- list(c(6, 30), c(12, 50), c(4,10))
#' animalsDF <- animals(rho, sdXY, muXY, noAnimals)


#Create a dataset for classification
animals <- function(rho, sdXY, muXY, noSim, seed = 5341) {
  #validate inputs
  assert_that(is.number(rho[1]), is.number(rho[2]), is.number(rho[2]))
  assert_that(rho[1] >= -1, rho[1] <= 1, rho[2] >= -1 , rho[2] <= 1, 
              rho[3] >= -1 , rho[3] <= 1)
  assert_that(is.number(sdXY[[1]][1]), is.number(sdXY[[2]][1]), 
              is.number(sdXY[[3]][1]))
  assert_that(is.number(muXY[[1]][1]), is.number(muXY[[2]][1]), 
              is.number(muXY[[3]][1]))
  assert_that(is.number(noSim[1]), is.number(noSim[2]), is.number(noSim[3])
              ,is.number(seed))
  
  catsVC     <- sigmaXY(rho[1], sdXY[[1]][1], sdXY[[1]][2])
  dogsVC     <- sigmaXY(rho[2], sdXY[[2]][1], sdXY[[2]][2])
  maggotsVC    <- sigmaXY(rho[3], sdXY[[3]][1], sdXY[[3]][2])
  
  catsBVN    <- genBVN(noSim[1], seed = 5341, muXY[[1]], catsVC)
  dogsBVN    <- genBVN(noSim[2], seed = 5342, muXY[[2]], dogsVC)
  maggotsBVN <- genBVN(noSim[3], seed = 5343, muXY[[3]], maggotsVC)
  
  animalsDf <- as.data.frame(rbind(catsBVN,dogsBVN, maggotsBVN))
  Animal <- c(rep("Cats", noSim[1]), rep("Dogs", noSim[2]), rep("Maggots", noSim[3]))
  animalsDf <- cbind(animalsDf, Animal)
  colnames(animalsDf) <- c("weight", "height", "Animal")
  
  #Provide 1 0 labels to relevant class
  animalsDf <- cbind(animalsDf, labCats = c(rep(1, noAnimals[1]),
                                            rep(0, noAnimals[2]), 
                                            rep(0, noAnimals[3])))
  animalsDf <- cbind(animalsDf, labDogs = c(rep(0, noAnimals[1]),
                                            rep(1, noAnimals[2]),
                                            rep(0, noAnimals[3])))
  animalsDf <- cbind(animalsDf, labMaggots = c(rep(0, noAnimals[1]),
                                               rep(0, noAnimals[2]),
                                               rep(1, noAnimals[3])))
  
  return(animalsDf)
}

