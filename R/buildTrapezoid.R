
#' buildTrapezoid - Build Trapezoid from Vector
#'
#' This function takes in a numeric vector representing a product
#' and a length variable that defines the desired vector output size and
#' creates a trapezoidal shape that best fits the productVector the function
#' was supplied.
#'
#' @param productVector A numeric vector representing a product
#' @param trapMaxLength A length variable that defines the desired vector
#' output size
#' @return A trapezoid as a vector of numbers which may or may not produce a
#' trepezoidal shape that represents the life cycle fit to the productVector.
#' @return scaledProductVector as a vector of numbers which were scaled between
#' 0 and 1, from the productVector input.
#' @export


buildTrapezoid <- function(productVector, trapMaxLength) {
  # rescale product vector
  rescale <- function(x) {
    if (min(x) != max(x)) {
      x <- (x-min(x))/(max(x) - min(x))
    } else {
      x <- rep(1, length(x))
    }
    return(x)
  }
  # remove leading and trailing zeros
  productVector <- productVector[min(which(productVector != 0 )) :
                                   max(which(productVector != 0 ))]
  productVector <- rescale(productVector)
  # find first and last high point in product vector
  maxVector <- which(productVector  == 1)
  pointTwo <- ifelse(min(maxVector) == 1, 0, min(maxVector))
  pointThree <- max(maxVector)
  # develop ascending line of trapezoid
  if (min(maxVector) == 1) {
    ascendVector <- NULL
  } else {
    ascendVector <- seq(0,1, by=1/pointTwo)
    cap <- length(ascendVector)-1
    ascendVector <- ascendVector[2:cap]
  }
  # develop platue line of trapezoid
  platueVectorLength <- max(maxVector) - (min(maxVector))
  platueVectorLength <- ifelse(platueVectorLength == 0, 1,
                               platueVectorLength + 1)
  platueVector <- rep(1, platueVectorLength)
  # develop descending line of trapezoid
  if (pointThree == length(productVector)) {
    descendVector <- rep(0, (trapMaxLength) - length(productVector))
  } else {
    descendVector <- productVector[pointThree:length(productVector)]
    lastPoint <- descendVector[length(descendVector)]
    descendFactor <- (1-lastPoint)/length(descendVector)
    descendVector <- rev(seq(0,1, by = descendFactor))
    descendVector <- descendVector[2:length(descendVector)]
  }
  # combine lines into one vector to make trapezoid
  trapVector <- c(ascendVector, platueVector, descendVector)
  # if trapezoid exceeds length requirement, trim to fit
  trapVector <- trapVector[1:trapMaxLength]
  # if trapezoid does not meet length requirement, fill with NA
  fillNAVectorlength <- trapMaxLength - length(trapVector)
  fillNAVector <- rep(NA, fillNAVectorlength)
  trapVector <- c(trapVector, fillNAVector)
  # if product vector does not meet length requirement, fill with NA
  fillNAVectorlength <- trapMaxLength - length(productVector)
  fillNAVector <- rep(NA, fillNAVectorlength)
  productVector <- c(productVector, fillNAVector)
  results <- list(trapezoid = trapVector, scaledProductVector = productVector)
  return(results)
}
