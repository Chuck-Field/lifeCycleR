

#' lcForecaster - Life Cycle Forecaster
#'
#' This function takes in a set of pregenerated life cycle curves, from the buildLifeCycle
#' function, a product vector of 'actual volumes' and the desired forecast length.
#' The output will provide a list of items $forecast provides the exact
#' forecast for the function parameters provided. The $forecastFit provides the
#' entire life cycle fit. The #bestCurve provides the curve number row from the curves
#' data frame entered into the function. And $curveShape gives the curve shape used.
#'
#' @param curves A set of pregenerated life cycle curves
#' @param productVector A vector of time series history on the product being forecasted
#' @param forecastLength The number of units forecasting, based on the productVector time period
#' @param reScale default is TRUE and is a placeholder for upcoming feature
#' @return List as forecast, forecastFit, bestCurve, curveShape
#' @importFrom Metrics rmse
#' @importFrom stats na.omit
#' @export

lcForecaster <- function(curves, productVector, forecastLength, reScale) {

  if (sum(productVector) != 0) {
    productVectorLength <- length(na.omit(productVector))
    rescale <- function(x) (x-min(x))/(max(x) - min(x))
    bestFitVector <- vector()
    for(i in 1:nrow(curves)) {
      lcFactor <- as.numeric(productVector[productVectorLength])/as.numeric(curves[i,productVectorLength])
      fulllcForecast <- round(as.numeric(curves[i,]) * lcFactor, 0)
      lcForecast <- fulllcForecast[(productVectorLength+1):(productVectorLength+forecastLength)]
      bestFitVector[i] <- rmse(as.numeric(productVector), as.numeric(curves[i,1:productVectorLength]))
    }
    bestCurve <- which.min(bestFitVector)
    lcFactor <- as.numeric(productVector[productVectorLength])/as.numeric(curves[bestCurve,productVectorLength])
    lcFactor <- ifelse(is.infinite(lcFactor),0,lcFactor)
    lcFactor <- ifelse(is.nan(lcFactor),0,lcFactor)
    fulllcForecast <- round(as.numeric(curves[bestCurve,]) * lcFactor, 0)
    lcForecast <- fulllcForecast[(productVectorLength+1):(productVectorLength+forecastLength)]

  } else {
    lcForecast <- rep(0, forecastLength)
    fulllcForecast <- as.numeric(curves[1,]) * 0
    bestCurve <- 0
    curveShape = rep(0, length(curves[1,]))
  }
  results <- list(forecast = lcForecast, forecastFit = fulllcForecast, bestCurve = bestCurve, curveShape = curves[bestCurve,])
}
