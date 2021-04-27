
#' buildLifeCycle - Build Life Cycle Curve Set
#'
#' This function takes in a data frame of curve shapes built by the
#' buildTrapezoid or the buildPoly function and the number of life cycle
#' curves that need to be created. It's recommended that the
#' BuildTrapezoid function is used to create fitted shapes, but raw
#' product data can be used, as long as no NA values exist.
#'
#' @param dataframe A set of pregenerated life cycle curves.
#' @param curveNumber The desired number of curve models
#' @return A data frame of Curve Models where each row represents an average
#' life cycle shape.
#' @importFrom stats kmeans
#' @export

buildLifeCycle <- function(dataframe, curveNumber) {
  if (any(is.na(dataframe))) {
    print('Warning: Dataframe contains at least one NA value. All NA values will be converted to 0 (zero).')
    dataframe[is.na(dataframe)] <- 0
  }
  # cluster data frame
  km <- kmeans(dataframe, centers = curveNumber, nstart = 10)
  clusters <- cbind(dataframe, km$`cluster`)
  colnames(clusters)[ncol(clusters)] <- 'Cluster'
  # create curves from cluster averages and rescale to peak of 1
  rescale <- function(x) (x-min(x))/(max(x) - min(x))
  curveSet <- data.frame()
  for(i in 1:curveNumber) {
    cluster <- clusters[clusters$Cluster == i,]
    clusterAverages <- colMeans(cluster[,1:ncol(clusters)-1])
    clusterAverages <- rescale(clusterAverages)
    curveSet <- rbind(curveSet,clusterAverages)
  }
  colnames(curveSet) <- NULL
  return(curveSet)
}
