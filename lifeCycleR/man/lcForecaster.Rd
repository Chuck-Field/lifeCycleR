\name{lcForecaster}
\alias{lcForecaster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{lcForecaster - Life Cycle Forecaster
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lcForecaster(curves, productVector, forecastLength, reScale)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{curves}{
%%     ~~Describe \code{curves} here~~
}
  \item{productVector}{
%%     ~~Describe \code{productVector} here~~
}
  \item{forecastLength}{
%%     ~~Describe \code{forecastLength} here~~
}
  \item{reScale}{
%%     ~~Describe \code{reScale} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (curves, productVector, forecastLength, reScale)
{
    if (sum(productVector) != 0) {
        productVectorLength <- length(na.omit(productVector))
        rescale <- function(x) (x - min(x))/(max(x) - min(x))
        bestFitVector <- vector()
        for (i in 1:nrow(curves)) {
            lcFactor <- as.numeric(productVector[productVectorLength])/
              as.numeric(curves[i,
                productVectorLength])
            fulllcForecast <- round(as.numeric(curves[i, ]) *
                lcFactor, 0)
            lcForecast <- fulllcForecast[(productVectorLength +
                1):(productVectorLength + forecastLength)]
            bestFitVector[i] <- rmse(as.numeric(productVector),
                as.numeric(curves[i, 1:productVectorLength]))
        }
        bestCurve <- which.min(bestFitVector)
        lcFactor <- as.numeric(productVector[productVectorLength])/
          as.numeric(curves[bestCurve,
            productVectorLength])
        lcFactor <- ifelse(is.infinite(lcFactor), 0, lcFactor)
        lcFactor <- ifelse(is.nan(lcFactor), 0, lcFactor)
        fulllcForecast <- round(as.numeric(curves[bestCurve,
            ]) * lcFactor, 0)
        lcForecast <- fulllcForecast[(productVectorLength + 1):
          (productVectorLength + forecastLength)]
    }
    else {
        lcForecast <- rep(0, forecastLength)
        fulllcForecast <- as.numeric(curves[1, ]) * 0
        bestCurve <- 0
        curveShape = rep(0, length(curves[1, ]))
    }
    results <- list(forecast = lcForecast, forecastFit = fulllcForecast,
        bestCurve = bestCurve, curveShape = curves[bestCurve,
            ])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
