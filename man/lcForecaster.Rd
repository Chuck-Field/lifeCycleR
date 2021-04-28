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
  \code{} A data frame of curve models created by the buildLifeCycle()
}
  \item{productVector}{
  \code{} A vector of the current product history being forecasted
}
  \item{forecastLength}{
  \code{} An integer of the desired length of the forecast
}
  \item{reScale}{
  \code{} NA - Placeholder for future enhancement
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
 A list is returned where $forecast provides the forecast defined by the
 requested length. $forecastFit provides the entire forecast by the model
 length. $bestCurve provides the curve ID of the best curve from the curves
 data frame and $curveShape provides the vector of the model used.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Charles Field
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Step 1:

  # Produces a vector in the shape of a trapezoid which fits the product
  # history and extends to the length requested (trapMaxLength)
  # A Trapezoid fit would be created for each product available and
  # the best products to use, should already be showing decline.

  ProductHistoryDF <- buildTrapezoid(productVector, 120)

Step 2:

  # Takes the data frame from step one and creates the number of generic
  # life cycle models specified. It's recommended that at least 100 sample
  # shapes be provided in the data frame for each curve model desired.

  curveModelsDF <- buildLifeCycle(ProductHistoryDF, 5)

Step 3:

  # Produces a forecast for the "NewProductVector" at the desired units.
  # Products with little history, less than 12 time unites as an example,
  # may not get the benefit of receiving the best curve model for that
  # product.

  lcForecaster <- (curveModelsDF, NEWproductVector, 72)

}

