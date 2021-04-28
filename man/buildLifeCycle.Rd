\name{buildLifeCycle}
\alias{buildLifeCycle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{buildLifeCycle - Build Life Cycle Curve Set
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
buildLifeCycle(dataframe, curveNumber)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
  \code{} A data frame comprised of one product/service per row, where
  each row contains the customer/widget count of x time periods.
}
  \item{curveNumber}{
  \code{} An integer that represents a desired number of curve models
  created from the data frame of products.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
 The returned value will be a data frame comprised of a life cycle shape on each
 row. The number of rows should be equal to the desired number of models
 (curveNumber) requested in the function.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Charles Field
}
\note{


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
