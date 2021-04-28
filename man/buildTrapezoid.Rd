\name{buildTrapezoid}
\alias{buildTrapezoid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{buildTrapezoid - Build Trapezoid from Vector
%%  ~~function to do ... ~~
}
\description{
 This function takes in a numeric vector representing a product
 and a length variable that defines the desired vector output size and
 creates a trapezoidal shape that best fits the productVector the function
 was supplied.
}
\usage{
buildTrapezoid(productVector, trapMaxLength)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{productVector}{
  A numeric vector at x length, where each value reresents product adoption over
  time.
}
  \item{trapMaxLength}{
  A integer that represents the length of the trapezoidal fit, over the same
  time units, used in the productVector.
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
  Charles Field
}
\note{
 When supplying a data frame or group of vectors of product history counts,
 each product vector should have the same unit of measure. Days, Weeks, Months,
 etc. When supplying age mismatched products, each should be aligned to the same
 start and be filled with trailing NAs, so all vectors have the same length.
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
