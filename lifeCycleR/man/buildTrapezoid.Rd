\name{buildTrapezoid}
\alias{buildTrapezoid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{buildTrapezoid - Build Trapezoid from Vector
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
buildTrapezoid(productVector, trapMaxLength)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{productVector}{
%%     ~~Describe \code{productVector} here~~
}
  \item{trapMaxLength}{
%%     ~~Describe \code{trapMaxLength} here~~
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
function (productVector, trapMaxLength)
{
    rescale <- function(x) {
        if (min(x) != max(x)) {
            x <- (x - min(x))/(max(x) - min(x))
        }
        else {
            x <- rep(1, length(x))
        }
        return(x)
    }
    productVector <- productVector[min(which(productVector !=
        0)):max(which(productVector != 0))]
    productVector <- rescale(productVector)
    maxVector <- which(productVector == 1)
    pointTwo <- ifelse(min(maxVector) == 1, 0, min(maxVector))
    pointThree <- max(maxVector)
    if (min(maxVector) == 1) {
        ascendVector <- NULL
    }
    else {
        ascendVector <- seq(0, 1, by = 1/pointTwo)
        cap <- length(ascendVector) - 1
        ascendVector <- ascendVector[2:cap]
    }
    platueVectorLength <- max(maxVector) - (min(maxVector))
    platueVectorLength <- ifelse(platueVectorLength == 0, 1,
        platueVectorLength + 1)
    platueVector <- rep(1, platueVectorLength)
    if (pointThree == length(productVector)) {
        descendVector <- rep(1, (trapMaxLength) - length(productVector))
    }
    else {
        descendVector <- productVector[pointThree:length(productVector)]
        lastPoint <- descendVector[length(descendVector)]
        descendFactor <- (1 - lastPoint)/length(descendVector)
        descendVector <- rev(seq(0, 1, by = descendFactor))
        descendVector <- descendVector[2:length(descendVector)]
    }
    trapVector <- c(ascendVector, platueVector, descendVector)
    trapVector <- trapVector[1:trapMaxLength]
    fillNAVectorlength <- trapMaxLength - length(trapVector)
    fillNAVector <- rep(NA, fillNAVectorlength)
    trapVector <- c(trapVector, fillNAVector)
    fillNAVectorlength <- trapMaxLength - length(productVector)
    fillNAVector <- rep(NA, fillNAVectorlength)
    productVector <- c(productVector, fillNAVector)
    results <- list(trapezoid = trapVector, scaledProductVector = productVector)
    return(results)
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
