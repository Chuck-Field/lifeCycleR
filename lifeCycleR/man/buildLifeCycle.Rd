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
%%     ~~Describe \code{dataframe} here~~
}
  \item{curveNumber}{
%%     ~~Describe \code{curveNumber} here~~
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
function (dataframe, curveNumber)
{
    if (any(is.na(dataframe))) {
        print("Warning: Dataframe contains at least one NA value. All NA
        values will be converted to 0 (zero).")
        dataframe[is.na(dataframe)] <- 0
    }
    km <- kmeans(dataframe, centers = curveNumber, nstart = 10)
    clusters <- cbind(dataframe, km$cluster)
    colnames(clusters)[ncol(clusters)] <- "Cluster"
    rescale <- function(x) (x - min(x))/(max(x) - min(x))
    curveSet <- data.frame()
    for (i in 1:curveNumber) {
        cluster <- clusters[clusters$Cluster == i, ]
        clusterAverages <- colMeans(cluster[, 1:ncol(clusters) -
            1])
        clusterAverages <- rescale(clusterAverages)
        curveSet <- rbind(curveSet, clusterAverages)
    }
    colnames(curveSet) <- NULL
    return(curveSet)
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
