\name{seXy}
\alias{seXy}
\alias{seX}
\alias{seXyIqr}
\alias{seXyMad}
\alias{seXy95}
\title{
Functions to Compute Measures of Central Tendency and Spread.  seX!
}
\description{
These functions compute various measures of central tendency and spread.  These functions return a vector containing the measure of central tendency, as well as that measure +/- the requested spread.  \code{seX} is a little different from the others in that it simply returns the standard error of x, hence \code{seX}.  Haven't we always needed a function for \code{seX}?
}
\usage{
seXy(x)
seXy95(x)
seXyMad(x)
seXyIqr(x)
seX(x)
}
\arguments{
  \item{x}{
A vector of numeric values whose measure of central tendency and spread are to be computed.
}
}
\details{
These functions include \code{na.omit}.  \code{seXy} returns the mean =\- the standard error.  \code{seXy95} returns the mean =\- the 95% confidence interval of the standard error.  \code{seXyMad} returns the median =\- median absolute deviation.  \code{seXyIqr} returns the median plus the first and third quartiles.
}
\value{
For all but \code{seX}, a vector of 3 numeric values, giving the measure of central tendency, that measure + the spread, and that measure - the spread.  So for example, \code{sd} gives the mean +/- the standard deviation.  For \code{seX}, a single value giving the standard error of x.
}

\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
x <- rnorm(100)
seX(x)
seXy(x)
seXy95(x)
seXyMad(x)
seXyIqr(x)
}

\keyword{ utilities }
