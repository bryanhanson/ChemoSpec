\name{plotScoresCor}
\Rdversion{1.1}
\alias{plotScoresCor}
\title{
Compute Confidence Ellipses
}

\description{
A utility function which when given a x,y data set computes both classical and robust confidence ellipses. An internal function, not generally called by the user.
}

\usage{
plotScoresCor(x, quan = 1/2, alpha = 0.025)
}
\arguments{
  \item{x}{
As per \code{\link[mvoutlier]{cor.plot}}.
}
  \item{quan}{
As per \code{\link[mvoutlier]{cor.plot}}.
}
  \item{alpha}{
As per \code{\link[mvoutlier]{cor.plot}}.
}
}
\value{
A list with the following elements (a simpler version of that in the original function \code{\link[mvoutlier]{cor.plot}}):
\item{x.cls}{The x values for the classical ellipse.}
\item{y.cls}{The y values for the classical ellipse.}
\item{c}{The correlation value for the classical ellipse.}
\item{x.rob}{The x values for the robust ellipse.}
\item{y.rob}{The y values for the robust ellipse.}
\item{r}{The correlation value for the robust ellipse.}
}


\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
See function \code{\link[mvoutlier]{cor.plot}} in package \code{mvoutlier} on which this function is based.
}

\keyword{ multivariate }
