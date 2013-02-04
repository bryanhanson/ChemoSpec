\name{binData}
\alias{binData}

\title{Bin or Bucket Data}

\description{
This function accepts a vector of x-values and averages them in groups of \code{bin.ratio} data points.  It also accepts a vector of y-values and sums them in groups of \code{bin.ratio} data points.  Both x and y data can be processed in the same call, or they can be processed separately.
}
\usage{
binData(x = NULL, y = NULL, bin.ratio = 2)
}
\arguments{
  \item{x}{
An optional vector of x values to be averaged in groups of \code{bin.data} points.
}
  \item{y}{
An optional vector of y values to be summed in groups of \code{bin.data} points.
}
  \item{bin.ratio}{
An integer giving the binning ratio, that is, the number of points to be grouped together into one subset of data.
}

}
\details{
The x and y values must be contiguous in the sense that there are no gaps in the values (i.e., x[n + 1] - x[n] must be the same for the entire data set; this can be checked with \code{\link{diff}} and is checked internally.  Note that this function is normally called by \code{\link{binBuck}} and that function can handle gaps, sending each continuous piece of data here to be binned.  If length(x or y) is not divisible by bin.ratio to give a whole number, data points are removed from the beginning of x or y until it is, and the number of data points removed is reported at the console.  The algorithm forces the requested bin.ratio to be used.
}
\value{As appropriate, a data.frame containing the following elements:
\item{mean.x}{A vector of the averaged x values.  Length will be approximately length(x)/bin.ratio, with length(x) adjusted as described above if this does not give a whole number.}
\item{sum.y}{A vector of the summed y values.  Length will be approximately length(y)/bin.ratio, with length(y) adjusted as described above if this does not give a whole number.}
}

\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\examples{
x <- seq(0, 1000, length.out = 3000); y <- rnorm(3000)
res <- binData(x, y)
length(res$mean.x) # will be half of the original length
# Now try it with bin.ratio that does not divide into 3000
x <- seq(0, 1000, length.out = 3000); y <- rnorm(3000)
res <- binData(x, y, bin.ratio = 7)
length(res$mean.x)
}
\keyword{ utilities }
\keyword{ manip }
