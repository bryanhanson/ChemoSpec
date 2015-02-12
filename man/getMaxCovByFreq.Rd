\name{getMaxCovByFreq}
\alias{getMaxCovByFreq}

\title{
Order Rows of a Covariance Matrix by Absolute Value 
}

\description{
Given a covariance matrix, the maximum absolute value of each row is computed.  The results are ordered, and optionally, limited to values greater than a particular value.
}
\usage{
getMaxCovByFreq(spectra, V = NULL, Quan = NULL)
}

\arguments{
  \item{spectra}{
An object of S3 class \code{\link{Spectra}}.
}
  \item{V}{
A numeric covariance matrix.
}
  \item{Quan}{
Numeric.  A value in the interval (0...1) giving the quantile to be selected.  For instance, \code{Quan = 0.1} selects the top 10 percent of values.
}
}

\value{
A data frame containing the frequencies from the \code{\link{Spectra}} object, the absolute value of the covariance at that frequency, and the relative covariance.  Sorted by absolute covariance in descending order.
}

\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}


\examples{
data(metMUD1)
V <- cov(metMUD1$data)
# Look at the top 1%
res <- getMaxCovByFreq(metMUD1, V, Quan = 0.01)
res
}


\keyword{ utilities }
