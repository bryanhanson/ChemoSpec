\name{sortCrossPeaks}
\alias{sortCrossPeaks}

\title{
Sort Rows of a Covariance Matrix of a Spectra Object
}

\description{
Given a covariance matrix, the maximum absolute value of each row is computed after excluding values near the diagonal.  By default, the results are ordered by the absolute value of the covariance, or optionally by frequency.  The results may be limited to a selected quantile of top results.

}
\usage{
sortCrossPeaks(spectra, V = NULL, window = NULL, Quan = NULL,
byFreq = FALSE, freqThres = 0.01, ...)
}

\arguments{
  \item{spectra}{
An object of S3 class \code{\link{Spectra}}.
}
  \item{V}{
A numeric covariance matrix, corresponding to \code{cov(spectra$data)}..
}
  \item{window}{
Numeric.  A value in the interval (0...1). This is converted to a number of data points near the diagonal which will be removed.  If \code{NULL}, the default, approximately 10 percent of the points are removed.
}
  \item{Quan}{
Numeric.  A value in the interval (0...1) giving the quantile to be selected.  For instance, \code{Quan = 0.1} selects the top 10 percent of the returned values.
}
  \item{byFreq}{
Logical.  Shall the results be sorted by frequency?  Only applies if \code{Quan != NULL}.  Frequencies closer than \code{freqThres} will be collapsed and replaced with their average.
}
  \item{freqThres}{
Numeric.  Only relevant if \code{Quan != NULL} and \code{byFreq = TRUE}. Frequencies closer than \code{freqThres} will be collapsed and replaced with their average.
}

  \item{\ldots}{
Arguments to be passed down stream.  In particular, you may wish to set \code{verbose = TRUE} which will pass through to \code{collapseRowsOrCols} and give information about which rows and columns which are collapsed if \code{Quan != NULL} and \code{byFreq = TRUE}.
}

}

\details{
The diagonal of a covariance matrix holds the variances, which are by definition the largest value in the row.  Because of the shape of an NMR peak, values near the diagonal essentially represent the variance as well.  We are interested in the values of the cross peaks, hence it is necessary to remove a number of data points near the diagonal from the computations.  The exact number to remove is subject to experimentation.

If the data were sorted by frequency without selecting the top values (i.e. \code{Quan = NULL}), there would be no point in collapsing the values as they are all equally spaced.  But when you have selected the top covariance values, the frequencies are no longer consecutive and equally spaced, so collapsing them replaces trivially close values with their averages.
}

\value{
A data frame containing the frequencies from the \code{\link{Spectra}} object, the maximum covariance at that frequency, the absolute value of the maximum covariance, and the relative absolute maximum covariance.
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
res <- sortCrossPeaks(metMUD1, V, Quan = 0.01)
res
}


\keyword{ utilities }
