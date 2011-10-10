\name{binBuck}
\alias{binBuck}
\title{
Bin or Bucket a Spectra Object
}
\description{
This function will bin a \code{"Spectra"} object by averaging every \code{bin.ratio} frequency values, and summing the corresponding intensity values.  The net effect is a smoothed and smaller data set.  If there are gaps in the frequency axis, each data chunk is processed separately.  Note: some folks refer to binning as bucketing.
}

\usage{
binBuck(spectra, bin.ratio)
}
\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"} to be binned.
}
  \item{bin.ratio}{
An integer giving the binning ratio, that is, the number of points to be grouped together into one subset of data.
}

}

\details{
If the frequency range is not divisible by bin.ratio to give a whole number, data points are removed from the beginning of the frequency data until it is, and the number of data points removed is reported at the console.  If there are gaps in the data where frequencies have been removed, each continuous piece is sent out and binned separately (by \code{\link{binBuck}}).
}

\value{
An object of S3 class \code{"Spectra"}.
}


\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\examples{
data(CuticleIR)
sumSpectra(CuticleIR)
res <- binBuck(CuticleIR, bin.ratio = 4)
sumSpectra(res)
}
\keyword{ utilities }
\keyword{ manip }