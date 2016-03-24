\name{sgfSpectra}
\alias{sgfSpectra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Apply Savitzky-Golay filters to a Spectra object
}
\description{
This function is a simple wrapper around the function \code{\link[signal]{sgolayfilt}}.  It allows one to apply Savitzky-Golay filters to a \code{Spectra} object in a convenient way.
}
\usage{
sgfSpectra(spectra, m = 0, ...)
}
\arguments{
  \item{spectra}{
An object of S3 class \code{\link{Spectra}} to be checked.
}
  \item{m}{
The desired m-th derivative.  \code{m = 0} smooths the data, \code{m = 1} gives the first derivative etc.
}
\item{\dots}{
Other parameters to be passed to \code{\link[signal]{sgolayfilt}}.
}

}

\value{
A object of class \code{\link{Spectra}}.
}

\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
  data(SrE.IR)
  myt1 <- expression(bolditalic(Serenoa)~bolditalic(repens)~bold(IR~Spectra))
  myt2 <- expression(bolditalic(Serenoa)~bolditalic(repens)~bold(IR~Spectra~(Smoothed)))

  par(mfrow = c(2, 1))
  plotSpectra(SrE.IR, xlim = c(1900, 2100), yrange = c(0, 0.05), main = myt1)
  test <- sgfSpectra(SrE.IR)
  plotSpectra(test, xlim = c(1900, 2100), yrange = c(0, 0.05), main = myt2)
  par(mfrow = c(1, 1))
}

\keyword{ utilities }
\keyword{ multivariate }
