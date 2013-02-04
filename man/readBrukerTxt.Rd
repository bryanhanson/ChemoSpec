\name{readBrukerTxt}
\alias{readBrukerTxt}
\title{
Read and process a Bruker NMR spectrum (.txt) as written by their software.
}
\description{
This function reads files written by the Bruker software (with extension ".txt").  This function is not extensively tested.  Not normally called by the user; used by\code{\link{files2SpectraObject}}.
}
\usage{
readBrukerTxt(file = "", debug = FALSE)
}

\arguments{
  \item{file}{
Character; the path to the file to be processed.
}
  \item{debug}{
Logical indicating if file names and progress information should be printed to the console.  Useful for troubleshooting.
}
}
\value{
A data frame with the following elements:
\item{x}{Extracted frequency values}
\item{y}{Extracted intensities}
}
\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\keyword{ utilities }
