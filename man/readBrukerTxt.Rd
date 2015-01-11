\name{readBrukerTxt}
\alias{readBrukerTxt}
\alias{readBrukerAscii}
\title{
Read and process a Bruker NMR spectrum saved using Topspin
}
\description{
These functions read files written by the Bruker Topspin software.  This function is not extensively tested.  Not normally called by the user; used by\code{\link{files2SpectraObject}}.
}
\usage{
readBrukerTxt(file = "", debug = FALSE)
readBrukerAscii(file = "", debug = FALSE)
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
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\keyword{ utilities }
