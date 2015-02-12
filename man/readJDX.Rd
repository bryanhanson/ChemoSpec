\name{readJDX}
\alias{readJDX}

\title{
Read and process a JCAMP-DX file.
}
\description{
This function reads files with the JCAMP-DX format (and extension \code{.dx}).  This function is not extensively tested.  It does not work with NMR data.  Not normally called by the user; used by \code{\link{files2SpectraObject}}.
}
\usage{
readJDX(file = "", debug = FALSE)
}

\arguments{
  \item{file}{
Character; the path to the file to be processed.
}
  \item{debug}{
Logical indicating if file names and progress information should be printed to the console.  Useful for troubleshooting.
}
}
\details{
The data block must be of the type XYDATA=(X++(Y..Y))  It handles AFFN format for the data block and only with '+', '-' or ' ' as the separator.
}
\value{
A data frame with the following elements:
\item{x}{Extracted frequency values}
\item{y}{Extracted intensities}
}
\references{
The details of the JCAMP-DX formats can be found at at \url{http://www.jcamp-dx.org/}

\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\keyword{ utilities }
