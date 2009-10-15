\name{colLeaf}
\Rdversion{1.1}
\alias{colLeaf}

\title{
Color Dendrogram of a Spectra Object
}

\description{
Utility function called by \code{\link{HCA}} and \code{\link{ScoreHCA}} via \code{dendrapply}. Not intended for end-users.
}
\usage{
colLeaf(n, spectra)
}

\arguments{
  \item{n}{
The leaf number (an integer).
}
  \item{spectra}{
An object of S3 class \code{"Spectra"}.
}
}

\value{
None. The leaf colors of the relevant dendrogram object are modified to correspond to those in \code{spectra}
}

\section{Called by}{\code{\link{HCA}}, \code{\link{ScoreHCA}}}

\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\note{
The basic idea was found in the help archives.  I can't write this kind of stuff!
}

\keyword{ utilities }
