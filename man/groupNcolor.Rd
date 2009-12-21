\name{groupNcolor}
\Rdversion{1.1}
\alias{groupNcolor}

\title{
Assign Group Membership and Colors for a Spectra Object
}
\description{
A utility function which looks for \code{gr.crit} in the file names of .csv files and assigns group membership.  Also assigns a color, a symbol and an alternate symbol to each group.  Not intended for users.}
\usage{
groupNcolor(spectra, gr.crit = NULL, gr.cols = c("auto"))
}
\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"}.  Until this function acts on \code{spectra} it is not quite complete.
}
  \item{gr.crit}{
As per \code{\link{getManyCsv}}
}
  \item{gr.cols}{
As per \code{\link{getManyCsv}}
}
}
\value{
A \emph{complete} object of S3 class \code{"Spectra"}.  Until this function has done its job, an object of class \code{"Spectra"} will not pass checks as the assembly is not complete (see \code{\link{chkSpectra}}).)
}


\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
\code{\link{getManyCsv}} for details; \code{\link{sumGroups}} to see the outcome.

}
\keyword{ utilities }
\keyword{ color }