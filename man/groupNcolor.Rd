\name{groupNcolor}
\Rdversion{1.1}
\alias{groupNcolor}

\title{
Assign Group Membership and Colors for a Spectra Object
}
\description{
A utility function which looks for \code{gr.crit} in the file names of .csv files and assigns group membership.  Also assigns a color, a symbol and an alternate symbol to each group.  Warnings are given if there are file names that don't match entries in \code{gr.crit} or there are entries in \code{gr.crit} that don't match any file names.  Not intended for users.}
\usage{
groupNcolor(spectra, gr.crit = NULL, gr.cols = c("auto"))
}
\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"}.  Until this function acts on \code{spectra} it is not quite complete.
}
  \item{gr.crit}{
As per \code{\link{files2SpectraObject}}
}
  \item{gr.cols}{
As per \code{\link{files2SpectraObject}}
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
\code{\link{files2SpectraObject}} for details; \code{\link{sumGroups}} to see the outcome.

}
\keyword{ utilities }
\keyword{ color }
