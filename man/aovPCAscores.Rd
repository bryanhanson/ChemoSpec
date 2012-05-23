\name{aovPCAscores}
\alias{aovPCAscores}
\title{
Conduct PCA and plot aovPCA Scores of a Spectra Object
}
\description{
Uses the results from \code{\link{aovPCA}} to conduct PCA and plot the scores.
}
\usage{
aovPCAscores(spectra, LM, plot = 1, type = "class", choice = NULL, ...)
}
\arguments{
  \item{spectra}{
An object of S3 class "Spectra".
}
  \item{LM}{
List of matrices created by \code{\link{aovPCA}}.
}
  \item{plot}{
An integer specifying which scores to plot.
}
  \item{type}{
Either classical ("cls") or robust ("rob"); Results in either \code{\link{classPCA}} or \code{\link{robPCA}} being called on the \code{\link{Spectra}} object.
}
  \item{choice}{
The type of scaling to be performed.  See \code{\link{classPCA}} and \code{\link{robPCA}} for details.
}
  \item{\dots}{
Additional parameters to be passed to \code{\link{plotScores}}.
}
}
\details{
Argument \code{plot} is used to select a matrix from those in \code{LM}.  The residual error matrix is then added to the selected matrix before performing PCA.  Use \code{names(LM)} to see which factor is stored in which matrix.
	}
\value{
Returns the PCA results, and creates the requested plot.
}
\references{
Harrington, et. al 2005, Pinto, et. al 2008.
}
\author{
Matthew J. Keinsley and Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}


\seealso{
The use of this function can be seen in \code{\link{aovPCA}}.  See also \code{\link{plotScores}}.
}

\keyword{ multivariate }
\keyword{ htest }
