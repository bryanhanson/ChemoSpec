\name{aovPCAscores}
\alias{aovPCAscores}
\title{
Plot ANOVA-PCA Scores from a Spectra Object
}
\description{
Uses the results from \code{\link{aov_pcaSpectra}} to conduct PCA and plot the scores.
}
\usage{
aovPCAscores(spectra, LM, plot = 1, type = "class", choice = NULL, ...)
}
\arguments{
  \item{spectra}{
An object of S3 class \code{\link{Spectra}}.
}
  \item{LM}{
List of matrices created by \code{\link{aov_pcaSpectra}}.
}
  \item{plot}{
An integer specifying which scores to plot.
}
  \item{type}{
Either classical ("cls") or robust ("rob"); Results in either \code{\link{c_pcaSpectra}} or \code{\link{r_pcaSpectra}} being called on the \code{\link{Spectra}} object.
}
  \item{choice}{
The type of scaling to be performed.  See \code{\link{c_pcaSpectra}} and \code{\link{r_pcaSpectra}} for details.
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
Pinto, Bosc, Nocairi, Barros, and Rutledge. "Using ANOVA-PCA for Discriminant Analysis: ..." Analytica Chimica Acta 629.1-2 (2008): 47-55.

Harrington, Vieira, Espinoza, Nien, Romero, and Yergey. "Analysis of Variance--Principal Component Analysis: ..." Analytica Chimica Acta 544.1-2 (2005): 118-27.

\url{https://github.com/bryanhanson/ChemoSpec}
}
\author{
Matthew J. Keinsley and Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}


\seealso{
The use of this function can be seen in \code{\link{aov_pcaSpectra}}.  See also \code{\link{plotScores}}.
}

\keyword{ multivariate }
\keyword{ htest }
