\name{plotScoresDecoration}
\Rdversion{1.1}
\alias{plotScoresDecoration}

\title{
Decorate PCA Score Plot of a Spectra Object
}
\description{
Utility function to carry out misc. labeling functions on the PCA score plot of a \code{"Spectra"} object.  Never called by the user.
}
\usage{
plotScoresDecoration(spectra, pca, pcs = c(1, 2), tol = "none")
}

\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"}
}
  \item{pca}{
An object of class \code{\link{prcomp}}, modified to include a list element called \code{$method}, a character string describing the pre-processing carried out and the type of PCA performed (it appears on the plot).  This is automatically provided if \code{ChemoSpec} functions \code{\link{classPCA}} or \code{\link{robPCA}} were used to create \code{pca}.
}
  \item{pcs}{
A vector of two integers specifying the PCA scores to plot.
}
  \item{tol}{
A number describing the fraction of points to be labeled.  \code{tol = 1.0} labels all the points; \code{tol = 0.05} labels the most extreme 5 percent.
}
}
\value{
None.  The score plot is decorated.
}


\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\keyword{ utilities }
