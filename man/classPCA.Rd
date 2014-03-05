\name{classPCA}
\Rdversion{1.1}
\alias{classPCA}
\title{
Classical PCA of Spectra Objects
}
\description{
A wrapper which carries out classical PCA analysis on a \code{"Spectra"} object.  The user can select various options for scaling.  There is no normalization by rows - do this manually using \code{\link{normSpectra}}.  There is an option to control centering, but this is mainly for compatibility with the \code{\link{aovPCA}} series of functions.  Centering the data should always be done in PCA and it is the default here.
}
\usage{
classPCA(spectra, choice = "noscale", cent = TRUE)
}
\arguments{
  \item{spectra}{An object of S3 class \code{"Spectra"}}.

  \item{choice}{A character string indicating the choice of scaling.  One of \code{c("noscale", "autoscale", "Pareto").}}

  \item{cent}{Logical: whether or not to center the data.  Always center the data unless you know it to be already centered.}
}
\details{
The scale choice \code{autoscale} scales the columns by their standard deviation.  \code{Pareto} scales by the square root of the standard deviation.
}

\value{
An object of class \code{\link{prcomp}}, modified to include a list element called \code{$method}, a character string describing the pre-processing carried out and the type of PCA performed (it appears on plots which you might make).
}


\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}\cr
K. Varmuza and P. Filzmoser \emph{Introduction to Multivariate Statistical Analysis in Chemometrics}, CRC Press, 2009.

}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
\code{\link{prcomp}} for the underlying function, \code{\link{robPCA}} for analogous robust PCA calculations.\cr
For displaying the results, \code{\link{plotScree}}, \code{\link{plotScores}}, \code{\link{plotLoadings}}, \code{\link{plot2Loadings}}.
}
\examples{
data(SrE.IR)
results <- classPCA(SrE.IR, choice = "noscale")
myt <- expression(bolditalic(Serenoa)~bolditalic(repens)~bold(IR~Spectra))
plotScores(SrE.IR, main = myt, results, pcs = c(1,2),
	ellipse = "rob", tol = 0.05)
}

\keyword{ multivariate }
