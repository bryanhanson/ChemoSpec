\name{robPCA}
\Rdversion{1.1}
\alias{robPCA}

\title{
Robust PCA of a Spectra Object
}
\description{
A wrapper which carries out robust PCA analysis on a \code{"Spectra"} object.  The data are row- and column-centered, and the user can select various options for scaling.  
}

\usage{
robPCA(spectra, choice = "noscale")
}

\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"}
}
  \item{choice}{
A character vector describing the type of scaling to be carried out.  One of \code{c("noscale", "mad")}.
}
}

\value{
An object of classes \code{"conPCA"} and \code{"princomp"} (see \code{\link{q2rPCA}}).  It includes a list element called \code{$method}, a character string describing the pre-processing carried out and the type of PCA performed (it appears on plots which you might make).
}


\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}\cr
K. Varmuza and P. Filzmoser \emph{Introduction to Multivariate Statistical Analysis in Chemometrics}, CRC Press, 2009.

}

\seealso{
See \code{\link[pcaPP]{PCAgrid}} on which this function is based.  For the classical version, see \code{\link{classPCA}}.\cr
For displaying the results, \code{\link{plotScree}}, \code{\link{plotScores}}, \code{\link{plotLoadings}}, \code{\link{plotScores3D}}
}

\examples{
data(SrE.IR)
results <- robPCA(SrE.IR, choice = "mad")
myt <- expression(bolditalic(Serenoa)~bolditalic(repens)~bold(IR~Spectra))
plotScores(SrE.IR, main = myt,
	results, pcs = c(1,2), ellipse = "rob", tol = 0.05)
}

\keyword{ multivariate }
\keyword{ robust }
