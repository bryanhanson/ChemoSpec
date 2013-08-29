\name{pcaDiag}
\Rdversion{1.1}
\alias{pcaDiag}

\title{
Outlier Diagnostic Plots for PCA of a Spectra Object
}

\description{
A function to carry diagnostics on the PCA results for a \code{"Spectra"} object.  Basically a wrapper to Filzmoser's \code{\link[chemometrics]{pcaDiagplot}} which colors everything according to the scheme stored in the \code{"Spectra"} object.  Works with PCA results of either class \code{"prcomp"} or class \code{"princomp"}.  Works with either classical or robust PCA results.
}

\usage{
pcaDiag(spectra, pca, pcs = 3, quantile = 0.975,
	plot = c("OD", "SD"), use.sym = FALSE, ...)
}

\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"}.
}
  \item{pca}{
An object of class \code{\link{prcomp}} or \code{\link{prcomp}}, modified to include a character string (\code{$method}) describing the pre-processing carried out and the type of PCA performed.
}
  \item{pcs}{
As per \code{\link[chemometrics]{pcaDiagplot}}.  The number of principal components to include.
}
  \item{quantile}{
As per \code{\link[chemometrics]{pcaDiagplot}}.  The significance criteria to use as a cutoff.
}
  \item{plot}{
A character string, indicating whether to plot the score distances or orthogonal distances, or both.  Options are \code{c("OD", "SD")}.
}
  \item{use.sym}{logical; if true, the color scheme is change to black and symbols are used for plotting.}
  \item{\dots}{
Additional parameters to be passed to the plotting functions.
}
}

\details{
If both plots are desired, the output should be directed to a file rather than the screen.  Otherwise, the 2nd plot overwrites the 1st in the active graphics window.  Alternatively, just call the function twice, once specifying \code{OD} and once specifying \code{SD}.
}

\value{
A list is returned as described in \code{\link[chemometrics]{pcaDiagplot}}, so the result must be assigned or it will appear at the console.  Side effect is a plot.
}


\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}\cr
K. Varmuza and P. Filzmoser \emph{Introduction to Multivariate Statistical Analysis in Chemometrics}, CRC Press, 2009.

}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
\code{\link[chemometrics]{pcaDiagplot}} in package \code{chemometrics} for the underlying function.
}

\examples{
data(SrE.IR)
results <- classPCA(SrE.IR, choice = "noscale")
temp <- pcaDiag(SrE.IR, results, pcs = 2, plot = "OD")
temp <- pcaDiag(SrE.IR, results, pcs = 2, plot = "SD")
}

\keyword{ multivariate }
