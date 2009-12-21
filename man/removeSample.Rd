\name{removeSample}
\Rdversion{1.1}
\alias{removeSample}

\title{
Remove Samples from a Spectra Object
}

\description{
Removes specified samples from a \code{"Spectra"} object.
}
\usage{
removeSample(spectra, rem.sam)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"}
}
  \item{rem.sam}{
Either an integer vector specifying the samples to be removed, or a character vector giving the sample names to be removed.
}
}

\details{
If \code{rem.sam} is a character vector, the sample names are grepped for the corresponding values.  Remember that the grepping process is greedy, i.e. grepping for "XY" find not only "XY" but also "XYZ".  Unused levels in \code{$groups} are dropped.
}

\value{
A modified object of S3 class \code{"Spectra"}.
}


\seealso{
\code{\link{removeFreq}} to remove selected frequencies from a \code{"Spectra"} object.
}

\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
data(CuticleIR)
new1 <- removeSample(CuticleIR, rem.sam = 20)
# removes the 20th spectrum/sample
new2 <- removeSample(CuticleIR, rem.sam = "GE")
# removes all samples whose name contains "GE"
new3 <- removeSample(CuticleIR, rem.sam = "GE10")
# removes one spectrum/sample with this exact name.

}

\keyword{ utilities }
\keyword{ manip }