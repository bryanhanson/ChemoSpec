\name{removeSample}
\Rdversion{1.1}
\alias{removeSample}
\alias{removeGroup}

\title{
Remove Samples or Groups from a Spectra Object
}

\description{
Removes specified samples from a \code{"Spectra"} object.
}
\usage{
removeSample(spectra, rem.sam)
removeGroup(spectra, rem.group)
}
\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"}
}
  \item{rem.sam}{
Either an integer vector specifying the samples to be removed, or a character vector giving the sample names to be removed.
}
  \item{rem.group}{
A character vector giving the groups to be removed.
}
}

\details{
If \code{rem.sam} or \code{rem.group} is a character vector, the sample or group names are grepped for the corresponding values.  Remember that the grepping process is greedy, i.e. grepping for "XY" find not only "XY" but also "XYZ".  Unused levels in \code{$groups} are dropped.  \code{removeSample} removes samples (objects) based upon the sample names.  \code{removeGroup} removes entire groups based upon the group name. \code{removeGroup} will report if it finds extra data elements.  These may be per sample data.  If so, the user will need to manually edit them using the indices reported to the console.
}

\value{
A modified object of S3 class \code{"Spectra"}.
}

\seealso{
\code{\link{removeFreq}} to remove selected frequencies from a \code{"Spectra"} object.
}

\references{
\url{https://github.com/bryanhanson/ChemoSpec}
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
