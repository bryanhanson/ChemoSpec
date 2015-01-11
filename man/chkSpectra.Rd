\name{chkSpectra}
\Rdversion{1.1}
\alias{chkSpectra}

\title{
Verify the Integrity of a Spectra Object
}

\description{
Utility function to verify that the structure of a \code{"Spectra"} object (an instance of an S3 object) is internally consistent. Rather than directly manipulating a \code{"Spectra"} object, one should manipulate it via \code{\link{removeFreq}} or \code{\link{removeSample}}.  Should not see much direct use by users.
}

\usage{
chkSpectra(spectra, confirm = FALSE)
}

\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"} to be checked.
}
  \item{confirm}{
Logical indicating whether or not to write the results to the console, as would be desirable for interactive use.
}
}

\details{
This function is similar in spirit to \code{\link{validObject}} in the S4 world.  When used at the console, and the object is OK, no message is written unless \code{confirm = TRUE}.  At the console, if there is a problem, messages are issued regardless of the value of \code{confirm}.  When used in a function, this function is silent (assuming \code{confirm = FALSE}) unless there is a problem.
}

\value{None; messages will be printed at the console if there is a problem.
}


\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
data(CuticleIR)
chkSpectra(CuticleIR, confirm = TRUE) # OK
# What's next works, but is the wrong way to manipulate a "Spectra" object.
# Use removeSample instead.
remove <- c(20:40) 
CuticleIR$freq <- CuticleIR$freq[-remove]
\dontrun{chkSpectra(CuticleIR, confirm = TRUE) # not OK, you didn't listen to me!}
}

\keyword{ classes }
\keyword{ utilities }
