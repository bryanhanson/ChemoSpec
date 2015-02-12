\name{chkSpectra}
\Rdversion{1.1}
\alias{chkSpectra}

\title{
Verify the Integrity of a Spectra Object
}

\description{
Utility function to verify that the structure of a \code{\link{Spectra}} object (an instance of an S3 object) is internally consistent. Rather than directly manipulating a \code{\link{Spectra}} object, one should manipulate it via \code{\link{removeFreq}} or \code{\link{removeSample}}.
}

\usage{
chkSpectra(spectra, confirm = FALSE)
}

\arguments{
  \item{spectra}{
An object of S3 class \code{\link{Spectra}} to be checked.
}
  \item{confirm}{
Logical indicating whether or not to write the results to the console, as would be desirable for interactive use.
}
}

\details{
This function is similar in spirit to \code{\link{validObject}} in the S4 world.  When used at the console, and the object is OK, no message is written unless \code{confirm = TRUE}.  At the console, if there is a problem, messages are issued regardless of the value of \code{confirm}.  When used in a function, this function is silent (assuming \code{confirm = FALSE}) unless there is a problem.
}

\value{
None; messages will be printed at the console if there is a problem.
}


\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
data(metMUD1)
chkSpectra(metMUD1, confirm = TRUE) # OK
# What's next is the wrong way to manipulate a Spectra object.
# Use removeSample instead.
# We won't run during checking as an error is raised
\dontrun{
remove <- c(20:40) 
metMUD1$freq <- metMUD1$freq[-remove]
chkSpectra(metMUD1, confirm = TRUE) # not OK, you didn't listen to me!
}
}

\keyword{ classes }
\keyword{ utilities }
