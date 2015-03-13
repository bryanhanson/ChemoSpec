\name{coordProjCS}
\alias{coordProjCS}
\title{
Modified Version of coordProj from mclust
}
\description{
This is a modified version of the function \code{coordProj} from \code{mclust}. In this version, the original symbol scheme for the error plot is changed to simply plot an X over the letters identifying the groups.  An internal function, not generally called by the user.
}
\usage{
coordProjCS(data, dimens = c(1, 2), parameters = NULL,
	z = NULL, classification = NULL, truth = NULL,
	uncertainty = NULL, what = c("classification", "errors", "uncertainty"),
	quantiles = c(0.75, 0.95), symbols = NULL, colors = NULL, scale = FALSE,
	xlim = NULL, ylim = NULL, CEX = 1, PCH = ".", identify = FALSE, ...)
}
\arguments{
  \item{data}{
See \code{coordProj}.
}
  \item{dimens}{
See \code{coordProj}.
}
  \item{parameters}{
See \code{coordProj}.
}
  \item{z}{
See \code{coordProj}.
}
  \item{classification}{
See \code{coordProj}.
}
  \item{truth}{
See \code{coordProj}.
}
  \item{uncertainty}{
See \code{coordProj}.
}
  \item{what}{
See \code{coordProj}.
}
  \item{quantiles}{
See \code{coordProj}.
}
  \item{symbols}{
See \code{coordProj}.
}
  \item{colors}{
See \code{coordProj}.
}
  \item{scale}{
See \code{coordProj}.
}
  \item{xlim}{
See \code{coordProj}.
}
  \item{ylim}{
See \code{coordProj}.
}
  \item{CEX}{
See \code{coordProj}.
}
  \item{PCH}{
See \code{coordProj}.
}
  \item{identify}{
See \code{coordProj}.
}
  \item{\dots}{
See \code{coordProj}.
}
}
\value{
See \code{coordProj}.
}
\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\keyword{ multivariate }
\keyword{ cluster }
