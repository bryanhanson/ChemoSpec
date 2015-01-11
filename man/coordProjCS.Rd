\name{coordProjCS}
\alias{coordProjCS}
\title{
Modified Version of coordProj from mclust
}
\description{
This is a modified version of the function \code{\link{coordProj}} from \code{mclust}. In this version, the original symbol scheme for the error plot is changed to simply plot an X over the letters identifying the groups.
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
See \code{\link{coordProj}}.
}
  \item{dimens}{
See \code{\link{coordProj}}.
}
  \item{parameters}{
See \code{\link{coordProj}}.
}
  \item{z}{
See \code{\link{coordProj}}.
}
  \item{classification}{
See \code{\link{coordProj}}.
}
  \item{truth}{
See \code{\link{coordProj}}.
}
  \item{uncertainty}{
See \code{\link{coordProj}}.
}
  \item{what}{
See \code{\link{coordProj}}.
}
  \item{quantiles}{
See \code{\link{coordProj}}.
}
  \item{symbols}{
See \code{\link{coordProj}}.
}
  \item{colors}{
See \code{\link{coordProj}}.
}
  \item{scale}{
See \code{\link{coordProj}}.
}
  \item{xlim}{
See \code{\link{coordProj}}.
}
  \item{ylim}{
See \code{\link{coordProj}}.
}
  \item{CEX}{
See \code{\link{coordProj}}.
}
  \item{PCH}{
See \code{\link{coordProj}}.
}
  \item{identify}{
See \code{\link{coordProj}}.
}
  \item{\dots}{
See \code{\link{coordProj}}.
}
}
\value{
See \code{\link{coordProj}}.
}
\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\keyword{ multivariate }
\keyword{ cluster }
