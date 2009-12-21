\name{makeEllipsoid}
\alias{makeEllipsoid}
\title{
Create Ellipsoid
}
\description{
Given at least 3 data points, this function creates either classical or robust ellipsoids at a given confidence limit, in either 2D or 3D.  The ellipsoids consist of randomly generate points, which if plotted as tiny points, create a sort of transparent surface.
}
\usage{
makeEllipsoid(data, cl = 0.95, rob = FALSE, frac.pts.used = 0.8)
}
\arguments{
  \item{data}{
A matrix of at least 3 data points, with  x, y and optionally z in columns.  See details.
}
  \item{cl}{
The confidence limit desired.
}
  \item{rob}{
Logical, indicating if robust ellipsoids are to be computed.
}
  \item{frac.pts.used}{
If \code{rob = TRUE}, this is the fraction of points to be considered the "good" part of the data.  See the documentation for \code{\link{cov.rob}} for details.
}
}
\value{
A matrix of 2 or 3 columns, representing x, y and optionally z.  These are the coordinates of points specifying an ellipse which has a likelihood of containing the true mean at the given confidence limit.
}
\section{Called by}{
\code{\link{plotScoresRGL}}, \code{\link{mclust3dSpectra}}, \code{\link{mclust3D}}.
}

\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\note{
The idea was taken from "An Introduction to rggobi" found at the ggobi web site: \url{http://www.ggobi.org}.  I added the robust option.
}
\details{
If only x and y are provided, at least 3 points must be given, as 2 points defines a line, not an ellipse.  For 3D data, and \code{rob = FALSE}, at least 4 points must be provided.  If \code{rob = TRUE}, 5 points would be theoretically required, but the code forces 8 to avoid unusual cases which would fail.  If fewer than 8 are given, the computation shifts to classical with a warning.  Note that depending upon how this function is called, one may end up with classical and robust ellipsoids in the plot.  Remember too that because the points are randomly generated, the x, y pairs or x, y, z triplets are not related to each other, and one cannot plot lines from point to point.  See the example for a 2D ellipse.  If you want a function that generates x, y points suitable for connecting to each other via lines, see \code{\link{plotScoresCor}}.
}
\seealso{\code{\link{cov.rob}} for the function that does the work.}

\examples{
# 2D example
x <- rnorm(10, 2, 0.5)
y <- rnorm(10, -2, 2)
ell <- makeEllipsoid(cbind(x,y), cl = 0.99)
plot(ell[,1], ell[,2], col = "red", pch = 20, cex = 0.3)
points(x,y)


}
\keyword{ multivariate }
\keyword{ utilities }
