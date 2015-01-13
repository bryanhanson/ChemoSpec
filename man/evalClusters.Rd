\name{evalClusters}
\alias{evalClusters}
\title{
Evaluate or Compare the Quality of Clusters Quantitatively
}
\description{
This function is a wrapper to the the \code{\link{intCriteria}} function in package \code{clusterCrit}.  It can be used to quantitatively compare two different clustering strategies.
}
\usage{
evalClusters(spectra, hclst, k = NULL, h = NULL, crit = "Dunn")
}
\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"}.
}
  \item{hclst}{
An object of S3 class \code{"hclust"}.
}
  \item{k}{
Integer.  The number of groups in which to cut the tree (\code{hclust}).
}
  \item{h}{
Numeric.  The height at which to cut the tree.  Either \code{k} or \code{h} must be given, with \code{k} taking precedence.  See \code{\link{cutree}}.
}
  \item{crit}{
String.  A string giving the criteria to be used in evaluating the quality of the cluster.  See \code{\link{intCriteria}}.
}
}

\value{
A list giving the results, as described in \code{\link{intCriteria}}.
}

\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
\code{\link{hclust}}
\code{\link{hcaSpectra}} for HCA analysis of a \code{"Spectra"} object.
\code{\link{hcaScores}} for HCA analysis of PCA scores from a \code{"Spectra"} object.
\code{\link{plotHCA}}
}

\examples{
data(SrE.IR)
res1 <- hcaSpectra(SrE.IR, main = myt) # default clustering and distance methods
res2 <- hcaSpectra(SrE.IR, main = myt, c.method = "median", d.method = "manhattan")
# The return value from hcaSpectra is a list with hclust as the first element.
crit1 <- evalClusters(SrE.IR, res1[[1]], k = 4)
crit2 <- evalClusters(SrE.IR, res2[[1]], k = 4)
# res1 gave a better clustering

}

\keyword{ multivariate }
\keyword{ cluster }
