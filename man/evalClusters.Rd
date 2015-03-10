\name{evalClusters}
\alias{evalClusters}
\title{
Evaluate or Compare the Quality of Clusters Quantitatively
}
\description{
This function is a wrapper to two functions: \code{\link{intCriteria}} function in package \code{clusterCrit}, and \code{\link{NbClust}} in package \code{NbClust}. It can be used to quantitatively compare different clustering options.
}
\usage{
evalClusters(spectra, pkg = "NbClust",
hclst = NULL, k = NULL, h = NULL, crit = "Dunn", ...)
}
\arguments{
  \item{spectra}{
An object of S3 class \code{\link{Spectra}}.
}
  \item{pkg}{
Character.  One of \code{c("NbClust", "clusterCrit")}.  The package to use for comparing clusters.
}

  \item{hclst}{
An object of S3 class \code{"hclust"}.  Only applies to \code{pkg = "clusterCrit"}.
}
  \item{k}{
Integer.  The number of groups in which to cut the tree (\code{hclust}).  Only applies to \code{pkg = "clusterCrit"}.
}
  \item{h}{
Numeric.  The height at which to cut the tree.  Either \code{k} or \code{h} must be given, with \code{k} taking precedence.  See \code{\link{cutree}}.  Only applies to \code{pkg = "clusterCrit"}.
}
  \item{crit}{
String.  A string giving the criteria to be used in evaluating the quality of the cluster.  See \code{\link{intCriteria}}.  Only applies to \code{pkg = "clusterCrit"}.
}
  \item{\dots}{
Other parameters to be passed to the functions. In particular, the default \code{NbClust} package will need some parameters.  See the example.
}

}

\value{
A list giving the results, as described in \code{\link{intCriteria}} or \code{\link{NbClust}}.
}

\details{
Both of the packages used here compute very similar quantities.  For details, see the publication and respective vignettes.  Package \code{clusterCrit} takes the approach in which you cluster in a separate step using whatever parameters you like, then the tree is cut either at a given height or in such a way as to produce a fixed number of groups.  One or more indices are then computed.  Then, you repeat this process with different clustering criteria, and compare.  Package \code{NbClust} allows one to specify a range of possible number of clusters and a few other parameters and will return indices corresponding to each set options, which is somewhat more automated.
}

\references{
M. Charrad et. al.  NbClust: An R Package for Determining the Relevant Number of Clusters in a Data Set.  J. Stat. Soft. vol. 61 no. 6 October 2014.

\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
\code{\link{hclust}} for the underlying base function.  \code{\link{hcaSpectra}} for HCA analysis of a \code{\link{Spectra}} object.  \code{\link{hcaScores}} for HCA analysis of PCA scores from a \code{\link{Spectra}} object.  \code{\link{plotHCA}} for the plotting function in \code{ChemoSpec}.
}

\examples{
# These are a little slow for CRAN checking
\dontrun{
data(metMUD2)
# Using clusterCrit
res1 <- hcaSpectra(metMUD2) # default clustering and distance methods
res2 <- hcaSpectra(metMUD2, d.method = "cosine")
# The return value from hcaSpectra is a list with hclust as the first element.
crit1 <- evalClusters(metMUD2, pkg = "clusterCrit", hclst = res1[[1]], k = 2)
crit2 <- evalClusters(metMUD2, pkg = "clusterCrit", hclst = res2[[1]], k = 2)
# crit1 and crit2 can now be compared.
#
# Using NbClust
res3 <- evalClusters(metMUD2, min.nc = 2, max.nc = 5, method = "average", index = "kl")
}
}

\keyword{ multivariate }
\keyword{ cluster }
