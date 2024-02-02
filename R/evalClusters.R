#'
#' Evaluate or Compare the Quality of Clusters Quantitatively
#'
#' This function is a wrapper to two functions: \code{intCriteria} function in
#' package \pkg{clusterCrit}, and \code{NbClust} in package \pkg{NbClust}. It
#' can be used to quantitatively compare different clustering options.
#'
#' Both of the packages used here compute very similar quantities.  For
#' details, see the publication and respective vignettes.  Package
#' \pkg{clusterCrit} takes the approach in which you cluster in a separate
#' step using whatever parameters you like, then the tree is cut either at a
#' given height or in such a way as to produce a fixed number of groups.  One
#' or more indices are then computed.  Then, you repeat this process with
#' different clustering criteria, and compare.  Package \pkg{NbClust} allows
#' one to specify a range of possible number of clusters and a few other
#' parameters and will return indices corresponding to each set options, which
#' is somewhat more automated.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param pkg Character.  One of \code{c("NbClust", "clusterCrit")}.  The
#' package to use for comparing clusters.
#'
#' @param hclst An object of S3 class \code{hclust}.  Only applies to
#' \code{pkg = "clusterCrit"}.
#'
#' @param k Integer.  The number of groups in which to cut the tree
#' (\code{hclust}).  Only applies to \code{pkg = "clusterCrit"}.
#'
#' @param h Numeric.  The height at which to cut the tree.  Either \code{k} or
#' \code{h} must be given, with \code{k} taking precedence.  See
#' \code{\link{cutree}}.  Only applies to \code{pkg = "clusterCrit"}.
#'
#' @param crit String.  A string giving the criteria to be used in evaluating
#' the quality of the cluster.  See \code{liintCriteria}.  Only applies to
#' \code{pkg = "clusterCrit"}.
#'
#' @param \dots Other parameters to be passed to the functions. In particular,
#' the default \code{NbClust} package will need some parameters.  See the
#' example.
#'
#' @return A list giving the results, as described in \code{\link[clusterCrit]{intCriteria}} or
#' \code{\link[NbClust]{NbClust}}.
#'
#' @author `r .writeDoc_Authors("BH")`
#'
#' @seealso \code{\link{hclust}} for the underlying base function.
#' \code{\link{hcaSpectra}} for HCA analysis of a \code{\link{Spectra}} object.
#' \code{\link{hcaScores}} for HCA analysis of PCA scores from a
#' \code{\link{Spectra}} object. Additional documentation at
#' \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @references M. Charrad et. al.  "NbClust: An R Package for Determining the
#' Relevant Number of Clusters in a Data Set."  J. Stat. Soft. vol. 61 no. 6
#' October 2014.
#'
#' @keywords multivariate cluster
#'
#' @examples
#'
#' \dontrun{
#' data(metMUD2)
#'
#' # Using clusterCrit
#' res1 <- hcaSpectra(metMUD2) # default clustering and distance methods
#' res2 <- hcaSpectra(metMUD2, d.method = "cosine")
#' # The return value from hcaSpectra is a list with hclust as the first element.
#' crit1 <- evalClusters(metMUD2, pkg = "clusterCrit", hclst = res1[[1]], k = 2)
#' crit2 <- evalClusters(metMUD2, pkg = "clusterCrit", hclst = res2[[1]], k = 2)
#' # crit1 and crit2 can now be compared.
#'
#' # Using NbClust
#' res3 <- evalClusters(metMUD2, min.nc = 2, max.nc = 5, method = "average", index = "kl")
#' }
#'
#' @export evalClusters
#' @importFrom stats hclust cutree
#'
evalClusters <- function(spectra, pkg = "NbClust", hclst = NULL,
                         k = NULL, h = NULL, crit = "Dunn", ...) {
  .chkArgs(mode = 11L)

  if (pkg == "NbClust") {
    if (!requireNamespace("NbClust", quietly = TRUE)) {
      stop("You need install package NbClust to use this function/option")
    }
    res <- NbClust::NbClust(spectra$data, ...)
    return(res)
  }

  if (pkg == "clusterCrit") {
    if (!requireNamespace("clusterCrit", quietly = TRUE)) {
      stop("You need install package clusterCrit to use this function/option")
    }
    if (is.null(hclust)) stop("You must provide an hclust object")
    ct <- cutree(hclst, k = k, h = h)
    res <- clusterCrit::intCriteria(spectra$data, ct, crit, ...)
    return(res)
  }
}
