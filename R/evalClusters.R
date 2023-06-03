#'
#' Evaluate or Compare the Quality of Clusters Quantitatively
#'
#' This function is a wrapper for \code{NbClust} in package \pkg{NbClust}. It
#' can be used to quantitatively compare different clustering options.
#'
#' @param spectra `r .writeDoc_Spectra1()`
#'
#' @param \dots Other parameters to be passed to the function. In particular,
#' \code{NbClust} package will need some parameters.  See the
#' example.
#'
#' @return A list giving the results, as described in \code{\link[NbClust]{NbClust}}.
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
#' res <- evalClusters(metMUD2, min.nc = 2, max.nc = 5, method = "average", index = "kl")
#' }
#'
#' @export evalClusters
#'
evalClusters <- function(spectra, ...) {
  .chkArgs(mode = 11L)

    if (!requireNamespace("NbClust", quietly = TRUE)) {
      stop("You need install package NbClust to use this function")
    }
    res <- NbClust::NbClust(spectra$data, ...)
    return(res)
}
