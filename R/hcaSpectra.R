#'
#' Plot HCA Results of a Spectra Object
#'
#' A wrapper which carries out HCA and plots a dendrogram colored by the
#' information in a \code{\link{Spectra}} object.  Many methods for computing
#' the clusters and distances are available.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param c.method A character string describing the clustering method; must be
#' acceptable to \code{\link{hclust}}.
#'
#' @param d.method A character string describing the distance calculation
#' method; must be acceptable as a method in \code{\link{rowDist}}.
#'
#' @param use.sym A logical; if true, use no color and use lower-case letters
#' to indicate group membership.
#'
#' @param leg.loc Character; if \code{"none"} no legend will be drawn.
#' Otherwise, any string acceptable to \code{\link{legend}}.
#'
#' @param \dots Other parameters to be passed to the plotting functions.
#'
#' @return A list, containing an object of class \code{\link{hclust}} and an
#' object of class \code{\link{dendrogram}}.  The side effect is a plot.
#'
#' @template authors-BH
#'
#' @seealso \code{\link{hclust}} for the underlying function.
#' \code{\link{hcaScores}} for similar analysis of PCA scores from a
#' \code{\link{Spectra}} object.  Additional documentation at
#' \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @keywords multivariate cluster
#'
#' @examples
#'
#' data(SrE.IR)
#' myt <- expression(bolditalic(Serenoa) ~ bolditalic(repens) ~ bold(IR ~ Spectra))
#' res <- hcaSpectra(SrE.IR, main = myt)
#'
#' @export hcaSpectra
#' @importFrom stats hclust
#'
hcaSpectra <- function(spectra,
                       c.method = "complete", d.method = "euclidean",
                       use.sym = FALSE, leg.loc = "topright", ...) {
  .chkArgs(mode = 11L)
  chkSpectra(spectra)

  if (use.sym) spectra$names <- paste(spectra$alt.sym, spectra$names, sep = " ")
  row.names(spectra$data) <- spectra$names # needed to create labels later
  distance <- rowDist(spectra$data, method = d.method)

  sub.title <- paste("clustering method: ", c.method, "      distance method: ", d.method, sep = "")

  hclst <- hclust(distance, method = c.method)

  d <- .plotHCA(
    spectra = spectra, hclst = hclst, sub.title = sub.title,
    use.sym = use.sym, leg.loc = leg.loc, ...
  )
  L <- list(hclst = hclst, dend = d)
  return(L)
}
