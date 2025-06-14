#'
#' Robust PCA of a Spectra Object
#'
#' A wrapper which carries out robust PCA analysis on a \code{\link{Spectra}}
#' object.  The data are row- and column-centered, and the user can select
#' various options for scaling.
#'
#' @param spectra `r .writeDoc_Spectra1()`
#'
#' @param choice A character vector describing the type of scaling to be
#' carried out.  One of \code{c("noscale", "mad")}.
#'
#' @return An object of classes \code{converted_from_princomp} and \code{prcomp}.
#' It includes a list element called \code{$method}, a
#' character string describing the pre-processing carried out and the type of
#' PCA performed (used to annotate plots).
#'
#' @seealso \code{\link[pcaPP]{PCAgrid}} for the underlying function,
#' \code{\link{c_pcaSpectra}} for classical PCA calculations,
#' \code{\link{s_pcaSpectra}} for sparse PCA calculations,
#' \code{\link{irlba_pcaSpectra}} for PCA via the IRLBA algorithm.
#' Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' `r .writeDoc_LinksShowPCAResults()`
#'
#' @references K. Varmuza and P. Filzmoser \emph{Introduction to Multivariate
#' Statistical Analysis in Chemometrics}, CRC Press, 2009.
#'
#' @author `r .writeDoc_Authors("BH")`
#'
#' @seealso \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @keywords multivariate robust
#'
#' @examples
#' \dontrun{
#' # This example assumes the graphics output is set to ggplot2 (see ?GraphicsOptions).
#' library("ggplot2")
#' data(metMUD1)
#' pca <- r_pcaSpectra(metMUD1)
#'
#' p1 <- plotScree(pca)
#' p1
#'
#' p2 <- plotScores(metMUD1, pca, pcs = c(1, 2), ellipse = "cls", tol = 0.05)
#' p2 <- p2 + ggtitle("Scores: metMUD1 NMR Data")
#' p2
#'
#' p3 <- plotLoadings(metMUD1, pca, loads = 1:2, ref = 1)
#' p3 <- p3 + ggtitle("Loadings: metMUD1 NMR Data")
#' p3
#' }
#' @export r_pcaSpectra
#'
#'
r_pcaSpectra <- function(spectra, choice = "noscale") {
  .chkArgs(mode = 11L)
  chkSpectra(spectra)

  if (.chkReqPkgs("pcaPP")) {
    choices <- c("noscale", "mad") # trap for invalid scaling method
    check <- choice %in% choices
    if (!check) stop("The choice of scaling parameter for robust PCA was invalid")

    # Note: PCAgrid produces an object of class princomp, not prcomp
    # so there must be some conversion to match class prcomp

    note <- choice
    if (choice == "noscale") choice <- NULL
    pca <- pcaPP::PCAgrid(spectra$data, k = 10, scale = choice, scores = TRUE)
    pca$method <- paste("l1median/", note, "/", "robust", sep = "")
    pca <- .r2qPCA(pca) # convert classes
  }
}
