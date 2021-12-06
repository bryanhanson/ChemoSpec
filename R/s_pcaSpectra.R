#' Sparse PCA of Spectra Objects
#'
#' A wrapper which carries out sparse PCA analysis on a
#' \code{\link{Spectra}} object.  The user can select various options for
#' scaling.  There is no normalization by rows - do this manually using
#' \code{\link{normSpectra}}. The data will be centered, as is required by PCA.
#'
#' The scale choice \code{autoscale} scales the columns by their standard
#' deviation.  \code{Pareto} scales by the square root of the standard
#' deviation.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param choice A character string indicating the choice of scaling.  One of
#' \code{c("noscale"}, \code{"autoscale"}, \code{"Pareto")}. \code{"autoscale"}
#' is called "standard normal variate" or "correlation matrix PCA" in some literature.
#'
#' @param K Integer. The number of components desired.
#'
#' @param para A vector of \code{length(K)} giving the tuning parameters.
#'
#' @param ... Other parameters to be passed to \code{\link[elasticnet]{arrayspc}}.
#'
#' @return An object of class \code{prcomp} and \code{converted_from_arrayspc},
#' which includes a list
#' element called \code{$method}, a character string describing the
#' pre-processing carried out and the type of PCA performed (used to annotate
#' plots).  A check is carried out to see if the computation was successful
#' and a warning issued if it failed.
#'
#' @template authors-BH
#'
#' @seealso \code{\link[elasticnet]{arrayspc}} for the underlying function,
#' \code{\link{c_pcaSpectra}} for classical PCA calculations,
#' \code{\link{r_pcaSpectra}} for robust PCA calculations,
#' \code{\link{irlba_pcaSpectra}} for PCA via the IRLBA algorithm.
#' Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @template results-links
#'
#' @references H. Zou, T. Hastie and R. Tibshirani "Sparse Principal Components Analysis"
#' \emph{J. Comp. Stat. Graphics} vol. 15 no. 2 pgs. 265-286 (2006).
#'
#' @keywords multivariate
#'
#' @examples
#' \dontrun{
#' # This example assumes the graphics output is set to ggplot2 (see ?GraphicsOptions).
#' library("ggplot2")
#' data(SrE.NMR)
#' pca <- s_pcaSpectra(SrE.NMR)
#'
#' p1 <- plotScree(pca)
#' p1
#'
#' p2 <- plotScores(SrE.NMR, pca, pcs = c(1, 2), ellipse = "cls", tol = 0.05)
#' p2 <- p2 + ggtitle("Scores: SrE NMR Data")
#' p2
#'
#' p3 <- plotLoadings(SrE.NMR, pca, loads = 1:2, ref = 1)
#' p3 <- p3 + ggtitle("Loadings: SrE NMR Data")
#' p3
#' }
#'
#' @export s_pcaSpectra
#' @importFrom stats sd
#'
s_pcaSpectra <- function(spectra, choice = "noscale", K = 3, para = rep(0.5, K), ...) {
  if (!requireNamespace("elasticnet", quietly = TRUE)) {
    stop("You need to install package elasticnet to use this function")
  }

  .chkArgs(mode = 11L)
  chkSpectra(spectra)

  choices <- c("noscale", "autoscale", "Pareto") # trap for invalid scaling method
  check <- choice %in% choices
  if (!check) stop("The choice of scaling parameter was invalid")

  # elasticnet::arrayspc does its own scaling, so we must plan for that
  # We must save a copy of the scaled data to use in reconstructing the scores

  if (choice == "noscale") {
    X <- scale(spectra$data, center = TRUE, scale = FALSE)
    pca <- elasticnet::arrayspc(x = spectra$data, K = K, para = para, use.corr = FALSE, ...)
  }

  if (choice == "Pareto") {
    col.sd <- apply(spectra$data, 2, sd)
    X <- scale(spectra$data, center = TRUE, scale = col.sd^0.5)
    pca <- elasticnet::arrayspc(x = spectra$data, K = K, para = para, use.corr = col.sd^0.5, ...)
  }

  if (choice == "autoscale") {
    col.sd <- apply(spectra$data, 2, sd)
    X <- scale(spectra$data, center = TRUE, scale = col.sd)
    pca <- elasticnet::arrayspc(x = spectra$data, K = K, para = para, use.corr = col.sd, ...)
  }

  # Modify the arrayspc class to conform to prcomp
  pca$method <- paste("centered/", choice, "/", "sparse", sep = "")
  pca$x <- X %*% pca$loadings # compute scores
  pca$rotation <- pca$loadings
  pca$loadings <- NULL
  pca$sdev <- NA_real_
  class(pca) <- c("converted_from_arrayspc", "prcomp")
  if (pca$pev[1] == 0.0) {
    warning("It appears the sparse PCA calculation has failed.  Check the results carefully and consider adjusting the tuning parameters")
  }
  pca
}
