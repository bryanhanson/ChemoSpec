#' Classical PCA of Spectra Objects
#'
#' A wrapper which carries out classical PCA analysis on a
#' \code{\link{Spectra}} object.  The user can select various options for
#' scaling.  There is no normalization by rows - do this manually using
#' \code{\link{normSpectra}}.  There is an option to control centering, but
#' this is mainly for compatibility with the \code{\link{aov_pcaSpectra}}
#' series of functions.  Centering the data should always be done in PCA and it
#' is the default here.
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
#' @param cent Logical: whether or not to center the data.  Always center the
#' data unless you know it to be already centered.
#'
#' @return An object of class \code{\link{prcomp}}, modified to include a list
#' element called \code{$method}, a character string describing the
#' pre-processing carried out and the type of PCA performed (used to annotate
#' plots).
#'
#' @template authors-BH
#'
#' @seealso \code{\link{prcomp}} for the underlying function,
#' \code{\link{s_pcaSpectra}} for sparse PCA calculations,
#' \code{\link{r_pcaSpectra}} for robust PCA calculations,
#' \code{\link{irlba_pcaSpectra}} for PCA via the IRLBA algorithm.
#' Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' For displaying the results, \code{\link{plotScree}},
#' \code{\link{plotScores}}, \code{\link{plotLoadings}},
#' \code{\link{plot2Loadings}}, \code{\link{sPlotSpectra}},
#' \code{\link{plotScores3D}}, \code{\link{plotScoresRGL}}.
#'
#' @references K. Varmuza and P. Filzmoser \emph{Introduction to Multivariate
#' Statistical Analysis in Chemometrics}, CRC Press, 2009.
#'
#' @keywords multivariate
#'
#' @examples
#' \dontrun{
#' # This example assumes the graphics output is set to ggplot2 (see ?GraphicsOptions).
#' library("ggplot2")
#' data(metMUD1)
#' pca <- c_pcaSpectra(metMUD1)
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
#'
#' @export c_pcaSpectra
#' @importFrom stats sd prcomp
#'
c_pcaSpectra <- function(spectra, choice = "noscale", cent = TRUE) {
  .chkArgs(mode = 11L)

  choices <- c("noscale", "autoscale", "Pareto") # trap for invalid scaling method
  check <- choice %in% choices
  if (!check) stop("The choice of scaling parameter was invalid")
  chkSpectra(spectra)

  # Center & scale the data using the desired method.

  if (identical(choice, "noscale")) {
    centscaled <- scale(spectra$data, center = cent, scale = FALSE)
  }

  if (identical(choice, "Pareto")) {
    col.sd <- apply(spectra$data, 2, sd)
    centscaled <- scale(spectra$data, center = cent, scale = col.sd^0.5)
  }

  if (identical(choice, "autoscale")) {
    col.sd <- apply(spectra$data, 2, sd)
    centscaled <- scale(spectra$data, center = cent, scale = col.sd)
  }

  # Now the PCA!

  pca <- prcomp(centscaled, retx = TRUE, center = FALSE, scale. = FALSE)
  pca$method <- paste("centered/", choice, "/", "classical", sep = "")

  pca
}
