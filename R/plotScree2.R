#'
#' Alternate Style Scree Plot DEPRECATED
#'
#' This function is deprecated.  Please use \code{plotScree(pca, style = 'alt')}
#'
#' @param pca
#' \itemize{
#'   \item An object of class \code{\link{prcomp}}, modified to include a
#' list element called \code{$method}, a character string describing the
#' pre-processing carried out and the type of PCA performed (it appears on the
#' plot).  This is automatically provided if \code{ChemoSpec} functions
#' \code{\link[ChemoSpec]{c_pcaSpectra}} or \code{\link[ChemoSpec]{r_pcaSpectra}}
#' were used to create \code{pca}.
#' }
#'
#' @param \dots Additional parameters to be passed to plotting functions.
#'
#' @return None.  Side effect is a plot.
#
plotScree2 <- function(pca, ...) {
  msg <- "This function is deprecated.  Please use plotScree(pca, style = 'alt')"
}
