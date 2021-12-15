#'
#' 3D PCA Score Plot for a Spectra Object
#'
#' Creates an interactive 3D plot of PCA scores from the analysis of a
#' \code{\link{Spectra}} object, color coded according the to scheme stored in
#' the object.  The plot is created by \code{plotly} and appears in a browser window.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param pca An object of class \code{\link{prcomp}}.
#'
#' @param pcs A vector of three integers specifying the PCA scores to plot.
#'
#' @param ellipse Logical indicating if confidence ellipses should be drawn.
#'
#' @param rob Logical; if \code{ellipse = TRUE}, indicates that robust
#' confidence ellipses should be drawn.  If \code{FALSE}, classical confidence
#' ellipses are drawn.
#'
#' @param cl A number indicating the confidence interval for the ellipse.
#'
#' @param frac.pts.used If \code{ellipse = TRUE} and \code{rob = TRUE}, a
#' number indicating the fraction of the data points to be considered "good"
#' and thus used to compute the robust confidence ellipse.
#'
#' @return None.  Side effect is a plot in a browser window.
#'
#' @template authors-BH
#'
#' @seealso Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @keywords multivariate hplot
#' @export plot3dScores
#' @importFrom ChemoSpecUtils sumGroups
#' @importFrom ChemoSpecUtils .getVarExplained
#' @importFrom plotly add_markers add_trace layout plot_ly
#'
#' @examples
#' if (interactive()) {
#'   data(metMUD1)
#'   pca <- c_pcaSpectra(metMUD1, choice = "noscale")
#'   p <- plot3dScores(metMUD1, pca)
#'   p
#' }
#'
plot3dScores <- function(spectra, pca, pcs = c(1:3), ellipse = TRUE, rob = FALSE,
                         cl = 0.95, frac.pts.used = 0.8) {

  .chkArgs(mode = 12L)
  chkSpectra(spectra)
  if (length(pcs) != 3) stop("Please give exactly 3 PCs to plot")

  x <- pca$x[, pcs[1]] # create a data frame with the scores
  y <- pca$x[, pcs[2]]
  z <- pca$x[, pcs[3]]
  DF1 <- data.frame(x = x, y = y, z = z, col = spectra$colors, gr = spectra$groups)

  gr <- sumGroups(spectra) # create a data frame for the ellipses
  DF2 <- data.frame(x = NA_real_, y = NA_real_, z = NA_real_,
                    col = NA_character_, gr = NA_character_)
  for (n in 1:length(gr$group)) { # work through the groups, add ellipses if n > 3
    # note that .makeEllipsoid has further checks for the number of data points
    w <- grep(gr$group[n], spectra$groups)
    if ((gr$no.[n] > 3) && (ellipse)) { # gather the ellipsoid points
      ell <- .makeEllipsoid(pca$x[w, pcs], rob = rob, cl = cl, frac.pts.used = frac.pts.used)
      x <- ell[, 1]
      y <- ell[, 2]
      z <- ell[, 3]
      col <- rep(gr$color[n], 1000)
      e_name <- paste(gr$group[n], "(ellipse)", sep = " ")
      group <- rep(e_name, 1000)
      tmp <- data.frame(x = x, y = y, z = z, col = col, gr = group)
      DF2 <- rbind(DF2, tmp)
    }
  }
  DF2 <- na.omit(DF2)

  L <- list(scores = DF1, ellipses = DF2)

  .plotly3d(spectra, pca, L, pcs, truth = NULL)
}
