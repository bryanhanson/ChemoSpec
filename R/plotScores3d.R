#'
#' 3D PCA Score Plot for a Spectra Object
#'
#' Creates an interactive 3D plot of PCA scores from the analysis of a
#' \code{\link{Spectra}} object, color coded according the to scheme stored in
#' the object.
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
#' @return None.  Side effect is a plot.
#'
#' @template authors-BH
#'
#' @seealso Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @keywords multivariate hplot
#'
#' @examples
#'
#' data(metMUD1)
#' pca <- c_pcaSpectra(metMUD1, choice = "noscale")
#' plotScores3D(metMUD1, pca, main = "metMUD1 NMR Spectra")
#' @export plotScores3d
#' @importFrom ChemoSpecUtils sumGroups
#' @importFrom ChemoSpecUtils .getVarExplained
#'
plotScores3d <- function(spectra, pca, pcs = c(1:3), ellipse = TRUE, rob = FALSE,
                         cl = 0.95, frac.pts.used = 0.8) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("You need to install package lattice to use this function")
  }

  .chkArgs(mode = 12L)
  chkSpectra(spectra)
  if (length(pcs) != 3) stop("Please give exactly 3 PCs to plot")

  x <- pca$x[, pcs[1]] # create a data frame with the scores
  y <- pca$x[, pcs[2]]
  z <- pca$x[, pcs[3]]
  DF1 <- data.frame(x = x, y = y, z = z, col = spectra$colors)

  variance <- .getVarExplained(pca)
  x.lab <- paste("PC", pcs[1], " (", format(variance[pcs[1]], digits = 2), "%", ")", sep = "")
  y.lab <- paste("PC", pcs[2], " (", format(variance[pcs[2]], digits = 2), "%", ")", sep = "")
  z.lab <- paste("PC", pcs[3], " (", format(variance[pcs[3]], digits = 2), "%", ")", sep = "")

  gr <- sumGroups(spectra) # create a data frame for the ellipses
  DF2 <- data.frame(x = NA_real_, y = NA_real_, z = NA_real_, col = NA_character_)
  for (n in 1:length(gr$group)) { # work through the groups, add ellipses if n > 3
    # note that .makeEllipsoid has further checks for the number of data points
    w <- grep(gr$group[n], spectra$groups)
    if ((gr$no.[n] > 3) && (ellipse)) { # gather the ellipsoid points
      ell <- .makeEllipsoid(pca$x[w, pcs], rob = rob, cl = cl, frac.pts.used = frac.pts.used)
      x <- ell[, 1]
      y <- ell[, 2]
      z <- ell[, 3]
      col <- rep(gr$color[n], 1000)
      tmp <- data.frame(x = x, y = y, z = z, col = col)
      DF2 <- rbind(DF2, tmp)
    }
  }
  DF2 <- na.omit(DF2)

  zlw <- 4L # zero line width
  dps <- 3.0 # data point size
  eps <- 0.5 # ellipse point size

  fig <- plot_ly(
    name = "scores", DF1, x = ~x, y = ~y, z = ~z,
    marker = list(size = dps, color = DF1$col)) %>%
    add_markers() %>%
    add_trace(name = "ellipses", data = DF2, x = ~x, y = ~y, z = ~z,
      mode = "points", type = "scatter3d", inherit = FALSE,
      marker = list(size = eps, color = DF2$col)) %>%
    layout(scene = list(
      xaxis = list(title = x.lab, zerolinewidth = zlw),
      yaxis = list(title = y.lab, zerolinewidth = zlw),
      zaxis = list(title = z.lab, zerolinewidth = zlw)
    ))
  fig
}
