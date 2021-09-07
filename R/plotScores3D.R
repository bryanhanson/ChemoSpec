#'
#' 3D PCA Score Plot for a Spectra Object
#'
#' Creates a basic 3D plot of PCA scores from the analysis of a
#' \code{\link{Spectra}} object, color coded according the to scheme stored in
#' the object.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param pca An object of class \code{\link{prcomp}}, modified to include a
#' list element called \code{$method}, a character string describing the
#' pre-processing carried out and the type of PCA performed (it appears on the
#' plot).  This is automatically provided if \code{ChemoSpec} functions
#' \code{\link{c_pcaSpectra}} or \code{\link{r_pcaSpectra}} were used to create
#' \code{pca}.
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
#' @param view A list of viewing transformations to be applied to the data.
#' May contain values for x, y and z axes; keep in mind that the order of the
#' transformations is important.  For example, specifying \code{view = list(x =
#' 45, y = 10)} produces a different view than \code{view = list(y = 10, x =
#' 45)}.  The list may be as along as you like - the series of transformations
#' representing an accumulation of tweaks to achieve the desired view.
#'
#' @param tol Quantile to be used to label extreme data points.  Currently not
#' used - need to fix the code!
#'
#' @param use.sym logical; if true, the color scheme is change to black and
#' symbols are used for plotting.
#'
#' @param \dots Other parameters to be passed downstream.
#'
#' @return None.  Side effect is a plot.
#'
#' @template authors-BH
#'
#' @seealso For a 2D plot of the scores, see \code{\link{plotScores}}.  For
#' interactive 3D plots, use \code{\link{plotScoresRGL}}.  Additional documentation
#' at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @keywords multivariate hplot
#'
#' @examples
#'
#' data(metMUD1)
#' pca <- c_pcaSpectra(metMUD1, choice = "noscale")
#' plotScores3D(metMUD1, pca, main = "metMUD1 NMR Spectra")
#'
#' @export plotScores3D
#' @importFrom graphics plot
#' @importFrom grid grid.text gpar
#' @importFrom ChemoSpecUtils sumGroups
#' @importFrom ChemoSpecUtils .getVarExplained
#'
plotScores3D <- function(spectra, pca, pcs = c(1:3), ellipse = TRUE, rob = FALSE,
                         cl = 0.95, frac.pts.used = 0.8,
                         view = list(y = 34, x = 10, z = 0), tol = 0.01, use.sym = FALSE, ...) {
  if (!requireNamespace("lattice", quietly = TRUE)) {
    stop("You need to install package lattice to use this function")
  }

  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("You need to install package grid to use this function")
  }

  .chkArgs(mode = 12L)
  chkSpectra(spectra)
  if (!length(pcs) == 3) stop("Please give exactly 3 PCs to plot")

  x <- pca$x[, pcs[1]] # organize a few things
  y <- pca$x[, pcs[2]]
  z <- pca$x[, pcs[3]]
  pch <- 20
  gr <- sumGroups(spectra)
  # move the original data into a df, add a size parameter
  df <- data.frame(x = x, y = y, z = z, sym = spectra$sym, col = spectra$colors, size = rep(0.5, length(spectra$names)))

  variance <- .getVarExplained(pca)
  x.lab <- paste("PC", pcs[1], " (", format(variance[ pcs[1] ], digits = 2), "%", ")", sep = "")
  y.lab <- paste("PC", pcs[2], " (", format(variance[ pcs[2] ], digits = 2), "%", ")", sep = "")
  z.lab <- paste("PC", pcs[3], " (", format(variance[ pcs[3] ], digits = 2), "%", ")", sep = "")

  for (n in 1:length(gr$group)) { # work through the groups, add ellipses if n > 3
    # note that .makeEllipsoid has further checks for the number of data points

    w <- grep(gr$group[n], spectra$groups)
    if ((gr$no.[n] > 3) && (ellipse)) { # gather the ellipsoid points
      ell <- .makeEllipsoid(pca$x[w, pcs], rob = rob, cl = cl, frac.pts.used = frac.pts.used)
      x <- ell[, 1]
      y <- ell[, 2]
      z <- ell[, 3]
      sym <- rep(gr$symbol[n], 1000)
      col <- rep(gr$color[n], 1000)
      size <- rep(0.05, 1000) # tiny points for a transparent ellipse
      temp <- data.frame(x = x, y = y, z = z, sym = sym, col = col, size = size)
      df <- rbind(df, temp)
    }
  }

  cube.key <- list(
    x = 0.5, y = 0.15, corner = c(0.5, 0.5), columns = length(gr$group),
    text = list(gr$group, col = gr$color, pch = 20)
  )

  if (use.sym) { # need to override a few things
    df$col <- "black"
    pch <- df$sym
    cube.key <- list(
      x = 0.5, y = 0.15, corner = c(0.5, 0.5),
      columns = length(gr$group),
      text = list(gr$group, col = "black"), points = TRUE, pch = gr$sym
    )
  }


  p <- lattice::cloud(z ~ x * y,
    data = df, col = as.character(df$col), cex = df$size, pch = pch,
    xlab = x.lab, ylab = y.lab, zlab = z.lab, ...,
    par.settings = list(
      axis.line = list(col = "transparent"),
      par.xlab.text = list(cex = 0.75),
      par.ylab.text = list(cex = 0.75),
      par.zlab.text = list(cex = 0.75)
    ),
    screen = view, zoom = 0.75,
    key = cube.key,
    panel = function(...) {
      lattice::panel.cloud(...)
    }
  )

  plot(p)
  grid::grid.text(spectra$desc, 0.5, 0.1, gp = grid::gpar(fontsize = 10))
}
