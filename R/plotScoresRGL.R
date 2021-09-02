#'
#' Interactive 3D Score Plot of a Spectra Object
#'
#' This function uses the \code{\link[rgl]{rgl}} package to create an interactive
#' plot of PCA scores derived from a \code{\link{Spectra}} object.  A title and
#' legend can be added if desired.  Classical or robust confidence ellipses may
#' be added if desired.
#'
#' If you intend to make a hard copy of your plot, use \code{lab.opts = TRUE}
#' until you have found a good view of your data.  Then note corners of the
#' cube where the title and legend won't interfere with viewing the data, and
#' use these as arguments for \code{t.pos} and \code{leg.pos}, and add
#' \code{title}.  Adjust as necessary, then turn off label display using
#' \code{lab.opts = FALSE}.  Back at the console, use \code{>
#' rgl.snapshot("file_name.png")} to create the hardcopy.
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
#' @param title A character string for the plot title.
#'
#' @param t.pos A character selection from \code{LETTERS[1:8]} ( = A through H)
#' indicating the desired location for the title.
#'
#' @param leg.pos A character selection from \code{LETTERS[1:8]} ( = A through
#' H) indicating the desired location for the legend.
#'
#' @param lab.opts A logical indicating whether or not to display the locations
#' where the title and legend can be placed.  These locations are the corners
#' of a cube surrounding the data.
#'
#' @param tol Quantile to be used to label extreme data points.
#'
#' @param use.sym logical; if true, the color scheme is changed to black and
#' symbols are used for plotting.
#'
#' @param axes character; One of \code{"fixed"} or \code{"float"}.  For \code{"fixed"}, reference axes
#'        are drawn along the positive x, y and z axes.  The length of the axes is the maximum of the
#'        the data values, so that all data points are inside the reference axes if positive.  For
#'        \code{"float"} the reference axes are drawn along the positive x, y and z axes, but the
#'        length of each axis corresponds to maximum for each dimension separately.  This option
#'        may make better use of the drawing space.
#'
#' @param \dots Additional parameters to pass downstream, generally to the
#' plotting routines.
#'
#' @return None.  Side effect is a plot
#'
#' @template authors-BH
#'
#' @seealso Other functions in \code{ChemoSpec} that plot PCA scores are:
#' \code{\link{plotScores}} (2D version), and \code{\link{plotScores3D}} (uses
#' \code{lattice} graphics).  Additional documentation at
#' \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#'
#' @keywords multivariate hplot dynamic
#'
#' @examples
#'
#' \dontrun{
#' data(metMUD1)
#' pca <- c_pcaSpectra(metMUD1, choice = "autoscale")
#' plotScoresRGL(metMUD1, pca,
#'   title = "metMUD1 NMR Spectra",
#'   leg.pos = "A", t.pos = "B"
#' )
#' }
#'
#' @export plotScoresRGL
#' @importFrom ChemoSpecUtils sumGroups
#' @importFrom ChemoSpecUtils .getVarExplained
#'
plotScoresRGL <- function(spectra, pca, pcs = c(1:3),
                          ellipse = TRUE, rob = FALSE, cl = 0.95, frac.pts.used = 0.8,
                          title = NULL, t.pos = NULL, leg.pos = NULL, lab.opts = FALSE,
                          tol = 0.01, use.sym = FALSE, axes = "fixed", ...) {
  .chkArgs(mode = 12L)
  chkSpectra(spectra)
  if (!length(pcs) == 3) stop("Please give exactly 3 PCs to plot")

  x <- pca$x[, pcs[1]]
  y <- pca$x[, pcs[2]]
  z <- pca$x[, pcs[3]]
  colors <- spectra$colors
  lets <- spectra$alt.sym
  gr <- sumGroups(spectra)

  variance <- .getVarExplained(pca)
  x.lab <- paste("PC", pcs[1], " (", format(variance[ pcs[1] ], digits = 2), "%", ")", sep = "")
  y.lab <- paste("PC", pcs[2], " (", format(variance[ pcs[2] ], digits = 2), "%", ")", sep = "")
  z.lab <- paste("PC", pcs[3], " (", format(variance[ pcs[3] ], digits = 2), "%", ")", sep = "")

  # The following info is used to position legend and title, and maybe to set axis lengths
  a <- range(x, y, z)
  b <- abs(a[1])
  d <- abs(a[2])
  ax.len <- max(a, b)

  if (axes == "fixed") {
    x.cor <- c(0, ax.len, 0, 0)
    y.cor <- c(0, 0, ax.len, 0)
    z.cor <- c(0, 0, 0, ax.len)
  }

  if (axes == "float") {
    x.cor <- c(0, abs(max(x)), 0, 0)
    y.cor <- c(0, 0, abs(max(y)), 0)
    z.cor <- c(0, 0, 0, abs(max(x)))
  }

  i <- c(1, 2, 1, 3, 1, 4)

  rgl::open3d() # draw axes and label them
  rgl::segments3d(x.cor[i], y.cor[i], z.cor[i],
    lwd = 2.0,
    line_antialias = TRUE
  )
  rgl::text3d(x.cor * 1.1, y.cor * 1.1, z.cor * 1.1,
    texts = c("", x.lab, y.lab, z.lab), adj = c(0, 1)
  )
  if (!use.sym) rgl::points3d(x, y, z, col = colors, size = 4, point_antialias = TRUE)
  if (use.sym) rgl::text3d(x, y, z, texts = lets) # draw data points

  if (tol > 0) {
    pts <- .labelExtremes3d(pca$x[, pcs], names = spectra$names, tol = tol)
    rgl::text3d(pts[, 1], pts[, 2], pts[, 3], texts = pts[, 4], cex = 0.75, adj = c(0, 0))
  }

  if (ellipse) { # compute and draw ellipsoids
    for (n in 1:length(gr$group)) {
      if (gr$no.[n] > 3) {
        d <- pca$x[, pcs]
        w <- grep(gr$group[n], spectra$groups)
        d <- d[w, ]
        ell <- .makeEllipsoid(d, rob = rob, cl = cl, frac.pts.used = frac.pts.used, ...)
        x <- ell[, 1]
        y <- ell[, 2]
        z <- ell[, 3]
        col <- rep(gr$color[n], 1000)
        a <- rep(0.1, 1000)
        if (!use.sym) rgl::points3d(x, y, z, col = col, size = 4, alpha = a, point_antialias = TRUE)
        if (use.sym) rgl::points3d(x, y, z, col = "black", size = 4, alpha = a, point_antialias = TRUE)
      }
    }
  }

  pos <- matrix(NA, 8, 3) # matrix of label positions
  pos[1:4, 1] <- ax.len # x values
  pos[5:8, 1] <- -ax.len
  pos[c(1, 2, 7, 8), 2] <- ax.len # y values
  pos[3:6, 2] <- -ax.len
  pos[seq(1, 8, 2), 3] <- ax.len # z values
  pos[seq(2, 8, 2), 3] <- -ax.len

  if (lab.opts) { # label the 8 cube corners
    labs <- LETTERS[1:8]
    if (!use.sym) rgl::text3d(pos, texts = labs, col = "orange")
    if (use.sym) rgl::text3d(pos, texts = labs, col = "black")
  }

  if ((!is.null(title)) && (!is.null(t.pos))) { # add title
    m <- match(t.pos, LETTERS[1:8])
    rgl::text3d(pos[m, ], texts = title, adj = c(0.5, 0.5), cex = 1.5)
  }

  if (!is.null(leg.pos)) { # add legend
    m <- match(leg.pos, LETTERS[1:8])
    h <- length(gr$group)

    sop <- matrix(NA, h, 3) # offset labels into 2 cols
    sop[, 2] <- pos[m, 2] # keep y coord constant
    amt <- 0.2 * ax.len
    od <- seq(1, h, 2)
    for (n in od) sop[n, 1] <- pos[m, 1] - amt
    if (h > 1) {
      ev <- seq(2, h, 2) # shift every other row l/r
      for (n in ev) sop[n, 1] <- pos[m, 1] + amt
    }

    # fix z coord - a bit trickier!
    r <- ceiling(h / 2) # number of rows
    z.off <- seq(-0.5 * (r - 1), 0.5 * (r - 1), 1)
    z.off <- sort(rep(z.off, 2), decreasing = TRUE)
    z.off <- z.off[1:h] # truncate so dim's match if h odd
    z.off <- z.off * amt + pos[m, 3] # center around specified point
    sop[, 3] <- z.off
    if (!use.sym) {
      rgl::text3d(sop,
        texts = gr$group,
        adj = c(0.5, 0.5), col = gr$color
      )
    }

    if (use.sym) {
      ss <- rbind(sop, sop) # double sop for label locs
      od <- seq(h + 1, h * 2, 2) # shift every other row l/r
      for (n in od) ss[n, 1] <- ss[n, 1] - amt
      if (h > 1) {
        ev <- seq(h + 2, h * 2, 2) # push loc symbol farther out
        for (n in ev) ss[n, 1] <- ss[n, 1] + amt
      }
      rgl::text3d(ss,
        texts = c(gr$group, gr$alt.sym),
        adj = c(0.5, 0.5), col = "black"
      )
    }
  }
}
