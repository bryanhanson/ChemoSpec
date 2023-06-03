#'
#' mclust Analysis of a Spectra Object in 3D
#'
#' This function conducts an mclust analysis of the PCA results of a
#' \code{\link{Spectra}} object and displays the results in 3D.  Classical or
#' robust confidence ellipses can be added if desired. Improperly classified
#' data points can be marked.  The interactive plot is made via \code{plotly}
#' and appears in a browser window. Note that the confidence ellipses computed
#' here are generated independently of the \code{Mclust} results - they do not
#' correspond to the ellipses seen in 2D plots from \code{Mclust}.
#'
#' @param spectra `r .writeDoc_Spectra1()`
#'
#' @param pca An object of class \code{\link{prcomp}}.
#'
#' @param pcs An integer vector describing which PCs to use.
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
#' @param truth A character vector indicating the known group membership for
#' reach row of the PC scores.  Generally this would be \code{spectra$groups}.
#'
#' @param ... Arguments to be passed to \code{mclust}.
#'
#' @return The mclust model is returned invisibly, and a plot is produced.
#'
#' @author `r .writeDoc_Authors("BH")`
#'
#' @seealso \code{\link[mclust]{Mclust}} for background on the method.
#' Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @importFrom plotly add_markers add_trace layout plot_ly
#' @export mclust3dSpectra
#' @keywords multivariate cluster
#'
#' @examples
#' \dontrun{
#' require(mclust)
#' data(metMUD1)
#' pca <- c_pcaSpectra(metMUD1)
#' mclust3dSpectra(metMUD1, pca)
#' 
#' # show mis-classified points
#' mclust3dSpectra(metMUD1, pca, truth = metMUD1$groups)
#' }
#'
#'
mclust3dSpectra <- function(spectra, pca, pcs = 1:3, 
                            ellipse = TRUE, rob = FALSE, cl = 0.95,
                            frac.pts.used = 0.8, truth = NULL, ...) {

  if (!requireNamespace("mclust", quietly = TRUE)) {
    stop("You need to install package mclust to use this function")
  }
  .chkArgs(mode = 12L)
  chkSpectra(spectra)
  if (length(pcs) != 3) stop("Please give exactly 3 PCs to plot")

  data <- pca$x[, pcs]
  colnames(data) <- c("x", "y", "z")
  mod <- mclust::Mclust(data, ...)
  gr <- unique(mod$classification)
  ng <- length(gr)

  my.col <- ChemoSpecUtils::Col12[1:ng]
  if (ng > 12) stop("Not enough colors for the groups. Contact maintainer.")

  DF1 <- DF2 <- data.frame(x = NA_real_, y = NA_real_, z = NA_real_,
                           col = NA_character_, gr = NA_character_)

  # Create a data frame of the original points as grouped by Mclust
  for (n in 1:ng) {
    w <- grep(gr[n], mod$classification)
    x <- data[w, 1]
    y <- data[w, 2]
    z <- data[w, 3]
    col <- rep(my.col[n], length(w))
    clust <- rep(paste("Cluster", n, sep = " "), length(w))
    temp <- data.frame(x = x, y = y, z = z, col = col, gr = clust)
    DF1 <- rbind(DF1, temp)

    # Create a data frame to hold the computed ellipse points
    if ((length(w)) > 3 && (ellipse)) {
      ell <- .makeEllipsoid(data[w, ],
        rob = rob, cl = cl, frac.pts.used = frac.pts.used)
      x <- ell[, 1]
      y <- ell[, 2]
      z <- ell[, 3]
      col <- rep(my.col[n], 1000)
      clust <- rep(paste("Cluster", n, "(ellipse)", sep = " "), 1000)
      temp <- data.frame(x = x, y = y, z = z, col = col, gr = clust)
      DF2 <- rbind(DF2, temp)
    }
  }

  DF1 <- DF1[-1, ] # remove NA in row 1
  if (ellipse) DF2 <- DF2[-1, ]
  L <- list(scores = DF1, ellipses = DF2, model = mod)

  fig <- .plotly3d(spectra, pca, L, pcs, truth) # pass to plotting function

  invisible(mod)
}
