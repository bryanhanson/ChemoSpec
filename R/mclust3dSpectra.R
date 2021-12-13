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
#' @param spectra An object of S3 class \code{\link{Spectra}}.
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
#' @template authors-BH
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
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("You need to install package plotly to use this function")
  }

  .chkArgs(mode = 12L)
  chkSpectra(spectra)
  if (length(pcs) != 3) stop("Please give exactly 3 PCs to plot")

  data <- pca$x[, pcs]
  colnames(data) <- c("x", "y", "z")
  mod <- mclust::Mclust(data, ...)
  gr <- unique(mod$classification)

  my.col <- ChemoSpecUtils::Col12[1:length(gr)]

  DF1 <- DF2 <- data.frame(x = NA_real_, y = NA_real_, z = NA_real_,
                           col = NA_character_, gr = NA_character_)

  # Create a data frame of the original points as grouped by Mclust
  for (n in 1:length(gr)) {
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
      clust <- rep(paste("Ellipse", n, sep = " "), 1000)
      temp <- data.frame(x = x, y = y, z = z, col = col, gr = clust)
      DF2 <- rbind(DF2, temp)
    }
  }

  DF1 <- DF1[-1, ]
  if (ellipse) DF2 <- DF2[-1, ]

  # code to set up axes centered on 0,0,0 (not currently used)
  # a <- range(DF1$x, DF1$y, DF1$z)
  # b <- abs(a[1])
  # d <- abs(a[2])
  # ax.len <- max(a, b)
  # x.cor <- c(0, ax.len, 0, 0)
  # y.cor <- c(0, 0, ax.len, 0)
  # z.cor <- c(0, 0, 0, ax.len)
  # i <- c(1, 2, 1, 3, 1, 4)

  variance <- .getVarExplained(pca)
  x.lab <- paste("PC", pcs[1], " (", format(variance[pcs[1]], digits = 2), "%", ")", sep = "")
  y.lab <- paste("PC", pcs[2], " (", format(variance[pcs[2]], digits = 2), "%", ")", sep = "")
  z.lab <- paste("PC", pcs[3], " (", format(variance[pcs[3]], digits = 2), "%", ")", sep = "")

  X <- FALSE
  if (!is.null(truth)) { # X out errors in classification
    ans <- mclust::classError(mod$classification, truth)
    wrong <- as.data.frame(data[ans$misclassified, ])
    if (nrow(wrong) == 0) warning("No points were misclassified, damn you're good!")
    if (nrow(wrong) > 0) X <- TRUE
  }

  score_names <- unique(DF1$gr)
  ellipse_names <- unique(DF2$gr)
  zlw <- 4L # zero line width
  dps <- 3.0 # data point size
  eps <- 0.5 # ellipse point size

  fig <- plot_ly()

  for (n in 1:length(gr)) { # draw scores
    DF1a <- DF1[DF1$gr == score_names[n],]
    fig <- fig %>% 
    add_trace(name = score_names[n], data = DF1a,
      x = ~x, y = ~y, z = ~z,
      mode = "markers", type = "scatter3d", inherit = FALSE,
      marker = list(size = dps, color = DF1a$col))
  }
 
  for (n in 1:length(gr)) { # add ellipses
    DF2a <- DF2[DF2$gr == ellipse_names[n],]
    fig <- fig %>% 
    add_trace(
      name = ellipse_names[n], data = DF2a,
      x = ~x, y = ~y, z = ~z,
      mode = "markers", type = "scatter3d", inherit = FALSE,
      marker = list(size = eps, color = DF2a$col)) 
  }

  if (X) { # mark mis-classified data points
    fig <- fig %>% add_trace(
      name = "mis-classified",
      data = wrong, x = ~x, y = ~y, z = ~z,
      mode = "markers", type = "scatter3d", inherit = FALSE,
      marker = list(size = 2, color = "black", symbol = "x"))
  }

  fig <- fig %>% layout(
      legend= list(itemsizing='constant'),
    title = paste("\n", spectra$desc, "\n", pca$method, sep = ""),
    scene = list(
      xaxis = list(title = x.lab, zerolinewidth = zlw),
      yaxis = list(title = y.lab, zerolinewidth = zlw),
      zaxis = list(title = z.lab, zerolinewidth = zlw)))

  print(fig)

  invisible(mod)
}
