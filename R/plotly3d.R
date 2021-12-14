#'
#' Function to Create a 3D Plot of Scores & Ellipses
#'
#' Plotting function called by either \code{plotScores3d} or \code{mclust3dSpectra}.
#' Not intende to be called by the user.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param L  A list containing a data frame of scores, a data frame of ellipses,
#'        and possibly an mclust model.
#'
#' @param pca An object of class \code{\link{prcomp}}.
#'
#' @param pcs An integer vector describing which PCs to use.
#'
#' @param truth A character vector indicating the known group membership for
#' reach row of the PC scores.  Generally this would be \code{spectra$groups}.
#'
#' @return None.  Side effect is a plot in a browser windwo.
#'
#' @template authors-BH
#'
#' @importFrom plotly add_markers add_trace layout plot_ly
#' @export
#' @noRd
#'
.plotly3d <- function(spectra, pca, L, pcs, truth = NULL) {

  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("You need to install package plotly to use this function")
  }

  DF1 <- L$scores
  DF2 <- L$ellipses

  # code to set up axes centered on 0,0,0 (not currently used)
  # a <- range(DF1$x, DF1$y, DF1$z)
  # b <- abs(a[1])
  # d <- abs(a[2])
  # ax.len <- max(a, b)
  # x.cor <- c(0, ax.len, 0, 0)
  # y.cor <- c(0, 0, ax.len, 0)
  # z.cor <- c(0, 0, 0, ax.len)
  # i <- c(1, 2, 1, 3, 1, 4)

  # If truth provided, extract the needed info from the mclust model
  # so we can X-out the mis-classified points
  X <- FALSE # if TRUE we will plot Xs on the scores that are mis-classified
  if (!is.null(truth)) {
    if (! "model" %in% names(L)) stop("truth was provided, but there was not mclust model")
    ans <- mclust::classError(L$model$classification, truth)
    wrong <- as.data.frame(DF1[ans$misclassified, ])
    if (nrow(wrong) == 0) warning("No points were misclassified")
    if (nrow(wrong) > 0) X <- TRUE
  }

  # Prep for the plot
  variance <- .getVarExplained(pca)
  x.lab <- paste("PC", pcs[1], " (", format(variance[pcs[1]], digits = 2), "%", ")", sep = "")
  y.lab <- paste("PC", pcs[2], " (", format(variance[pcs[2]], digits = 2), "%", ")", sep = "")
  z.lab <- paste("PC", pcs[3], " (", format(variance[pcs[3]], digits = 2), "%", ")", sep = "")

  s_names <- unique(DF1$gr) # names for score traces; becomes legend entry
  e_names <- unique(DF2$gr) # names for ellipse traces; becomes legend entry
  ng <- length(unique(DF1$gr)) # either: number of groups present in original
                               # Spectra object, or number of clusters found by mclust
  ne <- length(unique(DF2$gr)) # number of ellipses that will be drawn

  # Plot settings
  zlw <- 4L # zero line width
  dps <- 3.0 # data point size
  eps <- 0.5 # ellipse point size

  fig <- plot_ly()

  for (n in 1:ng) { # draw scores
    DF1a <- DF1[DF1$gr == s_names[n],]
    fig <- fig %>% 
    add_trace(name = s_names[n], data = DF1a,
      x = ~x, y = ~y, z = ~z,
      mode = "markers", type = "scatter3d", inherit = FALSE,
      marker = list(size = dps, color = DF1a$col))
  }
 
  for (n in 1:ne) { # add ellipses
    DF2a <- DF2[DF2$gr == e_names[n],]
    fig <- fig %>% 
    add_trace(
      name = e_names[n], data = DF2a,
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
}
