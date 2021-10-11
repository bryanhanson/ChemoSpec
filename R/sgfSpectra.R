#'
#' Apply Savitzky-Golay filters to a Spectra object
#'
#' This function is a simple wrapper around the function
#' \code{\link[signal]{sgolayfilt}}.  It allows one to apply Savitzky-Golay
#' filters to a \code{\link{Spectra}} object in a convenient way.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}} to be checked.
#'
#' @param m The desired m-th derivative.  \code{m = 0} smooths the data (i.ei a rolling
#' average), \code{m = 1} gives the first derivative etc.
#'
#' @param \dots Other parameters to be passed to
#' \code{\link[signal]{sgolayfilt}}.
#'
#' @return A object of class \code{\link{Spectra}}.
#'
#' @template authors-BH
#'
#' @keywords utilities multivariate
#'
#' @seealso Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#' @export sgfSpectra
#' @examples
#'
#' # This example assumes the graphics output is set to ggplot2 (see ?GraphicsOptions).
#' library("ggplot2")
#' library("patchwork")
#' data(SrE.IR)
#' myt1 <- expression(bolditalic(Serenoa) ~ bolditalic(repens) ~ bold(IR ~ Spectra))
#' myt2 <- expression(bolditalic(Serenoa) ~ bolditalic(repens) ~ bold(IR ~ Spectra ~ (Smoothed)))
#'
#' p1 <- plotSpectra(SrE.IR)
#' p1 <- p1 + ggtitle(myt1) + coord_cartesian(xlim = c(1900, 2100), ylim = c(0.0, 0.03))
#'
#' sgf <- sgfSpectra(SrE.IR)
#' p2 <- plotSpectra(sgf)
#' p2 <- p2 + ggtitle(myt2) + coord_cartesian(xlim = c(1900, 2100), ylim = c(0.0, 0.03))
#'
#' p3 <- p1/p2
#' p3
#'
sgfSpectra <- function(spectra, m = 0, ...) {
  if (!requireNamespace("signal", quietly = TRUE)) {
    stop("You need to install package signal to use this function")
  }

  .chkArgs(mode = 11L)
  chkSpectra(spectra)


  for (i in 1:length(spectra$names)) {
    spectra$data[i, ] <- signal::sgolayfilt(spectra$data[i, ], m = m, ...)
  }

  chkSpectra(spectra)
  return(spectra)
}
