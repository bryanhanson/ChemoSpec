#'
#' Display the Spectra in a Spectra Object One at a Time DEPRECATED
#'
#' DEPRECATED: Use \code{reviewAllSpectra} instead.  Plots each spectrum in a
#' \code{\link{Spectra}} object one at a time, and
#' waits for a return in the console before plotting the next spectrum.  Use
#' \code{ESC} to get out of the loop.
#'
#' The ggplot2 mode plots all the \code{\link{Spectra}} objects in the dataset on a single column.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param \dots Parameters to be passed downstream.
#'
#' @return
#' The returned value depends on the graphics option selected (see \code{\link{GraphicsOptions}}).
#' \itemize{
#'  \item{base:}{    None.  Side effect is a plot.}
#'  \item{ggplot2:}{    Returns a \code{ggplot2} plot object. The plot can be modified in the usual
#'                      \code{ggplot2} manner.}
#'  }
#'
#'
#' @template authors-BH
#'
#' @keywords hplot
#'
#' @seealso See \code{\link{GraphicsOptions}}
#'          for more information about the graphics options. Additional documentation at
#'          \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @export loopThruSpectra
#'
loopThruSpectra <- function(spectra, ...) {
  stop("loopThruSpectra has been renamed reviewAllSpectra")
}
