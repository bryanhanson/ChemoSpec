#'
#' Display the Spectra in a Spectra Object One at a Time
#'
#' Plots each spectrum in a \code{\link{Spectra}} object one at a time, and
#' waits for a return in the console before plotting the next spectrum.  Use
#' \code{ESC} to get out of the loop.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param \dots Parameters to be passed downstream.
#'
#' @return None. Side effect is a plot.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords hplot
#'
#' @seealso Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @export loopThruSpectra
#'
#' @importFrom grDevices devAskNewPage
#'
#' @examples
#'
#' \dontrun{
#' data(metMUD1)
#' loopThruSpectra(metMUD1)
#' }
#'
loopThruSpectra <- function(spectra, ...) {
  .chkArgs(mode = 11L)

  cat("Press ESC to stop looping through the spectra\n\n")
  ns <- length(spectra$names)
  for (i in 1:ns) {
    tt <- paste(spectra$names[i], " (#", i, " of ", ns, ")", sep = "")
    plotSpectra(spectra, which = i, main = tt, ...)
    devAskNewPage(ask = TRUE)
  }
  devAskNewPage(ask = FALSE)
}
