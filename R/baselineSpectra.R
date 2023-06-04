#'
#'
#' Baseline Correction of a Spectra Object
#'
#' This function mostly wraps functions in package \pkg{baseline} which
#' carries out a variety of baseline correction routines.  A simple linear
#' correction method is also available.
#'
#' In plots using methods from the baseline package, the x axis ticks give the
#' data point index, not the original values from your data. Note that you
#' cannot zoom the non-interactive display of corrected spectra because the
#' underlying function hardwires the display.  Try the interactive version
#' instead (\code{int = TRUE}), or use \code{\link{plotSpectra}} on the
#' corrected data.
#' In addition to the methods provided by \code{baseline}, you can also use
#' \code{method = "linear"}.  This correction is handled locally, and is very
#' simple: a line is drawn from the first data point to the last, and this
#' becomes the new baseline.  This is most suitable for cases in which the
#' baseline rises or falls steadily, as is often seen in chromatograms.
#'
#' @param spectra `r .writeDoc_Spectra1()`
#'
#' @param int Logical; if \code{TRUE}, do the correction interactively using
#' widgets.  No results are saved.  Use this for inspection and exploration
#' only.
#'
#' @param retC Logical: shall the baseline-corrected spectra be returned in the
#' \code{Spectra} object?
#'
#' @param \dots Other arguments passed downstream.  The relevant ones can be
#' found in \code{\link[baseline]{baseline}}.  Be sure to pay attention to
#' argument \code{method} as you will probably want to use it.  You can also
#' use \code{method = "linear"} for a simple linear fit, see Details.
#'
#' @param show Integer. A vector giving the sample numbers for which you wish
#'        to see the results of the baseline correction.  By "sample numbers"
#'        we mean the rows in the \code{spectra$data} matrix.  To find a specific
#'        sample type \code{spectra$names} to see which row contains that sample.
#'        To see all samples, use \code{show = 1:nrow(spectra$data)} where
#'        \code{spectra} is the name of your particular \code{Spectra} object.
#'
#' @return If \code{int = TRUE}, an interactive plot is created.  If \code{int
#' = FALSE} and \code{retC = FALSE}, an object of class \code{baseline} is
#' returned (see \code{\link[baseline]{baseline-class}}).  If \code{int =
#' FALSE} and \code{retC = TRUE}, a \code{Spectra} object containing the
#' corrected spectra is returned.  In these latter two cases plots are also
#' drawn.
#'
#' @author `r .writeDoc_Authors("BH")`
#'
#' @keywords hplot
#' @seealso Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#' @export baselineSpectra
#' @importFrom stats lm predict
#'
#' @examples
#' # You need to install package "baseline" for this example
#' if (requireNamespace("baseline", quietly = TRUE)) {
#'   data(SrE.IR)
#'   temp <- baselineSpectra(SrE.IR, int = FALSE, method = "modpolyfit")
#' }
#'
baselineSpectra <- function(spectra, int = TRUE, retC = FALSE, show = 1, ...) {

  # Mostly a simple wrapper to the excellent baseline package
  # Part of ChemoSpec.  Bryan Hanson December 2011

  # Some clunky checking here to send data either to package baseline
  # or handle it locally, with the linear method.
  # "method" is not a formal argument for compatibility with baseline
  # but we do need it if processing locally.

  .chkArgs(mode = 11L)

  dots <- list(...)
  linear <- FALSE
  if (any(names(dots) == "method")) {
    method <- dots$method
    if (method == "rfbaseline") {
      if (!requireNamespace("IDPmisc", quietly = TRUE)) {
        stop("You need to install package IDPmisc to use method rfbaseline")
      }
    }
    if (method == "linear") linear <- TRUE
  }


  if (linear) { # Process locally and return immediately
    np <- length(spectra$freq)

    # It would probably be better to use the mean of ~50 points
    # at the beginning and end for the fit

    # A fancier version could allow the user to give two ranges
    # where the baseline is just noise, and use those to
    # make the correction.  However, other methods of correction
    # are probably better in that case.

    for (i in 1:length(spectra$names)) {
      rMin <- min(spectra$data[i, ])
      spectra$data[i, ] <- spectra$data[i, ] - rMin
      # Do an lm from end to the other
      DF <- data.frame(
        x = c(spectra$freq[1], spectra$freq[np]),
        y = c(spectra$data[i, 1], spectra$data[i, np])
      )
      fit <- lm(y ~ x, DF)
      spectra$data[i, ] <- spectra$data[i, ] - predict(fit,
        newdata = data.frame(x = spectra$freq)
      )
    }

    chkSpectra(spectra)
    return(spectra)
  }

  # Below here, we are using methods in package baseline

  if (!requireNamespace("baseline", quietly = TRUE)) {
    stop("You need to install package baseline to use this function")
  }

  old.par <- par(no.readonly = TRUE) # interactive baseline makes 2 plots
  on.exit(par(old.par)) # reset when done (suggested by Dana Nadler)

  dat <- spectra$data # possible conflict with baseline's use of spectra
  if (int) baseline::baselineGUI(dat, ...) # no return value
  if (!int) {
    b <- baseline::baseline(dat, ...)
    if (is.integer(show)) {
      cat("Press ESC to stop looping through the spectra\n\n")

      for (i in show) {
        baseline::plot(b, specNo = i)
        mtext(spectra$names[i], side = 4)
        devAskNewPage(ask = TRUE)
      }
      devAskNewPage(ask = FALSE)
    }
    if (retC) {
      bc <- baseline::getCorrected(b) # the way it is supposed to be done...
      # works interactively, but not in vignette ???
      # bc <- b@corrected
      spectra$data <- bc
      chkSpectra(spectra)
      return(spectra)
    }

    if (!retC) {
      return(b)
    }
  }
}
