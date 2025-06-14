#'
#' Hierarchical Cluster-Based Peak Alignment on a Spectra Object
#'
#' This function is a wrapper to several functions in the \pkg{speaq} package.
#' It implements the CluPA algorithm described in the reference.
#'
#' @param spectra `r .writeDoc_Spectra1()`
#'
#' @param bT Numeric.  The baseline threshold. Defaults to five percent of the
#' range of the data, in \code{spectra$data}.  Passed to
#' \code{detectSpecPeaks}.
#'
#' @param \dots Other arguments to be passed to the underlying functions.
#'
#' @return A modifed \code{\link{Spectra}} object.
#'
#' @author `r .writeDoc_Authors("BH")`
#'
#' @references
#'
#' Vu TN, Valkenborg D, Smets K, Verwaest KA, Dommisse R, Lemiere F, Verschoren
#' A, Goethals B, Laukens K. "An integrated workflow for robust alignment and
#' simplified quantitative analysis of NMR spectrometry data" BMC
#' Bioinformatics vol. 12 pg. 405 (2011).
#'
#' @seealso Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @keywords utilities
#'
#' @examples
#' # This example assumes the graphics output is set to ggplot2 (see ?GraphicsOptions).
#' # You need to install package speaq for this example
#' if (requireNamespace("speaq", quietly = TRUE)) {
#'   library("ggplot2")
#'   data(alignMUD)
#'
#'   p1 <- plotSpectra(alignMUD,
#'     which = 1:20, lab.pos = 4.5, offset = 0.1,
#'     yrange = c(0, 5000), amp = 500
#'   )
#'   p1 <- p1 + ggtitle("Misaligned NMR Spectra") +
#'     coord_cartesian(xlim = c(1.5, 1.8), ylim = c(0, 1900))
#'   p1
#'
#'   aMUD <- clupaSpectra(alignMUD)
#'
#'   p2 <- plotSpectra(aMUD,
#'     which = 1:20, lab.pos = 4.5, offset = 0.1,
#'     yrange = c(0, 5000), amp = 500
#'   )
#'   p2 <- p2 + ggtitle("Aligned NMR Spectra") +
#'     coord_cartesian(xlim = c(1.5, 1.8), ylim = c(0, 1900))
#'   p2
#' }
#'
#' @export clupaSpectra
#'
#'
clupaSpectra <- function(spectra, bT = NULL, ...) {
  .chkArgs(mode = 11L)
  chkSpectra(spectra)

  if (.chkReqPkgs("speaq")) {
    if (is.null(bT)) bT <- 0.05 * diff(range(spectra$data)) + abs(min(spectra$data))
    pL <- speaq::detectSpecPeaks(spectra$data, baselineThresh = bT, ...)
    ref <- speaq::findRef(pL)[[1]]
    spectra$data <- speaq::dohCluster(spectra$data, pL, ref, ...)
    return(spectra)
  }
}
