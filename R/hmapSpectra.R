#'
#' Seriated Heat Map for a Spectra Object
#'
#' Creates a heat map with marginal dendrograms using seriation procedures.
#' Heirchical cluster analysis is followed by re-ordering the clusters in a coordinated way
#' across each dimension (controlled by argument \code{method}, see \code{\link[seriation]{hmap}}).
#' The vignette for package \pkg{seriation} has more details.
#'
#' @param spectra `r .writeDoc_Spectra1()`
#'
#' @param \dots Additional arguments to be passed downstream.  A great deal of
#' control is available - check \code{\link[seriation]{hmap}} for details.  Most of the control
#' actually derives from the \code{heatmap} function in package \pkg{stats}.
#' See the examples.
#'
#' @return A list as described in \code{\link[seriation]{hmap}}. Side effect is a plot.
#'
#' @author `r .writeDoc_Authors("BH")`
#'
#' @seealso \code{\link[seriation]{hmap}} which will get you to the package
#' (there is no package index page); the vignette is a good place to begin
#' (\code{browseVignettes("seriation")}).  Additional documentation at
#' \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @section Note:
#' The underlying \code{stats::heatmap} function does certain things automatically for the user.
#' For instance, if you pass a vector of names to argument \code{row_labels} it is automatically
#' reordered using the \code{rowInd} vector that is computed.  The user does not need to
#' do the reordering.  Another example is the labeling of the columns.  The labels are
#' automatically turned 90 degrees, and not every column is labeled to keep things readible.
#'
#' @section Interpretation:
#' Looking at the 2nd \code{hmapSpectra} example, and keeping in mind the nature of the sample
#' (see \code{\link{SrE.IR}}), the most similar samples based on the ester peaks (~1740),  are in
#' the lower right corner of the heatmap.  These are the two outlier samples, composed of triglycerides
#' which contain the ester functional group (and no detectable carboxylic acid).  The most similar
#' samples based on the carboxylic acid peaks (~1710) are in the upper left corner.  These samples are
#' mostly from the "pure" extract group, according to the manufacturer's label.  These samples have a modest
#' to low amount of the ester functional group, which indicates dilution (or adulteration if you like).
#' In fact, the first two samples (NP_adSrE and NR_pSrE) are the two samples with the smallest ester
#' peaks (see first plot in the examples).  This suggests that NP_adSrE was diluted only a little with added
#' olive oil.
#'
#' @keywords multivariate
#'
#' @examples
#' # You need to install package "seriation" for this example
#' if (requireNamespace("seriation", quietly = TRUE)) {
#'   data(SrE.IR)
#'
#'   # Let's look just at the carbonyl region
#'   IR <- removeFreq(SrE.IR, rem.freq = SrE.IR$freq > 1775 | SrE.IR$freq < 1660)
#'   p <- plotSpectra(IR, which = 1:16, lab.pos = 1800)
#'
#'   # Defaults, except for color scheme:
#'   res <- hmapSpectra(IR, col = heat.colors(5))
#'
#'   # Label samples and frequencies by passing arguments to stats:heatmap
#'   # Also make a few other nice plot adjustments
#'   res <- hmapSpectra(IR,
#'     col = heat.colors(5),
#'     row_labels = IR$names, col_labels = as.character(round(IR$freq)),
#'     margins = c(4, 6)
#'     )
#' }
#'
#' @export hmapSpectra
#'
hmapSpectra <- function(spectra, ...) {
  .chkArgs(mode = 11L)
  chkSpectra(spectra)

  if (!requireNamespace("seriation", quietly = TRUE)) {
    stop("You need to install package seriation to use this function")
  }

  res <- seriation::hmap(spectra$data, ...)
  res
}
