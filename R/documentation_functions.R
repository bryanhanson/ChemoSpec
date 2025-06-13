#
# Functions to assist with roxygen2 documentation
#
# Not exported or documented
#
#' @author `r .writeDoc_Authors("BH")`
#'
#' @noRd
.writeDoc_Authors <- function(vec) {
  authors <- NA_character_
  if ("BH" %in% vec) authors <- c(authors, "Bryan A. Hanson (DePauw University)")
  if ("TG" %in% vec) authors <- c(authors, "Tejasvi Gupta")
  if ("MK" %in% vec) authors <- c(authors, "Matthew J. Keinsley")
  authors <- na.omit(authors)
  authors <- paste(authors, collapse = ", ")
  authors <- paste(authors, ".", sep = "")
  authors
}

#' @noRd
.writeDoc_GraphicsDots <- function() {
  "Parameters to be passed to the plotting routines. *Applies to base graphics only*."
}

#' @noRd
.writeDoc_LegLoc <- function() {
  "Either a list with elements `x` and `y`, or a string like `'topright'`.  Values in a list should be on `[0,1]`, i.e. the lower left of the plot area is `0,0` and the upper right is `1,1`.  Allowed string values are those described in [graphics::legend()] under 'Details'. A value of `'none'` is acceptable as well."
}

#' @noRd
.writeDoc_Tol <- function() {
  "A number describing the fraction of points to be labeled. `tol = 1.0` labels all the points; `tol = 0.05` labels *approximately* the most extreme 5 percent. Set to `'none'` to completely suppress labels. Note that a simple approach based upon quantiles is used, assumes that both x and y are each normally distributed, and treats x and y separately.  Thus, this is not a formal treatment of outliers, just a means of labeling points. Groups are lumped together for the computation."
}

#' @noRd
.writeDoc_GraphicsReturn <- function() {
  "The returned value depends on the graphics option selected (see [ChemoSpecUtils::GraphicsOptions()]).
* `base`: None.  Side effect is a plot.
* `ggplot2`: The plot is displayed, and a `ggplot2` object is returned if the value is assigned.  The plot can be modified in the usual `ggplot2` manner.
"
}

#' @noRd
.writeDoc_GraphicsReturn2 <- function() {
  "The returned value depends on the graphics option selected (see [ChemoSpecUtils::GraphicsOptions()]).
* **base** A data frame or list containing the data plotted.  Assign the value and run `str()` or `names()` on it to see what it contains. Side effect is a plot.
* **ggplot2** The plot is displayed, and a `ggplot2` plot object is returned if the value is assigned. The plot can be modified in the usual `ggplot2` manner. If you want the plotted values, you can access them via the base graphics mode."
}

#' @noRd
.writeDoc_Spectra1 <- function() {
  "An object of S3 class [ChemoSpec::Spectra()]."
}

#' @noRd
.writeDoc_Spectra2 <- function() {
  "An object of S3 class [ChemoSpec2D::Spectra2D()]."
}

#' @noRd
.writeDoc_Spectra3 <- function() {
  "An object of S3 class [ChemoSpec::Spectra()] or [ChemoSpec2D::Spectra2D()]."
}

#' @noRd
.writeDoc_LinksShowPCAResults <- function() {
  "For displaying the results, [ChemoSpecUtils::plotScree()], [ChemoSpecUtils::plotScores()], [plotLoadings()], [plot2Loadings()], [sPlotSpectra()]."
}
