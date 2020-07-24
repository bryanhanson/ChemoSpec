#'
#' Exploratory Chemometrics for Spectroscopy
#'
#' A collection of functions for top-down exploratory data analysis of spectral
#' data obtained via nuclear magnetic resonance (NMR), infrared (IR) or Raman
#' spectroscopy.  Includes functions for plotting and inspecting spectra, peak
#' alignment, hierarchical cluster analysis (HCA), principal components
#' analysis (PCA) and model-based clustering. Robust methods appropriate for
#' this type of high-dimensional data are available.  ChemoSpec is designed
#' with metabolomics data sets in mind, where the samples fall into groups such
#' as treatment and control.  Graphical output is formatted consistently for
#' publication quality plots.  ChemoSpec is intended to be very user friendly
#' and help you get usable results quickly.  A vignette covering typical
#' operations is available.
#'
#' @name ChemoSpec-package
#'
#' @aliases ChemoSpec-package ChemoSpec
#'
#' @docType package
#'
#' @author Bryan A. Hanson and Matthew J. Keinsley.
#'
#' Maintainer: Bryan A. Hanson \email{hanson@@depauw.edu}
#'
#' @keywords package multivariate
#'
#' @seealso Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#  This import/export business based on stackoverflow.com/a/35118850/633251
#'
#' @importFrom ChemoSpecUtils rowDist hcaScores .shrinkLeaf .colLeaf .plotHCA .chkArgs
#' @export rowDist hcaScores .shrinkLeaf .colLeaf .plotHCA .chkArgs
#'
#' @importFrom ChemoSpecUtils chkSpectra sumSpectra sumGroups check4Gaps
#' @export  chkSpectra sumSpectra sumGroups check4Gaps
#'
#' @importFrom ChemoSpecUtils removeFreq removeGroup removeSample plotScores plotScree
#' @export removeFreq removeGroup removeSample plotScores plotScree
#'
#' @importFrom ChemoSpecUtils .labelExtremes .addEllipseInfo .computeEllipses .addLegend .groupNcolor
#' @export .labelExtremes .addEllipseInfo .computeEllipses .addLegend .groupNcolor
#'
#' @importFrom ChemoSpecUtils sampleDist
#' @export sampleDist
#'
NULL
