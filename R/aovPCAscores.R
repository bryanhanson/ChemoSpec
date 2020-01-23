#'
#'
#' Plot ANOVA-PCA Scores from a Spectra Object
#'
#' Uses the results from \code{\link{aov_pcaSpectra}} to conduct PCA and plot
#' the scores.
#' Argument \code{plot} is used to select a matrix from those in \code{LM}.
#' The residual error matrix is then added to the selected matrix before
#' performing PCA.  Use \code{names(LM)} to see which factor is stored in which
#' matrix.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param LM List of matrices created by \code{\link{aov_pcaSpectra}}.
#'
#' @param plot An integer specifying which scores to plot.
#'
#' @param type Either classical ("cls") or robust ("rob"); Results in either
#' \code{\link{c_pcaSpectra}} or \code{\link{r_pcaSpectra}} being called on the
#' \code{\link{Spectra}} object.
#'
#' @param choice The type of scaling to be performed.  See
#' \code{\link{c_pcaSpectra}} and \code{\link{r_pcaSpectra}} for details.
#'
#' @param \dots Additional parameters to be passed to \code{\link{plotScores}}.
#' For example, you can plot confidence ellipses this way.  Note that ellipses
#' are drawn based on the groups in \code{spectra$groups}, but the separation
#' done by \code{aov_pcaSpectra} is based on argument \code{fac}.  These may
#' not correspond, but you can edit \code{spectra$groups} to match if necessary.
#'
#' @return Returns the PCA results, and creates the requested plot.
#'
#' @author Matthew J. Keinsley and Bryan A. Hanson, DePauw University.
#'
#' @seealso The use of this function can be seen in
#' \code{\link{aov_pcaSpectra}}.  See also \code{\link{plotScores}}.
#' Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @references Pinto, Bosc, Nocairi, Barros, and Rutledge. "Using ANOVA-PCA for
#' Discriminant Analysis: ..." Analytica Chimica Acta 629.1-2 (2008): 47-55.
#'
#' Harrington, Vieira, Espinoza, Nien, Romero, and Yergey. "Analysis of
#' Variance--Principal Component Analysis: ..." Analytica Chimica Acta 544.1-2
#' (2005): 118-27.
#'
#' @keywords multivariate htest
#'
#' @export aovPCAscores
#'
aovPCAscores <- function(spectra, LM, plot = 1, type = "class", choice = NULL, ...) {

  # Function to plot Scores of ANOVA-PCA
  # Bryan Hanson and Matt Keinsley
  # DePauw University, June 2011

  # LM is the list of matrices from aov_pcaSpectra

  .chkArgs(mode = 11L)

  if (plot > length(LM)) {
    stop("Error, matrix to be plotted does not exist. Please choose a different plot!")
  }

  chkSpectra(spectra)

  types <- c("class", "rob")
  check <- type %in% types
  if (!check) {
    stop("PCA option invalid")
  }

  spectra$data <- LM[[plot]] + LM$Res.Error

  if (is.null(choice)) choice <- "noscale"
  if (type == "class") so <- c_pcaSpectra(spectra, choice = choice, cent = FALSE)
  if (type == "rob") so <- r_pcaSpectra(spectra, choice = choice)

  plotScores(spectra, so, ...) # at this point, just a typical score plot
  return(so)
}
