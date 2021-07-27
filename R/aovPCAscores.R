#'
#' Plot ANOVA-PCA Scores from a Spectra Object
#'
#' Uses the results from \code{\link{aov_pcaSpectra}} to plot the scores.
#' Argument \code{submat} is used to select PCA results from among those
#' stored in argument \code{PCA}.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param so \emph{List} of pca results created by \code{\link{aov_pcaSpectra}}.
#'
#' @param submat Integer.  Selects list element \code{submat} from \code{PCA}
#'        which is a list of PCA results, each corresponding to the computation
#'        in \code{\link{aov_pcaSpectra}}.
#'
#' @param \dots Additional parameters to be passed to \code{\link{plotScores}}.
#' For example, you can plot confidence ellipses this way.  Note that ellipses
#' are drawn based on the groups in \code{spectra$groups}, but the separation
#' done by \code{aov_pcaSpectra} is based on argument \code{fac}.  These may
#' not correspond, but you can edit \code{spectra$groups} to match if necessary.
#'
#' @template graphics-return2-arg
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
aovPCAscores <- function(spectra, so, submat = 1, ...) {

  .chkArgs(mode = 11L)
  if (!is.list(so)) stop("Argument 'so' should be a list of PCA results from aov_pcaSpectra")

  if (submat > length(so) ) {
    stop("Error, results to be plotted do not exist. Please choose a different submatrix!")
  }

  chkSpectra(spectra)

  go <- chkGraphicsOpt()

  if (go == "base") {
    so <- so[[submat]] # need to force evaluation for some reason (do.call is downstream)
    plotScores(spectra, so, ...)
    return(NULL)
  }

  if (go == "ggplot2") {
    so <- so[[submat]] # need to force evaluation for some reason (do.call is downstream)
    p <- plotScores(spectra, so)
    return(p)
  }

}
