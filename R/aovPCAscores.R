#'
#' Plot ANOVA-PCA Scores from a Spectra Object
#'
#' Uses the results from \code{\link{aov_pcaSpectra}} to plot the scores.
#' Argument \code{submat} is used to select PCA results from among those
#' stored in argument \code{PCA}.
#'
#' @param spectra `r .writeDoc_Spectra1()`
#'
#' @param so \emph{List} of pca results created by \code{\link{aov_pcaSpectra}}.
#'
#' @param submat Integer.  Selects list element \code{submat} from \code{PCA}
#'        which is a list of PCA results, each corresponding to the computation
#'        in \code{\link{aov_pcaSpectra}}.
#'
#' @param ellipse A character vector specifying the type of ellipses to be
#' plotted.  One of \code{c("both"}, \code{"none"}, \code{"cls"}, \code{"rob")}.  \code{cls}
#' specifies classical confidence ellipses, \code{rob} specifies robust
#' confidence ellipses.  An ellipse is drawn for each group unless there
#' are three or fewer samples in the group.
#'
#' @param use.sym A logical; if TRUE, the color scheme is set to black and the
#' points plotted with symbols.  Applies only to \code{\link[ChemoSpec]{Spectra}} objects.
#'
#' @param leg.loc Character; if \code{"none"} no legend will be drawn.
#' Otherwise, any string acceptable to \code{\link{legend}}.
#'
#' @param \dots Additional parameters to be passed to \code{\link{plotScores}}.
#' For example, you can plot confidence ellipses this way.  Note that ellipses
#' are drawn based on the groups in \code{spectra$groups}, but the separation
#' done by \code{aov_pcaSpectra} is based on argument \code{fac}.  These may
#' not correspond, but you can edit \code{spectra$groups} to match if necessary.
#'
#' @return `r .writeDoc_GraphicsReturn2()`
#' @param tol `r .writeDoc_Tol()`
#'
#' @author `r .writeDoc_Authors(c("BH", "MK"))`
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
aovPCAscores <- function(spectra, so, submat = 1, ellipse = "none", tol = "none",
                       use.sym = FALSE, leg.loc = "topright", ...) {

  .chkArgs(mode = 11L)
  if (!is.list(so)) stop("Argument 'so' should be a list of PCA results from aov_pcaSpectra")

  if (submat > length(so) ) {
    stop("Error, results to be plotted do not exist. Please choose a different submatrix!")
  }

  chkSpectra(spectra)

  go <- chkGraphicsOpt()

  if (go == "base") {
    so <- so[[submat]] # need to force evaluation here for some reason (do.call is downstream)
    plotScores(spectra, so, ellipse = ellipse, tol = tol, use.sym = use.sym, leg.loc = leg.loc, ...)
    return(NULL)
  }

  if (go == "ggplot2") {
    .chkReqGraphicsPkgs("ggplot2")
    so <- so[[submat]] # need to force evaluation here for some reason (do.call is downstream)
    p <- plotScores(spectra, so, ellipse = ellipse, tol = tol, use.sym = use.sym, leg.loc = leg.loc, ...)
    return(p)
  }
  
  if (go == "plotly") {
    .chkReqGraphicsPkgs("ggplot2")
    .chkReqGraphicsPkgs("plotly")
    so <- so[[submat]] # need to force evaluation here for some reason (do.call is downstream)
    p <- plotScores(spectra, so, ellipse = ellipse, tol = tol, use.sym = use.sym, leg.loc = leg.loc, ...)
    return(p)
  }
}
