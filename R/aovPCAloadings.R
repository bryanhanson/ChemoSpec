#'
#' Plot aovPCAscores Loadings of a Spectra Object
#'
#' Uses the results from \code{\link{aovPCAscores}} to plot the corresponding
#' loadings.
#'
#' @param spectra `r .writeDoc_Spectra1()`
#'
#' @param PCA List of pca results created by \code{\link{aov_pcaSpectra}}.
#'
#' @param submat Integer.  Selects list element \code{submat} from \code{PCA}
#'        which is a list of PCA results, each corresponding to the computation
#'        in \code{\link{aov_pcaSpectra}}.
#'
#' @param loads An integer vector giving the loadings to plot.
#'
#' @param ref An integer specifying the reference spectrum to plot, which
#'        appears at the bottom of the plot.
#'
#' @param \dots `r .writeDoc_GraphicsDots()`
#' @return `r .writeDoc_GraphicsReturn()`
#'
#' @author `r .writeDoc_Authors(c("BH", "MK"))`
#'
#' @seealso An example using this function can be seen in
#' \code{\link{aov_pcaSpectra}}.  See also \code{\link{plotLoadings}}.
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
#' @export aovPCAloadings
#'
aovPCAloadings <- function(spectra, PCA, submat = 1, loads = 1, ref = 1, ...) {

  .chkArgs(mode = 11L)
  chkSpectra(spectra)

  if (submat > length(PCA) ) {
    stop("Error, results to be plotted does not exist. Please choose a different submatrix!")
  }


  ##  Creation of titles for each graph depending on the number of factors and which graph was specified

  if (length(PCA) == 3) {
    if (submat == 1) title <- names(PCA)[1]
    if (submat == 2) title <- names(PCA)[2]
    if (submat == 3) title <- names(PCA)[3]
  }

  if (length(PCA) == 5) {
    if (submat == 1) title <- names(PCA)[1]
    if (submat == 2) title <- names(PCA)[2]
    if (submat == 3) title <- names(PCA)[3]
    if (submat == 4) title <- names(PCA)[4]
    if (submat == 5) title <- names(PCA)[5]
  }

  if (length(PCA) == 8) {
    if (submat == 1) title <- names(PCA)[1]
    if (submat == 2) title <- names(PCA)[2]
    if (submat == 3) title <- names(PCA)[3]
    if (submat == 4) title <- names(PCA)[4]
    if (submat == 5) title <- names(PCA)[5]
    if (submat == 6) title <- names(PCA)[6]
    if (submat == 7) title <- names(PCA)[7]
    if (submat == 8) title <- names(PCA)[8]
  }

  go <- chkGraphicsOpt()

  if (go == "base") {
    plotLoadings(spectra = spectra, pca = PCA[[submat]], title = title, loads = loads, ref = ref, ...)
    invisible(NULL)
  }

  if (go == "ggplot2") {
    .chkReqGraphicsPkgs("ggplot2")
    .chkReqGraphicsPkgs("patchwork")
    p <- plotLoadings(spectra = spectra, pca = PCA[[submat]], loads = loads, ref = ref)
    return(p)
  }

}
