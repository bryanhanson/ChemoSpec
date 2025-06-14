#'
#' mclust Analysis of a Spectra Object PCA Results
#'
#' This function is a wrapper for the \code{Mclust} function and associated
#' plotting functions.
#'
#' @param spectra `r .writeDoc_Spectra1()`
#'
#' @param pca An object of class \code{\link{prcomp}}.
#'
#' @param pcs An integer vector describing which PCs to use.
#'
#' @param dims A integer vector giving the PCA dimensions to use.
#'
#' @param plot A character string indicating what plot to make.  Options are
#' \code{c("BIC", "proj", "error")}; see \code{Mclust} for details.
#'
#' @param use.sym Logical; if true, the color scheme is changed to black and
#' symbols are used for plotting.
#'
#' @param \dots Other parameters to be passed downstream.
#'
#' @return The \code{Mclust} model is returned invisibly, and a plot is made.
#'
#' @author `r .writeDoc_Authors("BH")`
#'
#' @seealso \code{\link[mclust]{Mclust}} for background on the method.
#' Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @keywords multivariate cluster
#'
#' @examples
#' \dontrun{
#' require("mclust")
#' data(metMUD1)
#' cls <- c_pcaSpectra(metMUD1, choice = "autoscale")
#'
#' p <- plotScores(metMUD1, cls)
#'
#' mclustSpectra(metMUD1, cls, plot = "BIC")
#' mclustSpectra(metMUD1, cls, plot = "proj")
#' mclustSpectra(metMUD1, cls, plot = "errors", truth = metMUD1$groups)
#' }
#'
#' @export mclustSpectra
#' @importFrom graphics title mtext
#'
mclustSpectra <- function(spectra, pca, pcs = c(1:3), dims = c(1, 2),
                          plot = c("BIC", "proj", "errors"), use.sym = FALSE, ...) {
  .chkArgs(mode = 12L)
  chkSpectra(spectra)
  choices <- c("BIC", "proj", "errors")
  check <- plot %in% choices
  if (!check) stop("The choice of plot was invalid")


  if (.chkReqPkgs("mclust")) {
    d <- pca$x[, pcs]
    mod <- mclust::Mclust(d, ...)
    note <- paste("Mclust optimal model: ", mod$modelName, "\n", sep = "")
    my.sym <- letters[1:length(unique(mod$classification))]

    if (plot == "BIC") {
      if (use.sym) plot(mod, d, what = "BIC", colors = "black")
      if (!use.sym) plot(mod, d, what = "BIC")
      sub <- paste(spectra$desc, pca$method, sep = "  ")
      title(sub = sub, cex.sub = 0.75)
      mtext(note, line = -0.5)
    }


    if (plot == "proj") {
      if (!use.sym) {
        .coordProjCS(d,
          dimens = dims, what = "classification",
          classification = mod$classification, parameters = mod$parameters,
          symbols = my.sym
        )
      }
      if (use.sym) {
        .coordProjCS(d,
          dimens = dims, what = "classification",
          classification = mod$classification,
          parameters = mod$parameters, colors = "black"
        )
      }
      sub <- paste(spectra$desc, pca$method, sep = "  ")
      title(sub = sub, cex.sub = 0.75)
      mtext(note, line = -0.5)
    }

    if (plot == "errors") {
      .coordProjCS(d,
        dimens = dims, what = "errors",
        classification = mod$classification, parameters = mod$parameters,
        truth = spectra$groups, symbols = my.sym
      )
      sub <- paste(spectra$desc, pca$method, sep = "  ")
      title(sub = sub, cex.sub = 0.75)
      mtext(note, line = -0.5)
    }
    invisible(mod)
  }
}
