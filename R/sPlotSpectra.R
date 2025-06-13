#'
#' s-Plot of Spectra Data (Post PCA)
#'
#' Produces a scatter plot of the correlation of the
#' variables against their covariance for a chosen principal component.  It
#' allows visual identification of variables driving the separation and thus is
#' a useful adjunct to traditional loading plots.
#'
#' @param spectra `r .writeDoc_Spectra1()`
#'
#' @param pca The result of a pca calculation on \code{\link{Spectra}} (i.e.
#' the output from \code{\link{c_pcaSpectra}} or \code{\link{r_pcaSpectra}}).
#'
#' @param pc An integer specifying the desired pc plot.
#'
#' @param tol `r .writeDoc_Tol()`
#' @param \dots `r .writeDoc_GraphicsDots()`
#' @return `r .writeDoc_GraphicsReturn()`
#' @author `r .writeDoc_Authors(c("BH", "TG", "MK"))`
#'
#' @references Wiklund, Johansson, Sjostrom, Mellerowicz, Edlund, Shockcor,
#' Gottfries, Moritz, and Trygg. "Visualization of GC/TOF-MS-Based
#' Metabololomics Data for Identification of Biochemically Interesting
#' Compounds Usings OPLS Class Models" Analytical Chemistry Vol.80 no.1 pgs.
#' 115-122 (2008).
#'
#' @seealso Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @keywords hplot
#'
#' @export sPlotSpectra
#'
#' @importFrom graphics plot abline legend
#' @importFrom ggplot2 geom_vline
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' # This example assumes the graphics output is set to ggplot2 (see ?GraphicsOptions).
#' library("ggplot2")
#' data(SrE.IR)
#' pca <- c_pcaSpectra(SrE.IR)
#' myt <- expression(bolditalic(Serenoa) ~ bolditalic(repens) ~ bold(IR ~ Spectra))
#' p <- sPlotSpectra(spectra = SrE.IR, pca = pca, pc = 1, tol = 0.001)
#' p <- p + ggtitle(myt)
#' p
#'
sPlotSpectra <- function(spectra,
                         pca,
                         pc = 1,
                         tol = 0.05,
                         ...) {
  ##  Code to produce s-plots from Spectra objects
  ##  as in Wiklund.  Part of ChemoSpec
  ##  Matthew J. Keinsley
  ##  DePauw University, July 2011

  msg <- "This function cannot be used with data from sparse pca"
  if (inherits(pca, "converted_from_arrayspc")) stop(msg)
  .chkArgs(mode = 12L)
  chkSpectra(spectra)
  if (length(pc) != 1) stop("You must choose exactly 1 pc to plot.")

  # Prep the data
  centspec <- scale(spectra$data, scale = FALSE)
  cv <- sdv <- c()

  for (i in 1:ncol(centspec)) {
    tmp <- (pca$x[, pc] %*% centspec[, i])
    cv <- c(cv, tmp)
    dv <- sd(as.vector(centspec[, i]))
    sdv <- c(sdv, dv)
  }

  cv <- cv / (nrow(centspec) - 1)
  crr <- cv / (sdv * pca$sdev[pc])
  ans <- data.frame(freq = spectra$freq, cov = cv, corr = crr)

  go <- chkGraphicsOpt()

  if (go == "base") {
    plot(cv, crr,
      xlab = "covariance", ylab = "correlation",
      pch = 20, ...
    )
    abline(v = 0.0, col = "red")
    abline(h = 0.0, col = "red")
    legend("bottomright", y = NULL, pca$method, bty = "n", cex = 0.75)

    # Next, if requested, we will label the extreme points on both dimensions

    if (is.numeric(tol)) .labelExtremes(ans[, 2:3], spectra$freq, tol)

    return(ans)
  }

  if ((go == "ggplot2") || (go == "plotly")) {
    x <- y <- label <- NULL
    .chkReqGraphicsPkgs("ggplot2")

    p <- ggplot(ans, aes(x = cv, y = crr)) +
      theme_bw() +
      xlab("covariance") +
      ylab("correlation")

    p <- p + geom_point() +
      geom_hline(yintercept = 0, color = "red") +
      geom_vline(xintercept = 0, color = "red")

    p <- p + theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

    p <- p + .ggAnnotate(pca$method, x = 0.05, y = 0.98, just = "left", gp = gpar(fontsize = 10))

    if (go == "ggplot2") {
      if (is.numeric(tol)) {
        CoordList <- .getExtremeCoords(ans[, 2:3], spectra$freq, tol)
        df <- data.frame(x = CoordList$x, y = CoordList$y, label = CoordList$l)
        p <- p + .ggRepel(df)
      }
      return(p)
    } else {
      .chkReqGraphicsPkgs("plotly")
      p <- ggplotly(p)
      if (is.numeric(tol)) {
        CoordList <- .getExtremeCoords(ans[, 2:3], spectra$freq, tol)
        df <- data.frame(x = CoordList$x, y = CoordList$y, label = CoordList$l)
        p <- p %>% add_annotations(
          x = df$x, y = df$y, text = df$label, xref = "x",
          yref = "y",
          showarrow = TRUE,
          arrowhead = 4,
          arrowsize = .5,
          ax = 0,
          ay = -15,
          font = list(
            size = 11
          )
        )
      }
      return(p)
    }
  }
}
