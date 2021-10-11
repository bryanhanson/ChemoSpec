#'
#' Plot the Distance Between Spectra and a Reference Spectrum in a Spectra Object
#'
#' This function plots the distance between a reference spectrum and all other
#' spectra in a \code{\link{Spectra}} object.  Distance can be defined in a number of
#' ways (see Arguments).
#'
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param method Character.  Any method acceptable to \code{\link{rowDist}}.
#'
#' @param ref Integer.  The spectrum to be used as a reference.
#'
#' @param labels Logical.  Shall the points be labeled?
#'
#' @template param-graphics-dots
#' @template param-graphics-return2
#'
#' @seealso To compare all spectra simultaneously in a heatmap, see
#' \code{\link[ChemoSpecUtils]{sampleDist}}.  Additional documentation
#' at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @template authors-BH-TG
#'
#' @keywords hplot multivariate
#'
#' @export plotSpectraDist
#'
#' @importFrom graphics plot text
#' @importFrom stats dist
#' @importFrom plyr arrange
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' # This example assumes the graphics output is set to ggplot2 (see ?GraphicsOptions).
#' library("ggplot2")
#' data(SrE.NMR)
#' txt1 <- paste("Distance from", SrE.NMR$names[1])
#' txt2 <- paste("Rank Distance from", SrE.NMR$names[1])
#' p <- plotSpectraDist(SrE.NMR)
#' p <- p + labs(title = txt1, xlab = txt2, ylab = txt2) +
#'          coord_cartesian(ylim = c(0, 1.1), xlim = c(0, 16))
#' p
#'
plotSpectraDist <- function(spectra,
                            method = "pearson",
                            ref = 1,
                            labels = TRUE,
                            ...) {
  .chkArgs(mode = 11L)
  chkSpectra(spectra)

  DM <- as.matrix(rowDist(spectra$data, method = method))
  dimnames(DM) <- list(spectra$names, spectra$names)
  d <- DM[, ref]
  d <- d[-ref]
  newcols <- spectra$colors[-ref]
  newnames <- spectra$names[-ref]
  DF <- data.frame(label = newnames, col = newcols, y = d, stringsAsFactors = FALSE)
  DF <- arrange(DF, y)
  DF$x <- 1:nrow(DF)
  go <- chkGraphicsOpt()

  if (go == "base") {

    if (labels) {
      plot(x = DF$x, y = DF$y, type = "p", col = DF$col, pch = 20, ...)
      text(x = DF$x, y = DF$y, labels = DF$label, cex = 0.5, adj = c(0, 0), ...)
    }

    if (!labels) plot(x = DF$x, y = DF$y, type = "p", col = DF$col, pch = 20, ...)
    return(DF)
  }

  if ((go == "ggplot2") || (go == "plotly")) {
    x <- y <- label <- NULL # satisfy CRAN check complaints
    .chkReqGraphicsPkgs("ggplot2")

    p <- ggplot(DF, aes(x = x, y = y)) +
      theme_bw() +
      geom_point(color = DF$col) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme(axis.title = element_blank())

    if (go == "ggplot2") {
      if (labels) {
        # p <- p + geom_text_repel(aes(label = label), size = 3)
        p <- p + .ggRepel(DF)
      }
      return(p)
    } else {
      .chkReqGraphicsPkgs("plotly")
      p <- ggplotly(p, tooltip = "label")
      p <- p %>% add_annotations(
        x = DF$x, y = DF$y, text = DF$label, xref = "x",
        yref = "y",
        showarrow = TRUE,
        arrowhead = 4,
        arrowsize = .5,
        ax = 0,
        ay = -15,
        font = list(
          size = 10
        )
      )

      return(p)
    }
  } # end of go = "ggplot2"
}
