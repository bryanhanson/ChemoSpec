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
#' @template graphics-dots-arg
#' @template graphics-return2-arg
#'
#' @seealso To compare all spectra simultaneously in a heatmap, see
#' \code{\link[ChemoSpecUtils]{sampleDist}}.  Additional documentation
#' at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @author Bryan A. Hanson, DePauw University, Tejasvi Gupta.
#'
#' @keywords hplot multivariate
#'
#' @export plotSpectraDist
#'
#' @importFrom graphics plot text
#' @importFrom stats dist
#' @importFrom plyr arrange
#' @importFrom ggrepel geom_text_repel
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' data(SrE.NMR)
#' txt1 <- paste("Distance from", SrE.NMR$names[1]) # capture before padding
#' txt2 <- paste("Rank Distance from", SrE.NMR$names[1])
#' SrE.NMR$names <- paste("  ", SrE.NMR$names, sep = "") # pad the names for better appearance
#' temp <- plotSpectraDist(SrE.NMR,
#'   xlab = txt2, ylab = txt1, main = txt1,
#'   ylim = c(0, 1.1), xlim = c(0, 16), srt = 45
#' )
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
  DF <- data.frame(name = newnames, col = newcols, dist = d, stringsAsFactors = FALSE)
  DF <- arrange(DF, dist)
  go <- chkGraphicsOpt()

  if (go == "base") {

    if (labels) {
      plot(x = 1:nrow(DF), y = DF$dist, type = "p", col = DF$col, pch = 20, ...)
      text(x = 1:nrow(DF), y = DF$dist, labels = DF$name, cex = 0.5, adj = c(0, 0), ...)
    }

    if (!labels) plot(x = 1:nrow(DF), y = DF$dist, type = "p", col = DF$col, pch = 20, ...)
    return(DF)
  }

  if ((go == "ggplot2") || (go == "plotly")) {
    name <- NULL # satisfy CRAN check complaints
    chkReqGraphicsPkgs("ggplot2")

    p <- ggplot(DF, aes(x = 1:nrow(DF), y = dist)) +
      theme_bw() +
      geom_point(color = DF$col) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme(axis.title = element_blank())

    if (go == "ggplot2") {
      if (labels) {
        p <- p + geom_text_repel(aes(label = name), size = 3)
      }
      return(p)
    } else {
      chkReqGraphicsPkgs("plotly")
      p <- ggplotly(p, tooltip = "name")
      p <- p %>% add_annotations(
        x = 1:nrow(DF), y = DF$dist, text = DF$name, xref = "x",
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
