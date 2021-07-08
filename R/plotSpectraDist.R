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
#' @param \dots Plot parameters to be passed to the plotting routines.
#'
#' @return A data frame containing .
#'
#' @return
#' The returned value depends on the graphics option selected (see \code{\link{GraphicsOptions}}).
#' \describe{
#'  \item{base:}{A data frame containing the data plotted (sample names, sample
#'               colors, distances).  A plot of the distances is created.}
#'  \item{ggplot2:}{The plot is displayed, and a \code{ggplot2} plot object is returned.
#'                  The plot can be modified in the usual \code{ggplot2} manner.
#'                  If you want the values of the distances, they can be
#'                  had via the base plot option.}
#' }
#'
#' @seealso To compare all spectra simultaneously in a heatmap, see
#' \code{\link[ChemoSpecUtils]{sampleDist}}.  Additional documentation
#' at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @author Bryan A. Hanson, DePauw University,Tejasvi Gupta.
#'
#' @keywords hplot multivariate
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
#' @export plotSpectraDist
#'
#' @importFrom graphics plot text
#' @importFrom stats dist
#' @importFrom plyr arrange
#'
plotSpectraDist <- function(spectra, method = "pearson", ref = 1, labels = TRUE, ...) {
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

  if (go == "ggplot2") {
    name <- NULL # quiet check complaints
    p <- ggplot(DF, aes(x = 1:nrow(DF), y = dist)) +
      theme_bw() +
      geom_point(color = DF$col) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      theme(axis.title = element_blank())

    if (labels) {
      p <- p + geom_text(aes(label = name, angle = 45), nudge_y = 0.01, size = 3)
    }
    # if (!labels) {
      # print(p)
    # }
  } # end of go = "ggplot2"
  return(p)
}
