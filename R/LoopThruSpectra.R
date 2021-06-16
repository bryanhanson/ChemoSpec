#'
#' Display the Spectra in a Spectra Object One at a Time
#'
#' Plots each spectrum in a \code{\link{Spectra}} object one at a time, and
#' waits for a return in the console before plotting the next spectrum.  Use
#' \code{ESC} to get out of the loop.
#'
#' The ggplot2 mode plots all the \code{\link{Spectra}} objects in the dataset on a single column.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param \dots Parameters to be passed downstream.
#'
#' @return
#' The returned value depends on the graphics option selected (see \code{\link{GraphicsOptions}}).
#' \itemize{
#'  \item{base:}{    None.  Side effect is a plot.}
#'  \item{ggplot2:}{    Returns a \code{ggplot2} plot object. The plot can be modified in the usual
#'                      \code{ggplot2} manner.}
#'  }
#'
#'
#' @author Bryan A. Hanson, DePauw University, Tejasvi Gupta.
#'
#' @keywords hplot
#'
#' @seealso See \code{\link{GraphicsOptions}}
#'          for more information about the graphics options. Additional documentation at
#'          \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @export loopThruSpectra
#'
#' @importFrom grDevices devAskNewPage
#' @importFrom ggplot2 xlab ylab facet_grid element_rect
#' @examples
#' \dontrun{
#' data(metMUD1)
#' loopThruSpectra(metMUD1)
#' }
#'
loopThruSpectra <- function(spectra, ...) {
  .chkArgs(mode = 11L)

  go <- chkGraphicsOpt()
  if (go == "base") {
    cat("Press ESC to stop looping through the spectra\n\n")
    ns <- length(spectra$names)
    for (i in 1:ns) {
      tt <- paste(spectra$names[i], " (#", i, " of ", ns, ")", sep = "")
      plotSpectra(spectra, which = i, main = tt, ...)
      devAskNewPage(ask = TRUE)
    }
    devAskNewPage(ask = FALSE)
  }

  if (go == "ggplot2") {
    x <- spectra$freq
    l.x <- length(x)
    df1 <- data.frame(x = NA_real_, y = NA_real_, spectra.name = NA_real_)
    for (i in 1:length(spectra$names))
    {
      y <- spectra$data[i, ]
      spectra.name <- rep(spectra$names[i], l.x)
      df2 <- data.frame(x = x, y = y, spectra.name = spectra.name)
      df1 <- rbind(df1, df2)
    }
    df1 <- df1[-1, ]
    p <- ggplot(df1, aes(x = x)) +
      geom_line(aes(y = y)) +
      theme_bw() +
      xlab(spectra$unit[1]) +
      ylab(spectra$unit[2]) +
      facet_grid(spectra.name ~ ., switch = "both") + # faceting
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

    return(p)
  }
}
