#'
#' Review All the Spectra in a Spectra Object
#'
#' Utility to review all spectra in a \code{Spectra} object.  Output depends upon the graphics
#' output choice.
#' \describe{
#'   \item{base:}{Plots each spectrum one at a time, and waits for a return in the console before
#'     plotting the next spectrum.  Use \code{ESC} to get out of the loop.}
#'   \item{ggplot2:}{All the spectra are plotted in a single column.}
#'  }
#'
#' @param spectra `r .writeDoc_Spectra1()`
#'
#' @param \dots `r .writeDoc_GraphicsDots()`
#' @return `r .writeDoc_GraphicsReturn()`
#'
#' @author `r .writeDoc_Authors(c("BH", "TG"))`
#'
#' @keywords hplot
#'
#' @seealso See [ChemoSpecUtils::GraphicsOptions()]
#'          for more information about the graphics options. Additional documentation at
#'          \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @export reviewAllSpectra
#'
#' @importFrom grDevices devAskNewPage
#' @importFrom ggplot2 xlab ylab facet_grid element_rect
#'
#' @examples
#' # Because there are 16 spectra in this data set, you probably want to
#' # expand the height of the graphics device to see the spectra clearly.
#' # This example assumes the graphics output is set to ggplot2 or plotly (see ?GraphicsOptions).
#' # If you do options(ChemoSpecGraphics == "plotly") you'll get the results
#' # in a web page, which is particularly convenient.
#' library("ggplot2")
#' data(metMUD1)
#' p <- reviewAllSpectra(metMUD1)
#' p
#' 
#'
reviewAllSpectra <- function(spectra, ...) {
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

  if ((go == "ggplot2") || (go == "plotly")) {
    .chkReqGraphicsPkgs("ggplot2")
    Frequency <- spectra$freq
    l.x <- length(Frequency)

    df1 <- data.frame(Frequency = NA_real_, y = NA_real_, spectra.name = NA_character_)
    for (i in 1:length(spectra$names)) {
      y <- spectra$data[i, ]
      spectra.name <- rep(spectra$names[i], l.x)
      df2 <- data.frame(Frequency = Frequency, y = y, spectra.name = spectra.name)
      df1 <- rbind(df1, df2)
    }
    df1 <- df1[-1, ]

    p <- ggplot(df1, aes(x = Frequency)) +
      geom_line(aes(y = y)) +
      xlab(spectra$unit[1]) +
      ylab(spectra$unit[2]) +
      facet_grid(spectra.name ~ ., switch = "both") + # faceting
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

    if (go == "ggplot2") {
      return(p)
    } else {
      .chkReqGraphicsPkgs("plotly")
      p <- ggplotly(p, tooltip = "Frequency")
      return(p)
    }
  }
}
