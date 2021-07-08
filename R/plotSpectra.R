#'
#' Plot Spectra Object
#'
#' Plots the spectra stored in a \code{\link{Spectra}} object. Spectra may be plotted
#' offset or stacked.  The vertical scale is controlled by a combination of
#' several parameters.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param which An integer vector specifying which spectra to plot, and the
#' order.
#'
#' @param yrange A vector giving the limits of the y axis desired, for instance
#' \code{c(0, 15)}.  This parameter depends upon the range of values in the
#' stored spectra and defaults to the height of the largest peak in the data
#' set.  Interacts with the next two arguments, as well as the number of
#' spectra to be plotted as given in \code{which}.  Trial and error is used to
#' adjust all these arguments to produce the desired plot.
#'
#' @param offset A number specifying the vertical offset between spectra if
#' more than one is plotted.  Set to 0.0 to overlay the spectra.
#'
#' @param amplify A number specifying an amplification factor to be applied to
#' all spectra.  Useful for magnifying spectra so small features show up
#' (though large peaks will then be clipped, unless you zoom on the x axis).
#'
#' @param lab.pos A number (in frequency units) giving the location of a label for each spectrum.
#' Generally, pick an area that is clear in all spectra plotted.  If no label
#' is desired, give \code{lab.pos} outside the plotted x range.
#'
#' @param showGrid Logical.  Places light gray vertical lines at each tick mark
#' if \code{TRUE}.
#'
#' @param leg.loc A list giving x and y coordinates; if \code{"none"} no legend will be drawn.
#' The plotting of the legend is based on the origin of the plot as [0,0] and the top right
#' corner is [1,1]. If one wants the plot dead center use \code{leg.loc = list(x = 0.5, y = 0.5)}.
#'
#' @param \dots Parameters to be passed to the plotting routines. Arguments must be consistent with the 
#'        choice of graphics option. See \code{\link{GraphicsOptions}}.
#'
#' @return
#' The returned value depends on the graphics option selected (see \code{\link{GraphicsOptions}}).
#' \describe{
#'  \item{base:}{    None.  Side effect is a plot.}
#'  \item{ggplot2:}{    Returns a \code{ggplot2} plot object. The plot can be modified in the usual
#'                      \code{ggplot2} manner.}
#' }
#'
#' @author Bryan A. Hanson, DePauw University, Tejasvi Gupta.
#'
#' @seealso \code{\link{plotSpectraJS}} for the interactive version. See \code{\link{GraphicsOptions}}
#'          for more information about the graphics options. Additional documentation at
#'          \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#'
#' @keywords hplot
#'
#' @export plotSpectra
#'
#' @importFrom graphics grid lines text points plot
#' @importFrom ggplot2 aes annotate annotation_custom coord_cartesian element_blank
#' @importFrom ggplot2 element_line element_text geom_line ggplot ggtitle labs
#' @importFrom ggplot2 scale_color_manual theme theme_bw theme_classic ylim
#' @importFrom grid grobTree textGrob
#' @importFrom reshape2 melt
#' @examples
#'
#' data(metMUD1)
#'
#' # Using base graphics (the default, but need to set here
#' # in case someone changed it!)
#' options(ChemoSpecGraphics = "ggplot2")
#' plotSpectra(metMUD1,
#'   main = "metMUD1 NMR Data",
#'   which = c(10, 11), yrange = c(0, 1.5),
#'   offset = 0.06, amplify = 10, lab.pos = 0.5
#' )
#'
#' # Add a legend at x, y coords
#' plotSpectra(metMUD1,
#'   main = "metMUD1 NMR Data",
#'   which = c(10, 11), yrange = c(0, 1.5),
#'   offset = 0.06, amplify = 10, lab.pos = 0.5,
#'   leg.loc = list(x = 0.8, y = 0.8)
#' )
#'
#' options(ChemoSpecGraphics = "base") # turn off for later examples/tests
#'
#'

plotSpectra <- function(spectra, which = c(1),
                        yrange = range(spectra$data),
                        offset = 0.0, amplify = 1.0,
                        lab.pos = mean(spectra$freq),
                        showGrid = TRUE, leg.loc = "none", ...) {
  .chkArgs(mode = 11L)
  chkSpectra(spectra)

  # Helper Function to calculate the label y position
  # spec: spectra data matrix with *samples in rows* (previously subsetted by which,
  #       & modified by offset & amplify)
  # pct: label position in % of y range
  pct = 20.0
  calcYpos <- function(spec, pct) {
    ymin <- apply(spec, 1, min)
    ymax <- apply(spec, 1, max)
    ypos <- ymin + (pct * (ymax - ymin))/100
  }

  go <- chkGraphicsOpt()

  if (go == "base") {

    # Prepare the data needed for plotting, apply amplify & offset
    M <- spectra$data[which,, drop = FALSE]
    Mcols <- spectra$colors[which]
    Mnames <- spectra$names[which]
    freq <- spectra$freq
    count <- 0L
    for (i in 1:nrow(M)) {
      M[i, ] <- (M[i,] + (offset * count)) * amplify
      count <- count + 1
    }

    # Set up and plot the first spectrum (type = "n")
    plot(freq, M[1,],
      type = "n",
      xlab = spectra$unit[1], ylab = spectra$unit[2],
      ylim = yrange,
      frame.plot = FALSE, ...
    )

    # Place grid under all spectra
    if (showGrid) grid(ny = NA, lty = 1)

    # Add the spectra
    for (i in 1:nrow(M)) lines(freq, M[i, ], col = Mcols[i], ...)

    # Add sample names
    lab.x <- lab.pos
    lab.y <- calcYpos(M, pct)
    text(lab.x, lab.y, labels = Mnames, adj = c(0.5, 0.5), cex = 0.75)

    # Prep legend location if legend requested
    if (all(leg.loc != "none")) {
      x.min <- min(spectra$freq)
      x.max <- max(spectra$freq)

      y.min <- yrange[1]
      y.max <- yrange[2]
      args <- as.list(match.call())[-1] # capture xlim if user passes it
      if ("xlim" %in% names(args)) {
        xl <- eval(args$xlim) # Converting 'args$xlim' to a usable form
        x.min <- xl[1]
        x.max <- xl[2]
      }
      # base graphics normally uses data native coordinates
      # convert to percent to be consistent with how ggplot2 will plot the legend
      # also eliminates guessing about the y coord for the legend when spectra are offset
      leg.loc$x <- (leg.loc$x) * (x.max - x.min) + x.min
      leg.loc$y <- (leg.loc$y) * (y.max - y.min) + y.min

      .addLegend(spectra, leg.loc, use.sym = FALSE, bty = "n")
    }
  }

  if (go == "ggplot2") {
    value <- variable <- Frequency <- NULL # satisfy CRAN check engine

    # Set up data frame to hold data to be plotted ready for melting
    df <- data.frame(spectra$freq)
    count <- 0
    for (i in which) {
      spec <- ((spectra$data[i, ]) + (count * offset)) * amplify
      df <- cbind(df, spec)
      count <- count + 1
    }
    names(df) <- c("Frequency", spectra$names[which])

    lab.x <- lab.pos # values in native data space
    lab.y <- calcYpos(t(as.matrix(df[,-1])), pct)

    molten.data <- reshape2::melt(df, id = c("Frequency"))

    p <- ggplot(data = molten.data,
      aes(x = Frequency, y = value, group = variable, color = variable)) +
      geom_line() +
      scale_color_manual(name = "Key", values = spectra$colors[which]) +
      annotate("text", x = lab.x, y = lab.y, label = spectra$names[which]) +
      labs(x = spectra$unit[1], y = spectra$unit[2]) +
      ylim(yrange) +
      theme_bw() +
      theme(legend.position = "none") +
      theme(panel.border = element_blank(), axis.line = element_line(colour = "black")) +
      theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

    if (!showGrid) {
      p <- p + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
    }

    if (all(leg.loc != "none")) {
      group <- c(NA_real_)
      color <- c(NA_real_)
      for (i in spectra$groups)
      {
        if (!(i %in% group)) {
          group <- c(group, i)
        }
      }
      for (i in spectra$colors)
      {
        if (!(i %in% color)) {
          color <- c(color, i)
        }
      }
      group <- group[-1]
      color <- color[-1]

      # 0.025 is the fudge factor for better display of legend

      keys <- grobTree(textGrob("Key",
        x = leg.loc$x, y = leg.loc$y + 0.025, hjust = 0,
        gp = gpar(col = "black", fontsize = 12)
      ))

      for (i in 1:length(group))
      {
        grob <- grid::grobTree(textGrob(group[i],
          x = leg.loc$x, y = leg.loc$y, hjust = 0,
          gp = gpar(col = color[i], fontsize = 12)
        ))
        leg.loc$y <- leg.loc$y - 0.025
        p <- p + annotation_custom(grob) + annotation_custom(keys)
      }
    }

    return(p)
  }
}
