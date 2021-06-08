#'
#' Plot Spectra Object
#'
#' Plots the spectra stored in a \code{\link{Spectra}} object.  One may choose
#' which spectra to plot, and the x range to plot.  Spectra may be plotted
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
#' more than one is plotted.  Set to 0.0 for a stacked plot.
#'
#' @param amplify A number specifying an amplification factor to be applied to
#' all spectra.  Useful for magnifying spectra so small features show up
#' (though large peaks will then be clipped, unless you zoom on the x axis).
#'
#' @param lab.pos A number giving the location for the identifying label.
#' Generally, pick an area that is clear in all spectra plotted.  If no label
#' is desired, give \code{lab.pos} outside the plotted x range.
#'
#' @param showGrid Logical.  Places light gray vertical lines at each tick mark
#' if \code{TRUE}.
#'
#' @param leg.loc Character; if \code{"none"} no legend will be drawn.
#' Otherwise, any string acceptable to \code{\link{legend}}.
#'
#' @param \dots Additional parameters to be passed to plotting functions.
#'
#' @return None.  Side effect is a plot.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso \code{\link{plotSpectraJS}} for the interactive version.
#' Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#'
#' @keywords hplot
#'
#' @examples
#'
#' data(metMUD1)
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
#'   leg.loc = list(x = 3.2, y = 1.45)
#' )
#' @export plotSpectra
#'
#' @importFrom graphics grid lines text points plot
#'
plotSpectra <- function(spectra, which = c(1),
                        yrange = range(spectra$data),
                        offset = 0.0, amplify = 1.0,
                        lab.pos = mean(spectra$freq),
                        showGrid = TRUE, leg.loc = "none", ...) {
  .chkArgs(mode = 11L)
  chkSpectra(spectra)
  
  go<-chkGraphicsOpt()
  print(go)
  if(go =='base')
  {
  # set up and plot the first spectrum

  spectrum <- spectra$data[which[1], ] * amplify

  plot(spectra$freq, spectrum,
    type = "n",
    xlab = spectra$unit[1], ylab = spectra$unit[2],
    ylim = yrange,
    frame.plot = FALSE, ...
  )
  if (showGrid) grid(ny = NA, lty = 1) # grid will be underneath all spectra
  lines(spectra$freq, spectrum, col = spectra$colors[which[1]], ...)
  lab.x <- lab.pos
  spec.index <- findInterval(lab.x, sort(spectra$freq))
  lab.y <- spectrum[spec.index]
  text(lab.x, lab.y, labels = spectra$names[which[1]], pos = 3, cex = 0.75)

  which <- which[-1] # first spectrum already plotted so remove it from the list
  count <- 0 # get the other spectra and plot them as well
  for (n in which) {
    count <- count + 1
    spectrum <- (spectra$data[n, ] + (offset * count)) * amplify
    points(spectra$freq, spectrum, type = "l", col = spectra$colors[n], ...)
    lab.y <- spectrum[spec.index]
    text(lab.x, lab.y, labels = spectra$names[n], pos = 3, cex = 0.75)
  }

  if (all(leg.loc != "none")) .addLegend(spectra, leg.loc, use.sym = FALSE, bty = "n")
  }
  
  if(go =="ggplot2")
  {
    
  
    #Created an empty vector df
    df <- 0
    
    #Added the frequency in the dataframe
    df <- data.frame(spectra$freq)
    
    #Added the data for the specified plots 
    for (i in which) {
      i <- (spectra$data[i, ])
      df <- cbind(df, i)
    }
    #df
    
    #Defined the empty name vector for renaming the columns in dataframe 
    name <- c()
    for (i in which)
    {
      name <- c(name, spectra$names[i])
    }
    
    #Renamed the names of the columns
    names(df) <- c("WaveNumber", name)
    
    # df
    
    count <- 0
    
    spectrum <- spectra$data[which[1], ] * amplify
    
    #x coordinate of the label
    lab.x <- lab.pos
    spec.index <- findInterval(lab.x, sort(spectra$freq))
    
    #Empty vector for storing y coordinate of the label
    lab.y <- c()
    for (i in 2:ncol(df)) {
      df[, i] <- (df[, i] + (count * offset)) * amplify
      lab.y <- c(lab.y, df[, i][spec.index] + 0.1)
      count <- count + 1
    }
    # print(lab.y)
    
    #Empty vector for storing the colors of the plots
    color <- c()
    for (i in which)
    {
      color <- c(color, spectra$colors[i])
    }
    
    #Used this function so that I can create multiple plots
    molten.data <- melt(df, id = c("WaveNumber"))
    
    
    #prework for legend
    #legend in ggpot2 works on the principle of percent so I had to adjust so that
    #there is no conflict with "base" graphics
    if (all(leg.loc !="none")) {
      leg.loc$y
      min.x<-0
      max.x<-spectra$freq[length(spectra$freq)]
      leg.loc$x<-(leg.loc$x-min.x)/(max.x-min.x)
      min.y<-yrange[1]
      max.y<-yrange[2]
      leg.loc$y<-(leg.loc$y-min.y)/(max.y-min.y)
    }
    
    
    p <- ggplot(data = molten.data, aes(
      x = WaveNumber, y = value, group = variable,
      color = variable
    )) +
      geom_line() +
      
      #I have used it for manually specifying the color of each plot 
      scale_color_manual(values = color) +
      annotate("text",
               x = lab.pos,
               y = lab.y,
               label = name
      ) +
      
      #labels for x and y axis
      labs(x = spectra$unit[1], y = spectra$unit[2]) +
      
      #theme selection
      theme(plot.title = element_text(size = 12, color = "red", hjust = 0.5)) +
      theme_bw() +
      
      #y range
      ylim(yrange) +
      theme(legend.position = "none") +
      theme(panel.border = element_blank(), axis.line = element_line(colour = "black"))
    
    #legend location
    if (all(leg.loc !="none")) {
      p <- p + theme(
        legend.position = c(leg.loc$x,leg.loc$y),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)
      )
    }
    #condition for grid
    if (!showGrid) {
      p <- p + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
    }
    p
  }
}
