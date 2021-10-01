#'
#' Plot Measures of Central Tendency and Spread for a Spectra Object
#'
#' Compute and plot various measures of central tendency and
#' spread for a \code{\link{Spectra}} object.  Several different measures/spreads
#' are available.  These are useful as an overview of where a data set varies
#' the most.
#'
#' For \code{surveySpectra} the method choice works as follows: \code{sd} plots
#' the mean spectrum +/- the standard deviation, \code{sem} plots the mean
#' spectrum +/- the standard error of the mean, \code{sem95} plots the mean
#' spectrum +/- the standard error at the 95 percent confidence interval,
#' \code{mad} plots the median spectrum +/- the median absolute deviation, and
#' finally, \code{iqr} plots the median spectrum + the upper hinge and - the
#' lower hinge.
#'
#' For \code{surveySpectra2}, the spectra are mean centered and plotted.  Below
#' that, the relative summary statistic is plotted, offset, but on the same
#' scale.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}} to be analyzed.
#'
#' @param method Character.  One of \code{c("sd", "sem", "sem95", "mad",
#' "iqr")}.
#'
#' @param by.gr Logical, indicating if the analysis is to be done by group or
#' not. Applies to \code{surveySpectra} only.
#'
#' @param lab.pos Numeric, giving the frequency where the label should be drawn.
#' Applies to \code{surveySpectra2} only.
#'
#' @template param-graphics-dots
#' @template param-graphics-return
#'
#' @seealso Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @template authors-BH-TG
#'
#' @keywords hplot
#'
#' @export surveySpectra surveySpectra2
#'
#' @importFrom stats sd
#' @importFrom graphics plot lines text
#' @importFrom plyr aaply
#' @importFrom ggplot2 xlab ylab facet_grid element_rect
#' @importFrom plotly ggplotly
#'
#' @describeIn surveySpectra Spectral survey emphasizing mean or median spectrum, optionally by group.
#'
#' @examples
#'
#' # This example assumes the graphics output is set to ggplot2 (see ?GraphicsOptions).
#' library("ggplot2")
#' data(SrE.IR)
#' myt <- expression(bolditalic(Serenoa) ~ bolditalic(repens) ~ bold(Extract ~ IR ~ Spectra))
#'
#' p1 <- surveySpectra(SrE.IR, method = "iqr")
#' p1 <- p1 + ggtitle(myt)
#' p1
#' 
#' p2 <- surveySpectra2(SrE.IR, method = "iqr")
#' p2 <- p2 + ggtitle(myt)
#' p2
#'
surveySpectra <- function(spectra,
                          method = c("sd", "sem", "sem95", "mad", "iqr"),
                          by.gr = TRUE, ...) {
  .chkArgs(mode = 11L)
  chkSpectra(spectra)

  choices <- c("sd", "sem", "sem95", "mad", "iqr")

  if (!(method %in% choices)) {
    stop("The selected method is invalid.")
  }

  go <- chkGraphicsOpt()

  if (go == "base") {
    .chkReqGraphicsPkgs("lattice")

    if (!by.gr) {
      x <- spectra$freq

      if (method == "iqr") {
        y <- aaply(spectra$data, 2, .seXyIqr)
        df <- data.frame(x, y1 = y[, 1], y2 = y[, 2], y3 = y[, 3])
        p <- lattice::xyplot(y1 + y2 + y3 ~ x,
          data = df,
          col = c("black", "red", "red"), xlab = spectra$unit[1],
          ylab = "Full Data Set, median +/- iqr", type = "l", ...
        )
        plot(p)
      }

      if (method == "sd") {
        y1 <- aaply(spectra$data, 2, mean)
        s <- aaply(spectra$data, 2, sd)
        y2 <- y1 + s
        y3 <- y1 - s
        df <- data.frame(x, y1, y2, y3)
        p <- lattice::xyplot(y1 + y2 + y3 ~ x,
          data = df,
          col = c("black", "red", "red"), xlab = spectra$unit[1],
          ylab = "Full Data Set, mean +/- sd", type = "l", ...
        )
        plot(p)
      }

      if (method == "sem") {
        y <- aaply(spectra$data, 2, .seXy)
        df <- data.frame(x, y1 = y[, 1], y2 = y[, 2], y3 = y[, 3])
        p <- lattice::xyplot(y1 + y2 + y3 ~ x,
          data = df,
          col = c("black", "red", "red"), xlab = spectra$unit[1],
          ylab = "Full Data Set, mean +/- sem", type = "l", ...
        )
        plot(p)
      }

      if (method == "mad") {
        y <- aaply(spectra$data, 2, .seXyMad)
        df <- data.frame(x, y1 = y[, 1], y2 = y[, 2], y3 = y[, 3])
        p <- lattice::xyplot(y1 + y2 + y3 ~ x,
          data = df,
          col = c("black", "red", "red"), xlab = spectra$unit[1],
          ylab = "Full Data Set, median +/- mad", type = "l", ...
        )
        plot(p)
      }

      if (method == "sem95") {
        y <- aaply(spectra$data, 2, .seXy95)
        df <- data.frame(x, y1 = y[, 1], y2 = y[, 2], y3 = y[, 3])
        p <- lattice::xyplot(y1 + y2 + y3 ~ x,
          data = df,
          col = c("black", "red", "red"), xlab = spectra$unit[1],
          ylab = "Full Data Set, mean +/- 95% ci sem", type = "l", ...
        )
        plot(p)
      }
    }

    if (by.gr) {
      gr <- sumGroups(spectra)

      # See if any groups should be dropped due to too few members
      rem <- c()
      dropGroups <- FALSE
      for (n in 1:length(gr$group)) {
        if (gr$no.[n] <= 3) {
          message(
            "\nGroup ", gr$group[n],
            " has 3 or fewer members\n so your stats are not very useful...\n This group has been dropped for display purposes!"
          )
          rem <- c(rem, gr$group[n])
          dropGroups <- TRUE
        }
      }

      if (dropGroups) {
        spectra <- removeGroup(spectra, rem.group = rem)
        gr <- sumGroups(spectra) # update now that groups have been removed
      }

      # Now set up and plot
      x <- spectra$freq
      l.x <- length(x)

      if (method == "iqr") {
        df1 <- data.frame(x = NA_real_, y1 = NA_real_, y2 = NA_real_, y3 = NA_real_, spectra.group = NA_real_)
        for (n in 1:length(gr$group)) {
          which <- as.character(spectra$groups) == gr$group[n]
          y <- aaply(spectra$data[which, ], 2, .seXyIqr)
          spectra.group <- rep(gr$group[n], l.x)
          df2 <- data.frame(x = x, y1 = y[, 1], y2 = y[, 2], y3 = y[, 3], spectra.group = spectra.group)
          df1 <- rbind(df1, df2)
        }

        df1 <- df1[-1, ]
        p <- lattice::xyplot(y1 + y2 + y3 ~ x | spectra.group,
          data = df1,
          col = c("black", "red", "red"), xlab = spectra$unit[1],
          ylab = "median +/- iqr", type = "l",
          strip.left = TRUE, strip = FALSE,
          scales = list(x = "same", y1 = "same", y2 = "same", y3 = "same"),
          layout = c(1, length(gr$group)), ...
        )
        plot(p)
      }

      if (method == "sd") {
        df1 <- data.frame(x = NA_real_, y1 = NA_real_, y2 = NA_real_, y3 = NA_real_, spectra.group = NA_real_)
        for (n in 1:length(gr$group)) {
          which <- as.character(spectra$groups) == gr$group[n]
          y1 <- aaply(spectra$data[which, ], 2, mean)
          s <- apply(spectra$data[which, ], 2, sd)
          y2 <- y1 + s
          y3 <- y1 - s
          spectra.group <- rep(gr$group[n], l.x)
          df2 <- data.frame(x = x, y1 = y1, y2 = y2, y3 = y3, spectra.group = spectra.group)
          df1 <- rbind(df1, df2)
        }

        df1 <- df1[-1, ]
        p <- lattice::xyplot(y1 + y2 + y3 ~ x | spectra.group,
          data = df1,
          col = c("black", "red", "red"), xlab = spectra$unit[1],
          ylab = "mean +/- sd", type = "l",
          strip.left = TRUE, strip = FALSE,
          scales = list(x = "same", y1 = "same", y2 = "same", y3 = "same"),
          layout = c(1, length(gr$group)), ...
        )
        plot(p)
      }

      if (method == "sem") {
        df1 <- data.frame(x = NA_real_, y1 = NA_real_, y2 = NA_real_, y3 = NA_real_, spectra.group = NA_real_)
        for (n in 1:length(gr$group)) {
          which <- as.character(spectra$groups) == gr$group[n]
          y <- aaply(spectra$data[which, ], 2, .seXy)
          spectra.group <- rep(gr$group[n], l.x)
          df2 <- data.frame(x = x, y1 = y[, 1], y2 = y[, 2], y3 = y[, 3], spectra.group = spectra.group)
          df1 <- rbind(df1, df2)
        }

        df1 <- df1[-1, ]
        p <- lattice::xyplot(y1 + y2 + y3 ~ x | spectra.group,
          data = df1,
          col = c("black", "red", "red"), xlab = spectra$unit[1],
          ylab = "mean +/- sem", type = "l",
          strip.left = TRUE, strip = FALSE,
          scales = list(x = "same", y1 = "same", y2 = "same", y3 = "same"),
          layout = c(1, length(gr$group)), ...
        )
        plot(p)
      }

      if (method == "mad") {
        df1 <- data.frame(x = NA_real_, y1 = NA_real_, y2 = NA_real_, y3 = NA_real_, spectra.group = NA_real_)
        for (n in 1:length(gr$group)) {
          which <- as.character(spectra$groups) == gr$group[n]
          y <- aaply(spectra$data[which, ], 2, .seXyMad)
          spectra.group <- rep(gr$group[n], l.x)
          df2 <- data.frame(x = x, y1 = y[, 1], y2 = y[, 2], y3 = y[, 3], spectra.group = spectra.group)
          df1 <- rbind(df1, df2)
        }

        df1 <- df1[-1, ]
        p <- lattice::xyplot(y1 + y2 + y3 ~ x | spectra.group,
          data = df1,
          col = c("black", "red", "red"), xlab = spectra$unit[1],
          ylab = "median +/- mad", type = "l",
          strip.left = TRUE, strip = FALSE,
          scales = list(x = "same", y1 = "same", y2 = "same", y3 = "same"),
          layout = c(1, length(gr$group)), ...
        )
        plot(p)
      }

      if (method == "sem95") {
        df1 <- data.frame(x = NA_real_, y1 = NA_real_, y2 = NA_real_, y3 = NA_real_, spectra.group = NA_real_)
        for (n in 1:length(gr$group)) {
          which <- as.character(spectra$groups) == gr$group[n]
          y <- aaply(spectra$data[which, ], 2, .seXy95)
          spectra.group <- rep(gr$group[n], l.x)
          df2 <- data.frame(x = x, y1 = y[, 1], y2 = y[, 2], y3 = y[, 3], spectra.group = spectra.group)
          df1 <- rbind(df1, df2)
        }

        df1 <- df1[-1, ]
        p <- lattice::xyplot(y1 + y2 + y3 ~ x | spectra.group,
          data = df1,
          col = c("black", "red", "red"), xlab = spectra$unit[1],
          ylab = "mean +/- 95 % ci sem", type = "l",
          strip.left = TRUE, strip = FALSE,
          scales = list(x = "same", y1 = "same", y2 = "same", y3 = "same"),
          layout = c(1, length(gr$group)), ...
        )
        plot(p)
      }
    }
  }

  if ((go == "ggplot2") || (go == "plotly")) {
    .chkReqGraphicsPkgs("ggplot2")

    # Helper Function to plot in ggplot2 mode
    # df data frame with computed y values
    # x: x coordinates of the plot
    # y1: computed y coordinates first column
    # y2: computed y coordinates second column
    # y3: computed y coordinates third column
    # ylabel: label of the y coordinate

    .ssPlot <- function(df, Frequency, y1, y2, y3, ylabel) {
      ggplot(df, aes(x = Frequency)) +
        geom_line(aes(y = y1), color = "black") +
        geom_line(aes(y = y2), color = "red") +
        geom_line(aes(y = y3), color = "red") +
        xlab(spectra$unit[1]) +
        ylab(ylabel) +
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    }

    if (!by.gr) {
      Frequency <- spectra$freq
      p <- NULL

      if (method == "iqr") {
        y <- aaply(spectra$data, 2, .seXyIqr)
        df <- data.frame(Frequency, y1 = y[, 1], y2 = y[, 2], y3 = y[, 3])
        p <- .ssPlot(df, Frequency, y1, y2, y3, ylabel = "Full Data Set, median +/- iqr")
      }

      if (method == "sd") {
        y1 <- aaply(spectra$data, 2, mean)
        s <- aaply(spectra$data, 2, sd)
        y2 <- y1 + s
        y3 <- y1 - s
        df <- data.frame(Frequency, y1, y2, y3)

        p <- .ssPlot(df, Frequency, y1, y2, y3, ylabel = "Full Data Set, mean +/- sd")
      }

      if (method == "sem") {
        y <- aaply(spectra$data, 2, .seXy)
        df <- data.frame(Frequency, y1 = y[, 1], y2 = y[, 2], y3 = y[, 3])

        p <- .ssPlot(df, Frequency, y1, y2, y3, ylabel = "Full Data Set, mean +/- sem")
      }

      if (method == "mad") {
        y <- aaply(spectra$data, 2, .seXyMad)
        df <- data.frame(Frequency, y1 = y[, 1], y2 = y[, 2], y3 = y[, 3])

        p <- .ssPlot(df, Frequency, y1, y2, y3, ylabel = "Full Data Set, median +/- mad")
      }

      if (method == "sem95") {
        y <- aaply(spectra$data, 2, .seXy95)
        df <- data.frame(Frequency, y1 = y[, 1], y2 = y[, 2], y3 = y[, 3])

        p <- .ssPlot(df, Frequency, y1, y2, y3, ylabel = "Full Data Set, mean +/- 95% ci sem")
      }
      if (go == "ggplot2") {
        return(p)
      } else {
        .chkReqGraphicsPkgs("plotly")
        p <- ggplotly(p, tooltip = "Frequency")
        return(p)
      }
    } # end of if (!by.gr)

    if (by.gr) {
      gr <- sumGroups(spectra)
      p <- NULL
      # See if any groups should be dropped due to too few members
      rem <- c()
      dropGroups <- FALSE
      for (n in 1:length(gr$group)) {
        if (gr$no.[n] <= 3) {
          message(
            "\nGroup ", gr$group[n],
            " has 3 or fewer members\n so your stats are not very useful...\n This group has been dropped for display purposes!"
          )
          rem <- c(rem, gr$group[n])
          dropGroups <- TRUE
        }
      }

      if (dropGroups) {
        spectra <- removeGroup(spectra, rem.group = rem)
        gr <- sumGroups(spectra) # update now that groups have been removed
      }

      # Now set up and plot
      Frequency <- spectra$freq
      l.x <- length(Frequency)

      if (method == "iqr") {
        df1 <- data.frame(Frequency = NA_real_, y1 = NA_real_, y2 = NA_real_, y3 = NA_real_, spectra.group = NA_real_)
        for (n in 1:length(gr$group)) {
          which <- as.character(spectra$groups) == gr$group[n]
          y <- aaply(spectra$data[which, ], 2, .seXyIqr)
          spectra.group <- rep(gr$group[n], l.x)
          df2 <- data.frame(Frequency = Frequency, y1 = y[, 1], y2 = y[, 2], y3 = y[, 3], spectra.group = spectra.group)
          df1 <- rbind(df1, df2)
        }
        df1 <- df1[-1, ]

        p <- .ssPlot(df1, Frequency, y1, y2, y3, ylabel = "median +/- iqr")

        p <- p + facet_grid(spectra.group ~ ., switch = "both") +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      }

      if (method == "sd") {
        df1 <- data.frame(Frequency = NA_real_, y1 = NA_real_, y2 = NA_real_, y3 = NA_real_, spectra.group = NA_real_)
        for (n in 1:length(gr$group)) {
          which <- as.character(spectra$groups) == gr$group[n]
          y1 <- aaply(spectra$data[which, ], 2, mean)
          s <- apply(spectra$data[which, ], 2, sd)
          y2 <- y1 + s
          y3 <- y1 - s
          spectra.group <- rep(gr$group[n], l.x)
          df2 <- data.frame(Frequency = Frequency, y1 = y1, y2 = y2, y3 = y3, spectra.group = spectra.group)
          df1 <- rbind(df1, df2)
        }
        df1 <- df1[-1, ]

        p <- .ssPlot(df1, Frequency, y1, y2, y3, ylabel = "mean +/- sd")

        p <- p + facet_grid(spectra.group ~ ., switch = "both") +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      }

      if (method == "sem") {
        df1 <- data.frame(Frequency = NA_real_, y1 = NA_real_, y2 = NA_real_, y3 = NA_real_, spectra.group = NA_real_)
        for (n in 1:length(gr$group)) {
          which <- as.character(spectra$groups) == gr$group[n]
          y <- aaply(spectra$data[which, ], 2, .seXy)
          spectra.group <- rep(gr$group[n], l.x)
          df2 <- data.frame(Frequency = Frequency, y1 = y[, 1], y2 = y[, 2], y3 = y[, 3], spectra.group = spectra.group)
          df1 <- rbind(df1, df2)
        }
        df1 <- df1[-1, ]

        p <- .ssPlot(df1, Frequency, y1, y2, y3, ylabel = "mean +/- sem")

        p <- p + facet_grid(spectra.group ~ ., switch = "both") +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      }

      if (method == "mad") {
        df1 <- data.frame(Frequency = NA_real_, y1 = NA_real_, y2 = NA_real_, y3 = NA_real_, spectra.group = NA_real_)
        for (n in 1:length(gr$group)) {
          which <- as.character(spectra$groups) == gr$group[n]
          y <- aaply(spectra$data[which, ], 2, .seXyMad)
          spectra.group <- rep(gr$group[n], l.x)
          df2 <- data.frame(Frequency = Frequency, y1 = y[, 1], y2 = y[, 2], y3 = y[, 3], spectra.group = spectra.group)
          df1 <- rbind(df1, df2)
        }
        df1 <- df1[-1, ]

        p <- .ssPlot(df1, Frequency, y1, y2, y3, ylabel = "median +/- mad")

        p <- p + facet_grid(spectra.group ~ ., switch = "both") +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      }

      if (method == "sem95") {
        df1 <- data.frame(Frequency = NA_real_, y1 = NA_real_, y2 = NA_real_, y3 = NA_real_, spectra.group = NA_real_)
        for (n in 1:length(gr$group)) {
          which <- as.character(spectra$groups) == gr$group[n]
          y <- aaply(spectra$data[which, ], 2, .seXy95)
          spectra.group <- rep(gr$group[n], l.x)
          df2 <- data.frame(Frequency = Frequency, y1 = y[, 1], y2 = y[, 2], y3 = y[, 3], spectra.group = spectra.group)
          df1 <- rbind(df1, df2)
        }
        df1 <- df1[-1, ]

        p <- .ssPlot(df1, Frequency, y1, y2, y3, ylabel = "mean +/- 95 % ci sem")
        p <- p + facet_grid(spectra.group ~ ., switch = "both") +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      }
      if (go == "ggplot2") {
        return(p)
      } else {
        .chkReqGraphicsPkgs("plotly")
        p <- ggplotly(p, tooltip = c("Frequency"))
        return(p)
      }
    }
  }
}
