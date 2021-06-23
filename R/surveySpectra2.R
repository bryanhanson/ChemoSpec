#'
#' Plot Measures of Central Tendency and Spread for a Spectra Object
#'
#'
#' @describeIn surveySpectra Spectral survey emphasizing variation among spectra.
#'
surveySpectra2 <- function(spectra, method = c("sd", "sem", "sem95", "mad", "iqr"),
                           lab.pos = 0.9 * max(spectra$freq), ...) {
  .chkArgs(mode = 11L)
  chkSpectra(spectra)

  x <- spectra$freq
  M <- scale(spectra$data, scale = FALSE) # Center by column means

  # Compute the requested data

  if (method == "iqr") {
    y <- aaply(spectra$data, 2, .seXyIqr)
    y <- y[, 3] - y[, 2]
    lab <- "iqr"
  }

  if (method == "sd") {
    y <- aaply(spectra$data, 2, sd)
    lab <- "sd"
  }

  if (method == "sem") {
    y <- aaply(spectra$data, 2, .seXy)
    y <- y[, 3] - y[, 2]
    lab <- "sem"
  }

  if (method == "mad") {
    y <- aaply(spectra$data, 2, .seXyMad)
    y <- y[, 3] - y[, 2]
    lab <- "mad"
  }

  if (method == "sem95") {
    y <- aaply(spectra$data, 2, .seXy95)
    y <- y[, 3] - y[, 2]
    lab <- "sem95"
  }
  go <- chkGraphicsOpt()
  if (go == "base") {

    # Now set up the plot and plot it!

    M <- rbind(M, y)
    ymax <- max(M)

    # Offset the summary stat below everything else, with a small gap for aesthetics

    off1 <- diff(range(y))
    off2 <- min(M) - 0.05 * diff(range(M))
    ymin <- off2 - off1
    off3 <- abs(min(M)) + abs(max(y)) + abs(0.05 * diff(range(M)))

    plot(x, M[1, ], type = "n", ylim = c(ymin, ymax), xlab = spectra$unit[1], ylab = "Centered Spectra", ...)
    for (i in 1:(nrow(M) - 1)) {
      lines(x, M[i, ], col = spectra$colors[i])
    }
    lines(x, M[nrow(M), ] - off3)
    off4 <- min(M[nrow(M), ]) - off3 + 0.5 * diff(range(y))
    text(x = lab.pos, y = off4, labels = lab, cex = 1.2, adj = c(0.5, 1))
  }
  if (go == "ggplot2") {
    value <- variable <- Frequency <- NULL # satisfy CRAN check engine



    # M <- rbind(M, y)
    ymax <- max(M)

    # New small dataframe for plotting the different geom_line()
    df1 <- data.frame(x, y)


    # Offset the summary stat below everything else, with a small gap for aesthetics

    off1 <- diff(range(y))
    off2 <- min(M) - 0.05 * diff(range(M))
    ymin <- off2 - off1
    off3 <- abs(min(M)) + abs(max(y)) + abs(0.05 * diff(range(M)))

    df <- data.frame(x)
    for (i in 1:length(spectra$names))
    {
      spec <- M[i, ]
      df <- cbind(df, spec)
    }
    names(df) <- c("Frequency", spectra$names)

    molten.data <- reshape2::melt(df, id = c("Frequency"))
    p <- ggplot() +
      geom_line(
        data = molten.data,
        aes(x = Frequency, y = value, group = variable, color = variable)
      ) +
      scale_color_manual(name = "Key", values = spectra$colors) +
      theme_bw() +
      theme(legend.position = "none") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      labs(x = spectra$unit[1], y = "centered Spectra")
    p <- p + geom_line(data = df1, aes(x = x, y = y - off3))

    off4 <- min(df1$y) - off3 + 0.5 * diff(range(y))
    keys <- grobTree(textGrob("method",
      x = lab.pos, y = off4, hjust = 0,
      gp = gpar(col = "black", fontsize = 12)
    ))

    p <- p + annotate(
      geom = "text", x = lab.pos, y = off4, label = method,
      color = "black"
    )

    return(p)
  }
}
