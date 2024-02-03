#'
#' Plot Measures of Central Tendency and Spread for a Spectra Object
#'
#'
#' @describeIn surveySpectra Spectral survey emphasizing variation among spectra.
#'
surveySpectra2 <- function(spectra,
                           method = c("sd", "sem", "sem95", "mad", "iqr"),
                           lab.pos = 0.9 * max(spectra$freq), ...) {
  .chkArgs(mode = 11L)
  chkSpectra(spectra)

  x <- spectra$freq
  M <- scale(spectra$data, scale = FALSE) # Center by column means

  # Compute the requested data

  if (method == "iqr") {
    y <- t(apply(spectra$data, 2, .seXyIqr))
    y <- y[, 3] - y[, 2]
    lab <- "iqr"
  }

  if (method == "sd") {
    y <- t(apply(spectra$data, 2, sd))
    lab <- "sd"
  }

  if (method == "sem") {
    y <- t(apply(spectra$data, 2, .seXy))
    y <- y[, 3] - y[, 2]
    lab <- "sem"
  }

  if (method == "mad") {
    y <- t(apply(spectra$data, 2, .seXyMad))
    y <- y[, 3] - y[, 2]
    lab <- "mad"
  }

  if (method == "sem95") {
    y <- t(apply(spectra$data, 2, .seXy95))
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
    off3 <- abs(min(M)) + abs(max(y)) + abs(0.05 * diff(range(M)))
    off4 <- min(M[nrow(M), ]) - off3 + 0.5 * diff(range(y))

    ymin <- off2 - off1
    plot(x, M[1, ], type = "n", ylim = c(ymin, ymax), xlab = spectra$unit[1], ylab = "Centered Spectra", ...)
    for (i in 1:(nrow(M) - 1)) {
      lines(x, M[i, ], col = spectra$colors[i])
    }
    lines(x, M[nrow(M), ] - off3)
    text(x = lab.pos, y = off4, labels = lab, cex = 1.2, adj = c(0.5, 1))
  }

  if ((go == "ggplot2") || (go == "plotly")) {
    value <- variable <- Frequency <- NULL # satisfy CRAN check engine
    .chkReqGraphicsPkgs("ggplot2")

    ymax <- max(M)

    DF_spread <- data.frame(x, y) # holds measure of spread, e.g sd, seX etc

    # Offset the summary stat below everything else, with a small gap for aesthetics
    # off1 <- diff(range(y)) # not used in ggplot2 environment
    # off2 <- min(M) - 0.05 * diff(range(M))
    off3 <- abs(min(M)) + abs(max(y)) + abs(0.05 * diff(range(M)))
    off4 <- min(DF_spread$y) - off3 + 0.5 * diff(range(y)) # offset for label

    DF_centered <- as.data.frame(cbind(spectra$freq, t(M)))
    names(DF_centered) <- c("Frequency", spectra$names)

    molten <- reshape2::melt(DF_centered, id = c("Frequency"))

    p <- ggplot() +
      geom_line(
        data = molten,
        aes(x = Frequency, y = value, group = variable, color = variable)) +
      scale_color_manual(name = "Key", values = spectra$colors) +
      theme_bw() +
      theme(legend.position = "none") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      labs(x = spectra$unit[1], y = "Centered Spectra")

    p <- p + geom_line(data = DF_spread, aes(x = x, y = y - off3))
    p <- p + .ggAnnotate(method, x = lab.pos, y = off4, gp = gpar(fontsize = 8))

    if (go == "ggplot2") {
      return(p)
    } else {
      .chkReqGraphicsPkgs("plotly")
      p <- ggplotly(p, tooltip = "Frequency")
      return(p)
    }
  }
}
