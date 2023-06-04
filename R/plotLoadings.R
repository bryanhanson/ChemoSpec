#'
#' Plot PCA Loadings for a Spectra Object
#'
#' Creates a multi-panel plot of loadings along with a reference spectrum.
#'
#' @param spectra `r .writeDoc_Spectra1()`
#'
#' @param pca An object of class \code{\link{prcomp}}, modified to include a
#' list element called \code{$method}, a character string describing the
#' pre-processing carried out and the type of PCA performed (it appears on the
#' plot).  This is automatically provided if \code{ChemoSpec} functions
#' \code{\link{c_pcaSpectra}} or \code{\link{r_pcaSpectra}} were used to create
#' \code{pca}.
#'
#' @param loads An integer vector giving the loadings to plot.  More than 3
#' loadings creates a useless plot using the default graphics window.
#'
#' @param ref An integer specifying the reference spectrum to plot, which
#' appears at the bottom of the plot.
#'
#' @param \dots `r .writeDoc_GraphicsDots()`
#' @return `r .writeDoc_GraphicsReturn()`
#'
#' @author `r .writeDoc_Authors(c("BH", "TG"))`
#'
#' @seealso \code{\link{c_pcaSpectra}} for an example.  See \code{\link{plot2Loadings}}
#' to plot two loadings against each
#' other, and \code{\link{sPlotSpectra}} for an alternative approach.
#' Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @keywords multivariate hplot
#'
#' @export plotLoadings
#'
#' @importFrom graphics plot
#' @importFrom stats relevel
#' @importFrom ggplot2 geom_segment aes_string
#' @importFrom utils combn
#' @importFrom patchwork plot_layout
#'
plotLoadings <- function(spectra, pca, loads = c(1), ref = 1, ...) {
  .chkArgs(mode = 12L)
  go <- chkGraphicsOpt()

  if (go == "base") {
    .chkReqGraphicsPkgs("lattice")
    # Stack the requested data into a data frame for plotting

    names <- paste("PC", loads, "Loadings", sep = " ")
    names <- c("Ref. Spectrum", names)
    x <- rep(spectra$freq, length(loads) + 1)

    z <- rep(names[1], length(spectra$freq))
    y <- spectra$data[ref, ] # load in the reference spectrum


    for (n in 1:length(loads)) {
      y <- c(y, pca$rotation[, loads[n]]) # add in each loading
      z <- c(z, rep(names[n + 1], length(spectra$freq)))
    }

    z <- as.factor(z)
    z <- relevel(z, "Ref. Spectrum")
    df <- data.frame(y, x, z)

    # Do the plot
    # Note: no way exists to plot the x axis reversed for multiple panels

    p <- lattice::xyplot(y ~ x | z,
      data = df,
      xlab = spectra$unit[1], ylab = "",
      sub = list(
        label = pca$method,
        fontface = "plain"),
      layout = c(1, length(loads) + 1),
      strip.left = TRUE, strip = FALSE, col = "black",
      scales = list(x = "same", y = "free"),
      panel = function(..., type = "h") {
        if (lattice::panel.number() == 1) {
          lattice::panel.xyplot(..., type = "l")
        } else {
          lattice::panel.xyplot(..., type = type)
        }
      }, ...)

    plot(p)
  }

  if (go == "ggplot2") {
    Frequency <- patch_plot <- NULL # satisfy CRAN check engine
    .chkReqGraphicsPkgs(c("ggplot2", "patchwork"))

    names <- paste("PC", loads, "Loadings", sep = "")
    names <- c("RefSpectrum", names)
    x <- spectra$freq

    df <- cbind(Frequency = spectra$freq, RefSpectrum = spectra$data[ref, ], pca$rotation[, loads])
    df <- as.data.frame(df)
    names(df) <- c("Frequency", names)

    var_list <- combn(names(df)[1:ncol(df)], 2, simplify = FALSE)

    plots <- ncol(df) - 1
    plot_list <- list()
    for (i in 1:plots) {
      p <- ggplot(df, aes_string(x = var_list[[i]][1])) +
        geom_segment(aes_string(y = var_list[[i]][2], xend = x, yend = 0), color = "black") +
        theme_bw() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
      plot_list[[i]] <- p
    }

    ref_plot <- ggplot(df, aes(x = Frequency, y = df[, 2])) +
      geom_line() +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      ylab("Ref. spectrum") +
      xlab(pca$method)

    for (i in length(plot_list):2) {
      if (i == length(plot_list)) {
        patch_plot <- plot_list[[i]]
      } else {
        patch_plot <- patch_plot + plot_list[[i]]
      }
    }

    patch_plot + ref_plot + plot_layout(ncol = 1)
  } # end of go == "ggplot2"
}
