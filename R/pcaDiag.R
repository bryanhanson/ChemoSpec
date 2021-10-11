#'
#' Outlier Diagnostic Plots for PCA of a Spectra Object
#'
#' A function to carry diagnostics on the PCA results for a
#' \code{\link{Spectra}} object.  Basically a wrapper to Filzmoser's
#' \code{\link[chemometrics]{pcaDiagplot}} which colors everything according to
#' the scheme stored in the \code{\link{Spectra}} object.  Works with PCA
#' results of either class \code{prcomp} or class \code{princomp}.  Works
#' with either classical or robust PCA results.
#'
#' If both plots are desired, the output should be directed to a file rather
#' than the screen.  Otherwise, the 2nd plot overwrites the 1st in the active
#' graphics window.  Alternatively, just call the function twice, once
#' specifying \code{OD} and once specifying \code{SD}.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param pca An object of class \code{\link{prcomp}}
#' modified to include a character string (\code{$method}) describing the
#' pre-processing carried out and the type of PCA performed.
#'
#' @param pcs As per \code{\link[chemometrics]{pcaDiagplot}}.  The number of
#' principal components to include.
#'
#' @param quantile As per \code{\link[chemometrics]{pcaDiagplot}}.  The
#' significance criteria to use as a cutoff.
#'
#' @param plot A character string, indicating whether to plot the score
#' distances or orthogonal distances, or both.  Options are \code{c("OD",
#' "SD")}.
#'
#' @param use.sym logical; if true, the color scheme is change to black and
#' symbols are used for plotting.
#'
#' @template param-graphics-dots
#' @template param-graphics-return2
#' @template authors-BH-TG
#'
#' @seealso \code{\link[chemometrics]{pcaDiagplot}} in package
#' \code{chemometrics} for the underlying function. Additional documentation at
#' \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @references K. Varmuza and P. Filzmoser \emph{Introduction to Multivariate
#' Statistical Analysis in Chemometrics}, CRC Press, 2009.
#'
#' @keywords multivariate
#'
#' @export pcaDiag
#'
#' @importFrom stats qchisq median mad qnorm
#' @importFrom graphics plot abline
#' @importFrom ggplot2 geom_line geom_hline geom_point
#'
#' @examples
#' # This example assumes the graphics output is set to ggplot2 (see ?GraphicsOptions).
#' library("ggplot2")
#' data(SrE.IR)
#' pca <- c_pcaSpectra(SrE.IR, choice = "noscale")
#' p1 <- pcaDiag(SrE.IR, pca, pcs = 2, plot = "OD") + ggtitle("OD Plot")
#' p1
#' p2 <- pcaDiag(SrE.IR, pca, pcs = 2, plot = "SD") + ggtitle("SD Plot")
#' p2
#'
pcaDiag <- function(spectra,
                    pca,
                    pcs = 3,
                    quantile = 0.975,
                    plot = c("OD", "SD"),
                    use.sym = FALSE,
                    ...) {
  msg <- "This function cannot be used with data from sparse pca"
  if (inherits(pca, "converted_from_arrayspc")) stop(msg)
  .chkArgs(mode = 12L)
  if (inherits(pca, "prcomp")) pca <- .q2rPCA(pca)

  X <- spectra$data
  X.pca <- pca
  a <- pcs
  if (is.null(a)) a <- 3

  SDist <- sqrt(apply(t(t(X.pca$sco[, 1:a]^2) / X.pca$sdev[1:a]^2), 1, sum))
  ODist <- sqrt(apply((X - X.pca$sco[, 1:a] %*% t(X.pca$loa[, 1:a]))^2, 1, sum))
  critSD <- sqrt(qchisq(quantile, a))
  critOD <- (median(ODist^(2 / 3)) + mad(ODist^(2 / 3)) * qnorm(quantile))^(3 / 2)

  sub <- "sample index"

  go <- chkGraphicsOpt()

  if (go == "base") {
    if ("SD" %in% plot) {
      if (!use.sym) {
        plot(SDist,
          ylim = c(0, max(SDist)), ylab = "score distance",
          xlab = spectra$desc, sub = sub, main = "Possible PCA Outliers\nbased on Score Distance",
          col = spectra$colors, pch = 20, ...
        )
      }
      if (use.sym) {
        plot(SDist,
          ylim = c(0, max(SDist)), ylab = "score distance",
          xlab = spectra$desc, sub = sub, main = "Possible PCA Outliers\nbased on Score Distance",
          pch = spectra$sym, ...
        )
      }
      abline(h = critSD, lty = 2)

      y.data <- subset(SDist, SDist > critSD)
      x.data <- which(SDist %in% y.data, arr.ind = TRUE)
      data <- cbind(x.data, y.data)
      if (!length(x.data) == 0) .labelExtremes(data, names = spectra$names[x.data], tol = 1.0)
    }

    if ("OD" %in% plot) {
      if (!use.sym) {
        plot(ODist,
          ylim = c(0, max(ODist)), ylab = "orthogonal distance",
          xlab = spectra$desc, sub = sub, main = "Possible PCA Outliers\nbased on Orthogonal Distance",
          col = spectra$colors, pch = 20, ...
        )
      }
      if (use.sym) {
        plot(ODist,
          ylim = c(0, max(ODist)), ylab = "orthogonal distance",
          xlab = spectra$desc, sub = sub, main = "Possible PCA Outliers\nbased on Orthogonal Distance",
          pch = spectra$sym, ...
        )
      }
      abline(h = critOD, lty = 2)

      y.data <- subset(ODist, ODist > critOD)
      x.data <- which(ODist %in% y.data, arr.ind = TRUE)
      data <- cbind(x.data, y.data)
      if (!length(x.data) == 0) .labelExtremes(data, names = spectra$names[x.data], tol = 1.0)
    }

    return(list(SDist = SDist, ODist = ODist, critSD = critSD, critOD = critOD))
  }

  if ((go == "ggplot2") || (go == "plotly")) {
    x <- y <- label <- NULL
    .chkReqGraphicsPkgs("ggplot2")

    if ("SD" %in% plot) {
      if (!use.sym) {
        x_index <- 1:length(SDist)
        df <- data.frame(x_index, SDist)

        p <- ggplot(df, aes(x = x_index, y = SDist)) +
          theme_bw() +
          geom_point(color = spectra$colors) +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
          ) +
          ylim(0, max(SDist)) +
          geom_hline(yintercept = critSD, linetype = "longdash") +
          xlab(sub) +
          ylab("score distance")
      }

      if (use.sym) {
        x_index <- 1:length(SDist)
        df <- data.frame(x_index, SDist)

        p <- ggplot(df, aes(x = x_index, y = SDist)) +
          theme_bw() +
          geom_point(color = "black", shape = spectra$sym) +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
          ) +
          ylim(0, max(SDist)) +
          geom_hline(yintercept = critSD, linetype = "longdash") +
          xlab(sub) +
          ylab("score distance")
      }

      y.data <- subset(SDist, SDist > critSD)
      x.data <- which(SDist %in% y.data, arr.ind = TRUE)
      data <- cbind(x.data, y.data)
      if (go == "ggplot2") {
        if (!length(x.data) == 0) {
          CoordList <- .getExtremeCoords(data, names = spectra$names[x.data], tol = 1.0)
          df <- data.frame(x = CoordList$x, y = CoordList$y, label = CoordList$l)
          p <- p + .ggRepel(df)
        }
        return(p)
      } else {
        .chkReqGraphicsPkgs("plotly")
        p <- ggplotly(p)
        CoordList <- .getExtremeCoords(data, names = spectra$names[x.data], tol = 1.0)
        df <- data.frame(x = CoordList$x, y = CoordList$y, label = CoordList$l)
        p <- p %>% add_annotations(
          x = df$x, y = df$y, text = df$label, xref = "x",
          yref = "y",
          showarrow = TRUE,
          arrowhead = 4,
          arrowsize = .5,
          ax = 10,
          ay = -15,
          font = list(
            size = 12
          )
        )
        return(p)
      }
    } # end of SD plot

    if ("OD" %in% plot) {
      if (!use.sym) {
        x_index <- 1:length(ODist)
        df <- data.frame(x_index, ODist)

        p <- ggplot(df, aes(x = x_index, y = ODist)) +
          theme_bw() +
          geom_point(color = spectra$colors) +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
          ) +
          ylim(0, max(max(ODist), critOD)) +
          geom_hline(yintercept = critOD, linetype = "longdash") +
          xlab(sub) +
          ylab("orthogonal distance")
      }

      if (use.sym) {
        x_index <- 1:length(ODist)
        df <- data.frame(x_index, ODist)

        p <- ggplot(df, aes(x = x_index, y = ODist)) +
          theme_bw() +
          geom_point(color = "black", shape = spectra$sym) +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
          ) +
          ylim(0, max(max(ODist), critOD)) +
          geom_hline(yintercept = critOD, linetype = "longdash") +
          xlab(sub) +
          ylab("score distance")
      }


      y.data <- subset(ODist, ODist > critOD)
      x.data <- which(ODist %in% y.data, arr.ind = TRUE)
      data <- cbind(x.data, y.data)
      if (go == "ggplot2") {
        if (!length(x.data) == 0) {
          CoordList <- .getExtremeCoords(data, names = spectra$names[x.data], tol = 1.0)
          df <- data.frame(x = CoordList$x, y = CoordList$y, label = CoordList$l)
          p <- p + .ggRepel(df)
        }
        return(p)
      } else {
        .chkReqGraphicsPkgs("plotly")
        p <- ggplotly(p)
        CoordList <- .getExtremeCoords(data, names = spectra$names[x.data], tol = 1.0)
        df <- data.frame(x = CoordList$x, y = CoordList$y, label = CoordList$l)
        p <- p %>% add_annotations(
          x = df$x, y = df$y, text = df$label, xref = "x",
          yref = "y",
          showarrow = TRUE,
          arrowhead = 4,
          arrowsize = .5,
          ax = 10,
          ay = -15,
          font = list(
            size = 12
          )
        )
        return(p)
      }
    } # end of OD plot
  } # end of go = "ggplot2"
}
