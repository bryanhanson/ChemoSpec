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
#' @param \dots Additional parameters to be passed to the plotting functions.
#'
#' @return A list is returned as described in
#' \code{\link[chemometrics]{pcaDiagplot}}, so the result must be assigned or
#' it will appear at the console.  Side effect is a plot.
#'
#' @seealso Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso \code{\link[chemometrics]{pcaDiagplot}} in package
#' \code{chemometrics} for the underlying function.
#'
#' @references K. Varmuza and P. Filzmoser \emph{Introduction to Multivariate
#' Statistical Analysis in Chemometrics}, CRC Press, 2009.
#'
#' @keywords multivariate
#'
#' @examples
#'
#' data(SrE.IR)
#' res <- c_pcaSpectra(SrE.IR, choice = "noscale")
#' temp <- pcaDiag(SrE.IR, res, pcs = 2, plot = "OD")
#' temp <- pcaDiag(SrE.IR, res, pcs = 2, plot = "SD")
#' @export pcaDiag
#'
#' @importFrom stats qchisq median mad qnorm
#' @importFrom graphics plot abline
#'
pcaDiag <-
  function(spectra, pca, pcs = 3, quantile = 0.975,
           plot = c("OD", "SD"), use.sym = FALSE, ...) {
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

    sub <- paste(pca$method, a, "PCs", sep = " ")
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

    list(SDist = SDist, ODist = ODist, critSD = critSD, critOD = critOD)
  }
