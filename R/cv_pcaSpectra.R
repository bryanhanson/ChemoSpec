#'
#' Cross-Validation of Classical PCA Results for a Spectra Object
#'
#' This function carries out classical PCA on the data in a
#' \code{\link{Spectra}} object using a cross-validation method.  A simple
#' re-write of Peter Filzmoser's \code{\link[chemometrics]{pcaCV}} method
#' with some small plotting changes.
#'
#' @param spectra `r .writeDoc_Spectra1()`
#'
#' @param choice A character string indicating the choice of scaling.  One of
#' \code{c("noscale"}, \code{"autoscale"}, \code{"Pareto")}.
#'
#' @param pcs As per \code{\link[chemometrics]{pcaCV}} where it is called amax;
#' an integer giving the number of PC scores to include.
#'
#' @param repl As per \code{\link[chemometrics]{pcaCV}}; the number of
#' replicates to perform.
#'
#' @param segments As per \code{\link[chemometrics]{pcaCV}}.
#'
#' @param segment.type As per \code{\link[chemometrics]{pcaCV}}.
#'
#' @param length.seg As per \code{\link[chemometrics]{pcaCV}}.
#'
#' @param trace As per \code{\link[chemometrics]{pcaCV}}.
#'
#' @param \dots Parameters to be passed to the plotting routines.
#'
#' @return Invisibly, a list as described in \code{\link[chemometrics]{pcaCV}}.
#' Side effect is a plot.
#'
#' @author Bryan A. Hanson, DePauw University. Derived from \code{\link[chemometrics]{pcaCV}}.
#'
#' @seealso \code{\link[chemometrics]{pcaCV}} for the underlying function.
#' Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @references K. Varmuza and P. Filzmoser \emph{Introduction to Multivariate
#' Statistical Analysis in Chemometrics}, CRC Press, 2009.
#'
#' @keywords multivariate
#'
#' @examples
#' # You need to install package "pls" for this example
#' if (requireNamespace("pls", quietly = TRUE)) {
#'   data(SrE.IR)
#'   pca <- cv_pcaSpectra(SrE.IR, pcs = 5)
#' }
#'
#' @export cv_pcaSpectra
#' @importFrom stats sd
#' @importFrom graphics boxplot legend
#'
cv_pcaSpectra <- function(spectra, pcs, choice = "noscale", repl = 50, segments = 4,
                          segment.type = c("random", "consecutive", "interleaved"),
                          length.seg, trace = FALSE, ...) {
  .chkArgs(mode = 11L)
  chkSpectra(spectra)

  if (.chkReqPkgs("pls")) {
    choices <- c("noscale", "autoscale", "Pareto") # trap for invalid scaling method
    check <- choice %in% choices
    if (!check) stop("The choice of scaling parameter was invalid")
    chkSpectra(spectra)

    # Center & scale the data using the desired method.

    if (identical(choice, "noscale")) {
      centscaled <- scale(spectra$data, center = TRUE, scale = FALSE)
    }

    if (identical(choice, "Pareto")) {
      col.sd <- apply(spectra$data, 2, sd)
      centscaled <- scale(spectra$data, center = TRUE, scale = col.sd^0.5)
    }

    if (identical(choice, "autoscale")) {
      col.sd <- apply(spectra$data, 2, sd)
      centscaled <- scale(spectra$data, center = TRUE, scale = col.sd)
    }

    # Filzmoser's stuff follows... (modified slightly)

    X <- centscaled
    amax <- pcs
    if (missing(amax)) {
      amax <- 5
    } else {
      if (amax < 1 || amax > min(nrow(X) - 1, ncol(X))) {
        stop("Invalid number of components, amax")
      }
    }
    amax <- min(amax, nrow(X) - max(sapply(segments, length)) -
      1)
    optcomp <- matrix(NA, nrow = segments, ncol = repl)
    MSEP <- matrix(NA, nrow = repl, ncol = amax)
    dimnames(MSEP) <- list(paste("rep", 1:repl), paste(
      "PC",
      1:amax
    ))
    Fit <- matrix(NA, nrow = repl, ncol = amax)
    dimnames(Fit) <- list(paste("rep", 1:repl), 1:amax)
    for (i in 1:repl) {
      if (missing(length.seg)) {
        segment <- pls::cvsegments(nrow(X), k = segments, type = segment.type)
      } else {
        segment <- pls::cvsegments(nrow(X),
          length.seg = length.seg,
          type = segment.type
        )
      }
      if (trace) {
        cat(paste("Replication: ", i))
      }
      MSEPj <- matrix(NA, nrow = segments, ncol = amax)
      Fitj <- matrix(NA, nrow = segments, ncol = amax)
      for (n.seg in 1:length(segment)) {
        if (trace) {
          cat(n.seg, "")
        }
        seg <- segment[[n.seg]]
        obsuse <- as.numeric(unlist(segment[-n.seg]))
        Xtrain <- X[obsuse, ]
        obstest <- as.numeric(unlist(segment[n.seg]))
        Xtest <- X[obstest, ]
        if (ncol(Xtrain) > nrow(Xtrain)) {
          e <- eigen(Xtrain %*% t(Xtrain))
          Ttrain <- e$vectors %*% diag(sqrt(e$values))
          Ptrain <- t(Xtrain) %*% Ttrain %*% diag(1 / e$values)
        } else {
          Xtrain_svd <- svd(Xtrain)
          Ptrain <- Xtrain_svd$v
        }
        Ttest <- Xtest %*% Ptrain
        for (j in 1:amax) {
          MSEPj[n.seg, j] <- sum((Xtest - Ttest[, 1:j] %*%
            t(Ptrain[, 1:j]))^2)
        }
        Fitj[n.seg, ] <- MSEPj[n.seg, ] / sum(Xtest^2)
      }
      MSEP[i, ] <- apply(MSEPj, 2, mean)
      Fit[i, ] <- 1 - apply(Fitj, 2, mean)
    }

    # The plotting details have been modified quite a bit

    boxplot(as.data.frame(Fit),
      ylab = "Explained variance",
      xlab = "Number of components", ...
    )
    # construct a legend based upon values of center & scale
    note <- paste("centered/", choice, "/", "classical", sep = "")
    legend("bottomright", note, bty = "n", cex = 0.75)

    invisible(list(ExplVar = Fit, MSEP = MSEP))
  }
}
