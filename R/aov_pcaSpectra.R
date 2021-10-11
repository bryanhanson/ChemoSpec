#'
#' ANOVA-PCA Analysis of Spectra Data
#'
#' ANOVA-PCA is a combination of both methods developed by Harrington.  The
#' data is partitioned into submatrices corresponding to each experimental
#' factor, which are then subjected to PCA separately after adding the residual
#' error back.  If the effect of a factor is large compared to the residual
#' error, separation along the 1st PC in the score plot should be evident.
#' With this method, the significance of a factor can be visually determined
#' (ANOVA-PCA is not blind to group membership). ANOVA-PCA with only one factor
#' is the same as standard PCA and gives no additional separation.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param fac A vector of character strings giving the factors to be used in
#' the analysis.  These should be elements of \code{\link{Spectra}}.  Note that
#' there should be 2 or more factors, because ANOVA-PCA on one factor is the
#' same as standard PCA.  See the example.
#'
#' @param type Either classical ("cls") or robust ("rob"); Results in either
#' \code{\link{c_pcaSpectra}} or \code{\link{r_pcaSpectra}} being called on the
#' \code{\link{Spectra}} object.
#'
#' @param choice The type of scaling to be performed.  See
#' \code{\link{c_pcaSpectra}} and \code{\link{r_pcaSpectra}} for details.
#'
#' @param showNames  Logical.  Show the names of the submatrices in the console.
#'
#' @return A list of PCA results, one for each computed submatrix.
#'
#' @template authors-BH-MK
#'
#' @seealso The output of this function is used in
#' used in \code{\link{aovPCAscores}} and \code{\link{aovPCAloadings}}.
#' Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @references Pinto, Bosc, Nocairi, Barros, and Rutledge. "Using ANOVA-PCA for
#' Discriminant Analysis: ..." Analytica Chimica Acta 629.1-2 (2008): 47-55.
#'
#' Harrington, Vieira, Espinoza, Nien, Romero, and Yergey. "Analysis of
#' Variance--Principal Component Analysis: ..." Analytica Chimica Acta 544.1-2
#' (2005): 118-27.
#'
#' @keywords multivariate htest
#'
#' 
#' @examples
#' 
#' \dontrun{
#' # This example assumes the graphics output is set to ggplot2 (see ?GraphicsOptions).
#' library("ggplot2")
#' data(metMUD2)
#'
#' # Original factor encoding:
#' levels(metMUD2$groups)
#'
#' # Split those original levels into 2 new ones (re-code them)
#' new.grps <- list(geneBb = c("B", "b"), geneCc = c("C", "c"))
#' mM3 <- splitSpectraGroups(metMUD2, new.grps)
#'
#' # run aov_pcaSpectra
#' PCAs <- aov_pcaSpectra(mM3, fac = c("geneBb", "geneCc"))
#'
#' p1 <- aovPCAscores(mM3, PCAs, submat = 1, ellipse = "cls")
#' p1 <- p1 + ggtitle("aovPCA: B vs b")
#' p1
#'
#' p2 <- aovPCAscores(mM3, PCAs, submat = 2)
#' p2 <- p2 + ggtitle("aovPCA: C vs c")
#' p2
#'
#' p3 <- aovPCAscores(mM3, PCAs, submat = 3)
#' p3 <- p3 + ggtitle("aovPCA: Interaction Term")
#' p3
#'
#' p4 <- aovPCAloadings(spectra = mM3, PCA = PCAs)
#' p4 <- p4 + ggtitle("aov_pcaSpectra: Bb Loadings")
#' p4
#' }
#' @export
#'
aov_pcaSpectra <- function(spectra, fac, type = "class", choice = NULL, showNames = TRUE) {

  #  Function to conduct ANOVA-PCA per Harrington
  #  as explained by Pinto
  #  Matt Keinsley & Bryan Hanson
  #  DePauw University, Nov. 2010 onward (completed July 2011)

  .chkArgs(mode = 11L)

  types <- c("class", "rob")
  check <- type %in% types
  if (!check) {
    stop("PCA option invalid")
  }

  if (length(fac) > 3) {
    stop("Cannot process more than 3 factors!")
  }

  chkSpectra(spectra)

  nf <- length(fac)

  # Compute needed matrices
  # Naming of matrices follows Harrington 2005, at least some of the time
  # Hardwire all possible matrices for each factor

  MC <- scale(spectra$data, scale = FALSE) # mean centered

  if (nf == 1) {
    big <- list(DA = MC)
    flist <- list(spectra[[fac[1]]])
  }

  if (nf == 2) {
    big <- list(DA = MC, DB = MC, DAB = MC)
    flist <- list(
      fA = spectra[[fac[1]]], fB = spectra[[fac[2]]],
      fAB = interaction(spectra[[fac[1]]], spectra[[fac[2]]])
    )
  }

  if (nf == 3) {
    big <- list(DA = MC, DB = MC, DC = MC, DAB = MC, DAC = MC, DBC = MC)
    flist <- list(
      fA = spectra[[fac[1]]], fB = spectra[[fac[2]]],
      fC = spectra[[fac[3]]], fAB = interaction(spectra[[fac[1]]], spectra[[fac[2]]]),
      fAC = interaction(spectra[[fac[1]]], spectra[[fac[3]]]),
      fBC = interaction(spectra[[fac[2]]], spectra[[fac[3]]])
    )
  }

  # Check to see that we have more samples than degrees of freedom in the analysis.
  # This detail was worked out in correspondence with Jason James, Univ. of Washington.
  # If this is not checked, you get really strange plots but no errors (duplicated
  # or missing data points)

  lvlcnt <- 0L
  for (i in 1:length(flist)) lvlcnt <- lvlcnt + length(levels(flist[[i]]))
  if (lvlcnt >= length(spectra$names)) {
    msg <- paste("There are too many levels (", lvlcnt, ") in argument fac for the number of samples.", sep = "")
    stop(msg)
  }

  # Subtract matrices according to Pinto/Harrington
  # Run avgFacLvls on each successive residuals matrix
  # Number of times avgFacLvls runs depends on number of factors given

  # 1 factor aov_pcaSpectra is the same as running regular PCA so issue a warning
  if (nf == 1) {
    warning("aov_pcaSpectra is the same as ordinary PCA for 1 factor")
  }

  if (nf == 1) {
    big[[1]] <- .avgFacLvls(matrix = MC, flist[[1]])
    DAR <- MC - big[[1]]
    LM <- list(DA = big[[1]], DPE = DAR, MC = MC)
    names(LM) <- c(fac[1], "Res.Error", "MC Data")
  } # Harrington eqn (3)

  if (nf == 2) {
    big[[1]] <- .avgFacLvls(matrix = MC, flist[[1]])
    DAR <- MC - big[[1]]
    big[[2]] <- .avgFacLvls(matrix = DAR, flist[[2]])
    DBR <- DAR - big[[2]]
    big[[3]] <- .avgFacLvls(matrix = DBR, flist[[3]])
    DABR <- DBR - big [[3]]
    LM <- list(DA = big[[1]], DB = big[[2]], DAB = big[[3]], DPE = DABR, MC = MC)
    names(LM) <- c(fac[1], fac[2], paste(fac[1], "x", fac[2], sep = " "), "Res.Error", "MC Data")
  }

  if (nf == 3) {
    big[[1]] <- .avgFacLvls(matrix = MC, flist[[1]])
    DAR <- MC - big[[1]]
    big[[2]] <- .avgFacLvls(matrix = DAR, flist[[2]])
    DBR <- DAR - big[[2]]
    big[[3]] <- .avgFacLvls(matrix = DBR, flist[[3]])
    DCR <- DBR - big[[3]]
    big[[4]] <- .avgFacLvls(matrix = DCR, flist[[4]])
    DABR <- DCR - big[[4]]
    big[[5]] <- .avgFacLvls(matrix = DABR, flist[[5]])
    DACR <- DABR - big[[5]]
    big[[6]] <- .avgFacLvls(matrix = DACR, flist[[6]])
    DBCR <- DACR - big[[6]]
    LM <- list(
      DA = big[[1]], DB = big[[2]], DC = big[[3]], DAB = big[[4]],
      DAC = big[[5]], DBC = big[[6]], DPE = DBCR, MC = MC
    )
    names(LM) <- c(
      fac[1], fac[2], fac[3], paste(fac[1], "x", fac[2], sep = " "),
      paste(fac[1], "x", fac[3], sep = " "), paste(fac[2], "x", fac[3], sep = " "),
      "Res.Error", "MC Data"
    )
  }

  if (showNames) cat("The submatrices are:", paste(names(LM), collapse = ", "), "\n")

  # Now carryout the PCA (do so for each submatrix)

  n_pca <- length(LM) - 2 # leave off Res.Error, MC Data
  PCA <- vector("list", n_pca)
  names(PCA) <- names(LM)[1:n_pca]
  for (i in 1:n_pca) {
    spectra$data <- LM[[i]] + LM$Res.Error
    if (is.null(choice)) choice <- "noscale"
    if (type == "class") PCA[[i]] <- c_pcaSpectra(spectra, choice = choice, cent = FALSE)
    if (type == "rob") PCA[[i]] <- r_pcaSpectra(spectra, choice = choice)
  }

  return(PCA)
}
