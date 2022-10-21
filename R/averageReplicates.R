#'
#' Average Replicates in a Spectra Object
#'
#' Average the replicates in a \code{\link{Spectra}} object and return a new Spectra object
#' with fewer samples.  One should probably not do this until each individual sample has been
#' visualized for quality control, in case it is a potential outlier.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}} to be processed.
#'
#' @param uniq Character.  A character vector containing strings representing unique sample
#'        identifiers.  The sample names will be searched for these strings, and all samples
#'        matching a given string will be averaged and put into a new Spectra object.  For
#'        example, consider the case where samples are named
#'        \code{S_1_01, S_1_02, ..., S_2_01, S_2_02, ...}
#'        where \code{_01} and so forth signifies replicates of a particular sample.
#'        With \code{uniq = c("S_1", "S_2")} all \code{S_1} replicates will be averaged and
#'        all \code{S_2} replicates will be averaged.  N.B. the strings will be used as
#'        regex pattern and grepped.
#'
#' @return An object of S3 class \code{\link{Spectra}}.
#'
#' @template authors-BH
#'
#' @keywords utilities
#' @export
#'
#' @examples
#' data(SrE.IR)
#' averaged <- averageReplicates(SrE.IR, uniq = c("EPO", "OO", "adSrE", "pSrE"))
#' sumSpectra(SrE.IR)
#' sumSpectra(averaged)
#'
averageReplicates <- function(spectra, uniq) {

  .chkArgs(mode = 11L)
  chkSpectra(spectra)

  ns <- nrow(spectra$data)
  nu <- length(uniq)
  cnt <- 0L # keep track so we know if any samples have been missed

  # Set up a new Spectra object to hold the results
  spec <- list()
  spec$freq <- spectra$freq
  spec$data <- matrix(NA_real_, nrow = nu, ncol = length(spec$freq))
  spec$names <- rep(NA_character_, nu)
  spec$groups <- rep(NA_character_, nu)
  spec$colors <- rep(NA_character_, nu)
  spec$sym <- rep(NA_integer_, nu)
  spec$alt.sym <- rep(NA_character_, nu)
  spec$unit <- spectra$unit
  spec$desc <- paste(spectra$desc, "(replicates averaged)", sep = " ")

  # Fill the new Spectra object
  for (i in 1:length(uniq)) {
    avg <- grep(uniq[i], spectra$names)
    navg <- length(avg)
    if (navg == 0L) stop(paste("Did not find sample names matching ", uniq[i]))

    cnt <- cnt + navg
    if (navg >= 2L) spec$data[i,] <- colMeans(spectra$data[avg,])
    if (navg == 1L) spec$data[i,] <- spectra$data[avg,]
    spec$names[i] <- uniq[i]
    spec$groups[i] <- as.character(spectra$groups[avg[1]])
    spec$colors[i] <- spectra$colors[avg[1]]
    spec$sym[i] <- spectra$sym[avg[1]]
    spec$alt.sym[i] <- spectra$alt.sym[avg[1]]  
  }

  if (ns != cnt) warning("Looks like some samples did not get included (check entries in uniq)")

  spec$groups <- as.factor(spec$groups)
  class(spec) <- "Spectra"
  chkSpectra(spec)
  return(spec)
}

