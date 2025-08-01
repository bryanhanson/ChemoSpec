#'
#'
#' Average Levels of a Factor in a Data Matrix
#'
#' \code{\link{avgFacLvls}} takes a matrix and calculates the column means for
#' each level of each factor given. It then replaces the original matrix rows
#' with the means corresponding to the factor/level memership of a particular
#' sample (row).
#'
#'
#' @param matrix A matrix.
#'
#' @param fac A vector of character strings with length = \code{nrow(matrix)}
#'
#' @return A matrix whose rows are composed of the column means for each level
#' of the factor.
#'
#' @author `r .writeDoc_Authors(c("BH", "MK"))
#'
#' @seealso \code{\link{aov_pcaSpectra}} for full details.
#' Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#'
#' M1 <- matrix(rnorm(100), nrow = 20, byrow = TRUE)
#' facs <- factor(c(rep("A", 5), rep("B", 5), rep("C", 5), rep("D", 5)))
#' M2 <- avgFacLvls(M1, fac = facs)
#'
.avgFacLvls <- function(matrix, fac) {
  # Script to replace rows of a matrix with the averages
  # of their group (as identified by fac)
  # Part of ChemoSpec/aov_pcaSpectra
  # Matthew Keinsley
  # DePauw University, May 2011

  M <- matrix

  if (!length(fac) == nrow(M)) {
    stop("Length of factor must equal number of rows in matrix")
  }

  lev <- levels(fac)

  for (i in 1:length(lev)) {
    w <- which(fac == lev[i])
    # cat("Loop #", i, "\n")
    # cat("  Level is  ", lev[i], "\n")
    # cat("  Length of level", i, "is", length(w), "\n")
    # cat("  w = ", w, "\n")
    m <- M[w, ] # submatrix for a given level
    avg <- colMeans(m)

    # Replace the spectra with the group averages
    for (j in 1:length(w)) M[w[j], ] <- avg
  }
  return(M)
}
