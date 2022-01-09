#' Autocompletion for Spectra Objects
#'
#' Command line completion for Spectra objects.
#'
#' @param x The \code{Spectra} object.
#' @param pattern String.  The pattern to search for.
#' @return String. A vector of element names.
#' @export
#' @importFrom utils .DollarNames
#' @noRd
#'
.DollarNames.Spectra <- function(x, pattern = "") {
  grep(pattern, names(x), value = TRUE)
}
