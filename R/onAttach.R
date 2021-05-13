#'
#' @noRd
#'
.onAttach <- function(libname, pkgname) {
  options(ChemoSpecGraphics = "base")

  packageStartupMessage("The ChemoSpec graphics option is set to 'base'")
  packageStartupMessage("To change it, do\n\toptions(ChemoSpecGraphics = 'option'),\n\twhere 'option' is one of 'base' or 'ggplot2'")
}