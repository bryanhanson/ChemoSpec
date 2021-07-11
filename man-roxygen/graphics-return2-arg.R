#' @return
#' The returned value depends on the graphics option selected (see \code{\link{GraphicsOptions}}).
#' \describe{
#'  \item{base:}{A data frame or list containing the data plotted.  Assign the value and run \code{str()}
#'               or \code{names()} on it to see what it contains. Side effect is a plot.}
#'  \item{ggplot2:}{The plot is displayed, and a \code{ggplot2} plot object is returned if the
#'                  value is assigned. The plot can be modified in the usual \code{ggplot2} manner.
#'                  If you want the plotted values, you can access them via the base graphics mode.}
#' }
