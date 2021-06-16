#'
#' Graphic Output Options in ChemoSpec
#'
#' In \code{ChemoSpec}, the user may chose from the following graphics output options:
#' \itemize{
#'   \item \code{base} graphics, the default (also the style from the early days of \code{ChemoSpec}).
#'   \item \code{ggplot2} graphics.
#' }
#'
#' Here's how it works:
#' \itemize{
#'   \item Upon starting \code{ChemoSpec} the graphics output mode is set to \code{base}.
#'   \item To see the current value, do \code{chkGraphicsOpt()}.  If by some chance the
#'         value is corrupted it will be set to \code{base}.
#'   \item To change the graphics output mode, do \code{options(ChemoSpecGraphics = 'option')},
#'         where 'option' is one of the options listed above.
#' }
#'
#' @section Details:
#' \itemize{
#'   \item Base graphics are the original graphics option in \code{R}.  They cannot be modified.
#'   \item For \code{ggplot2} graphics, \code{ChemoSpec} employs \code{theme_bw} with only a
#'         very few modifications.  You can add things to your plot, or override the theme
#'         used here via the usual \code{ggplot2} methods.
#' }
#'
#' @docType data
#' 
#' @name GraphicsOptions
#' 
NULL
