#'
#' Graphic Output Options in ChemoSpec
#'
#' In \code{ChemoSpec}, the user may chose from the following graphics output options:
#' \itemize{
#'   \item \code{base} graphics, the default (also the only style from the early days of \code{ChemoSpec} through version 5).
#'   \item \code{ggplot2} graphics.
#' }
#'
#' Here's how it works:
#' \itemize{
#'   \item Upon starting \code{ChemoSpec} the graphics output mode is set to \code{base}.
#'   \item To see the current value, do \code{\link[ChemoSpecUtils]{chkGraphicsOpt}}.  If by some chance the
#'         value is corrupted it will be set to \code{base}.
#'   \item To change the graphics output mode, do \code{options(ChemoSpecGraphics = 'option')},
#'         where 'option' is one of the options listed above.
#' }
#'
#' What you can do with your plots:
#' \itemize{
#'   \item Base graphics are the original graphics option in \code{R}.  They cannot be modified.
#'   \item For \code{ggplot2} graphics, \code{ChemoSpec} employs \code{theme_bw} with only a
#'         very few modifications.  You can add things to your plot, or override the theme
#'         used here via the usual \code{ggplot2} methods.  A few simple examples are given below
#'         but this is not the place for a \code{ggplot2} tutorial.  See \url{https://ggplot2.tidyverse.org/}
#'         for all things \code{ggplot2}.
#' }
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords utilities
#'
#' @docType data
#'
#' @name GraphicsOptions
#'
#' @examples
#' data(metMUD1)
#'
#' # Using ggplot2 graphics
#' options(ChemoSpecGraphics = "ggplot2")
#' p1 <- plotSpectra(metMUD1,
#'   which = c(10, 11), yrange = c(0, 1.5),
#'   offset = 0.06, amplify = 10, lab.pos = 0.5)
#' p1
#'
#' # Modifying ggplot2 graphics
#' library(ggplot2)
#' # Add a title
#' p2 <- p1 + ggtitle("metMUD1 NMR Data")
#' p2
#'
#' # Zoom the axes
#' p3 <- p1 + coord_cartesian(xlim = c(1.5, 2.0))
#' p3
#'
#' # Change the ggplot2 theme
#' p4 <- p1 + theme_gray() + theme(legend.position = "none")
#' p4
#'
#' # plotLoadings uses patchwork so to modify, use & and not +
#' pca <- c_pcaSpectra(SrE.IR)
#' p5 <- plotLoadings(SrE.IR, pca, loads = c(1, 2))
#' p5
#' p6 <- p5 & ggtitle("metMUD1 NMR Data") & theme_gray()
#' p6
#'
#' options(ChemoSpecGraphics = "base") # turn off for later examples/tests
#'
NULL
