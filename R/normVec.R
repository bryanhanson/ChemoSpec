#'
#' Normalize a Vector to range -1 to +1
#' 
#' Each value of the vector passed to the function is divided by the square
#' root of the sum of every value squared, producing a new vector whose range
#' is restricted to, at most, -1 to +1.  Note that this assumes that the mean
#' of the original vector is zero. An internal function, not generally called
#' by the user.
#' 
#' @param x A numeric argument whose values are to be normalized.
#'
#' @return The normalized vector.
#'
#' @note The idea was taken from "An Introduction to rggobi" found at the ggobi
#' web site (originally www.ggobi.org but not available as of June 2018).
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @keywords utilities manip
#'
#' @examples
#' 
#' x1 <- rnorm(20, 2, 2)
#' range(x1)
#' sd(x1)/diff(range(x1))
#'
#' x2 <- .normVec(x1)
#' range(x2)
#' sd(x2)/diff(range(x2))
#' 
#' 
#' @export
#' @noRd
#'
.normVec <- function(x) x/sqrt(sum(x^2))
