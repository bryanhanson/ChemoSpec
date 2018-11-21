#'
#' Determine if a Number is a Whole Number
#' 
#' This function determines if a given number is a whole number within a given
#' tolerance.  Taken from the help page of \code{\link{is.integer}}.  An
#' internal function, not generally called by the user.
#' 
#' @param x A number to be tested.
#' 
#' @param tol Tolerance for the test.
#' 
#' @return A logical, indicating the outcome of the test.
#' 
#' @author Bryan A. Hanson, DePauw University. Carved out of \code{\link{is.integer}}.
#' 
#' @seealso \code{\link{is.integer}}
#' 
#' @keywords utilities
#' 
#' @export
#' @noRd
#'
.isWholeNo <- function(x, tol = .Machine$double.eps^0.5)  {	
	abs(x - round(x)) < tol
}


