
isWholeNo <- function(x, tol = .Machine$double.eps^0.5)  {
	
# Taken from the help to is.integer()
# Bryan Hanson, DePauw Univ, Nov 2009
# Used by binSpec in ChemoSpec
	
	abs(x - round(x)) < tol
	
	}
