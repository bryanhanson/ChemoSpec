rowDist <- function(x, method) {

# Function to access a variety of distance methods which turn out to be in different packages
# Suggested by Roberto Canteri, and used with extremely minor modifications
# Part of ChemoSpec, October 2011,  Bryan Hanson, DePauw University

	method <- match.arg(method, c("pearson", "correlation", "spearman", "kendall",
		"euclidean", "maximum", "manhattan", "canberra","binary", "minkowski"))

	if ( method %in% c("pearson", "correlation", "spearman", "kendall") ) {
		distance <- Dist(x, method = method)
		} else {distance <- dist(x, method = method)}
	return(distance)
	}
