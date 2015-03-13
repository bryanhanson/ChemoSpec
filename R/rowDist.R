rowDist <- function(x, method) {

# Function to access a variety of distance methods which turn out to be in different packages,
# except for cosine which is handled here.
# Some code suggested by Roberto Canteri, and used with extremely minor modifications
# Part of ChemoSpec, October 2011,  Bryan Hanson, DePauw University

	method <- match.arg(method, c("pearson", "correlation", "spearman", "kendall",
		"euclidean", "maximum", "manhattan", "canberra","binary", "minkowski",
		"cosine"))

	if (method %in% c("pearson", "correlation", "spearman", "kendall")) {
		if (!requireNamespace("amap", quietly = TRUE)) {
			stop("You need to install package amap to use this function/option")
			}
		distance <- amap::Dist(x, method = method)
		}
		
	if (method %in% c("euclidean", "maximum", "manhattan", "canberra","binary", "minkowski")) {
		distance <- dist(x, method = method)
		}
		
	if ( method == "cosine") { # code by Claudia Beleites/unmixR w/small modifications
		x <- as.matrix(x)
		x <- tcrossprod(x)
		l <- rowSums(x^2)
		l <- sqrt(outer(l, l))
		distance <- as.dist(x/l)
		}

	return(distance)
	}
