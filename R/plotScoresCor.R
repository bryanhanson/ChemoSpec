plotScoresCor <-
function (x, quan = 1/2, alpha = 0.025) {

# Function to plot robust correlation ellipsoids for x, y data
# Modifed slightly from cor.plot {mvoutlier}
# Part of the ChemoSpec Package
# Bryan Hanson, DePauw University, Aug 2009

	if (!requireNamespace("robustbase", quietly = TRUE)) {
		stop("You need to install package robustbase to use this function")
	}

	x <- as.matrix(x)
    covr <- robustbase::covMcd(x, cor = TRUE, alpha = quan)
    cov.svd <- svd(cov(x), nv = 0)
    covr.svd <- svd(covr$cov, nv = 0)
    r <- cov.svd[["u"]] %*% diag(sqrt(cov.svd[["d"]]))
    rr <- covr.svd[["u"]] %*% diag(sqrt(covr.svd[["d"]]))
    e <- cbind(cos(c(0:100)/100 * 2 * pi) * sqrt(qchisq(1 - alpha, 
        2)), sin(c(0:100)/100 * 2 * pi) * sqrt(qchisq(1 - alpha, 
        2)))
    tt <- t(r %*% t(e)) + rep(1, 101) %o% apply(x, 2, mean)
    ttr <- t(rr %*% t(e)) + rep(1, 101) %o% covr$center
    
   	ellipse <- list()
   	ellipse$x.cls <- tt[,1]
   	ellipse$y.cls <- tt[,2]
   	ellipse$c <- round(cor(x)[1, 2], 2)
   	ellipse$x.rob <- ttr[,1]
   	ellipse$y.rob <- ttr[,2]
   	ellipse$r <- round(covr$cor[1, 2], 2)
   	ellipse

 }

