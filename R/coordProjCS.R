coordProjCS <- 
function (data, dimens = c(1, 2), parameters = NULL, z = NULL, 
    classification = NULL, truth = NULL, uncertainty = NULL, 
    what = c("classification", "errors", "uncertainty"), quantiles = c(0.75, 
        0.95), symbols = NULL, colors = NULL, scale = FALSE, 
    xlim = NULL, ylim = NULL, CEX = 1, PCH = ".", identify = FALSE, 
    ...) 
{

	if (!requireNamespace("mclust", quietly = TRUE)) {
		stop("You need to install package mclust to use this function")
	}
	
	# This is a modified version of coordProj{mclust} for use in ChemoSpec
	# Part of the ChemoSpec package
	# Bryan Hanson, DePauw Univ, Dec 2009
	
    if (is.null(dimens)) 
        dimens <- c(1, 2)
    if (is.null(classification) && !is.null(z)) 
        classification <- mclust::map(z)
    if (is.null(uncertainty) && !is.null(z)) 
        uncertainty <- 1 - apply(z, 1, max)
    if (!is.null(parameters)) {
        mu <- parameters$mean
        L <- ncol(mean)
        sigma <- parameters$variance$sigma
        haveParams <- !is.null(mu) && !is.null(sigma) && !any(is.na(mu)) && 
            !any(is.na(sigma))
    }
    else haveParams <- FALSE
    data <- data[, dimens, drop = FALSE]
    if (dim(data)[2] != 2) 
        stop("need two dimensions")
    if (is.null(xlim)) 
        xlim <- range(data[, 1])
    if (is.null(ylim)) 
        ylim <- range(data[, 2])
    if (scale) {
        par(pty = "s")
        d <- diff(xlim) - diff(ylim)
        if (d > 0) {
            ylim <- c(ylim[1] - d/2, ylim[2] + d/2)
        }
        else {
            xlim <- c(xlim[1] + d/2, xlim[2] - d/2)
        }
    }
    if (is.null(dnames <- dimnames(data)[[2]])) 
        xlab <- ylab <- ""
    else {
        xlab <- dnames[1]
        ylab <- dnames[2]
    }
    if (haveParams) {
        G <- ncol(mu)
        dimpar <- dim(sigma)
        if (length(dimpar) != 3) {
            haveParams <- FALSE
            warning("covariance must be a 3D matrix")
        }
        if (G != dimpar[3]) {
            haveParams <- FALSE
            warning("means and variance parameters are incompatible")
        }
        mu <- array(mu[dimens, ], c(2, G))
        sigma <- array(sigma[dimens, dimens, ], c(2, 2, G))
    }
    if (!is.null(truth)) {
        if (is.null(classification)) {
            classification <- truth
            truth <- NULL
        }
    }
    if (!is.null(classification)) {
        classification <- as.character(classification)
        U <- sort(unique(classification))
        L <- length(U)
        noise <- classification[1] == "0"
        if (is.null(symbols)) {
            if (L <= length(mclust::mclust.options("classPlotSymbols"))) {
                symbols <- mclust::mclust.options("classPlotSymbols")
                if (noise) {
                  first <- symbols[1]
                  symbols[symbols == 16] <- first
                  symbols[1] <- 16
                }
            }
            else if (L <= 9) {
                symbols <- as.character(1:9)
            }
            else if (L <= 26) {
                symbols <- LETTERS
            }
        }
        else if (length(symbols) == 1) 
            symbols <- rep(symbols, L)
        if (is.null(colors)) {
            if (L <= length(mclust::mclust.options("classPlotColors"))) {
                colors <- mclust::mclust.options("classPlotColors")[1:L]
                if (noise) {
                  first <- colors[1]
                  colors[colors == "black"] <- first
                  colors[1] <- "black"
                }
            }
        }
        else if (length(colors) == 1) 
            colors <- rep(colors, L)
        if (length(symbols) < L) {
            warning("more symbols needed to show classification ")
            symbols <- rep(16, L)
        }
        if (length(colors) < L) {
            warning("more colors needed to show classification ")
            colors <- rep("black", L)
        }
    }
    if (length(what) > 1) 
        what <- what[1]
    choices <- c("classification", "errors", "uncertainty")
    m <- charmatch(what, choices, nomatch = 0)
    if (m) {
        what <- choices[m]
        bad <- what == "classification" && is.null(classification)
        bad <- bad || (what == "uncertainty" && is.null(uncertainty))
        bad <- bad || (what == "errors" && (is.null(classification) || 
            is.null(truth)))
        if (bad) 
            warning("insufficient input for specified plot")
        badClass <- (what == "errors" && (length(unique(classification)) != 
            length(unique(truth))))
        if (badClass && !bad) 
            warning("classification and truth differ in number of groups")
        bad <- bad && badClass
    }
    else {
        bad <- !m
        warning("what improperly specified")
    }
    if (bad) 
        what <- "bad"
    switch(EXPR = what, classification = {
        plot(data[, 1], data[, 2], type = "n", xlab = xlab, ylab = ylab, 
            xlim = xlim, ylim = ylim, main = "", ...)
        if (identify) {
            TITLE <- paste(paste(dimens, collapse = ","), "Coordinate Projection showing Classification")
            title(main = TITLE)
        }
        for (k in 1:L) {
            I <- classification == U[k]
            points(data[I, 1], data[I, 2], pch = symbols[k], 
                col = colors[k], cex = if (U[k] == "0") 
                  CEX/4
                else CEX)
        }
    }, errors = {
        ERRORS <- mclust::classError(classification, truth)$misclassified
        plot(data[, 1], data[, 2], type = "n", xlab = xlab, ylab = ylab, 
            xlim = xlim, ylim = ylim, main = "", ...)
        if (identify) {
            TITLE <- paste(paste(dimens, collapse = ","), "Coordinate Projection showing Errors")
            title(main = TITLE)
        }

### MODIFICATIONS BEGIN HERE ###
# Changed so that points in error are the normal points, but crossed out
# by overplotting with X
# commented code is from the original

#        CLASSES <- unique(as.character(truth))
#        symOpen <- c(2, 0, 1, 5)
#        symFill <- c(17, 15, 16, 18)
        good <- rep(TRUE, length(classification))
        good[ERRORS] <- FALSE

# next chunk is copied from above, under "classification"
  
        for (k in 1:L) {
            I <- classification == U[k]
            points(data[I, 1], data[I, 2], pch = symbols[k], 
                col = colors[k], cex = if (U[k] == "0") 
                  CEX/4
                else CEX)
        	}
        	
# now overplot the misclassifed points

		points(data[!good, 1], data[!good, 2], pch = 4, col = "black")
        
#        if (L > 4) {
#            points(data[good, 1], data[good, 2], pch = 1, col = colors, 
#                cex = CEX)
#            points(data[!good, 1], data[!good, 2], pch = 16, 
#                cex = CEX)
#        }
#        else {
#            for (k in 1:L) {
#                K <- truth == CLASSES[k]
#                if (any(I <- (K & good))) {
#                  points(data[I, 1], data[I, 2], pch = symOpen[k], 
#                    col = colors[k], cex = CEX)
#                }
#                if (any(I <- (K & !good))) {
#                  points(data[I, 1], data[I, 2], pch = symFill[k], 
#                    cex = CEX)
#                }
#            }
#        }
    }, uncertainty = {
        plot(data[, 1], data[, 2], type = "n", xlab = xlab, ylab = ylab, 
            xlim = xlim, ylim = ylim, main = "", ...)
        if (identify) {
            TITLE <- paste(paste(dimens, collapse = ","), "Coordinate Projection showing Uncertainty")
            title(main = TITLE)
        }
        breaks <- quantile(uncertainty, probs = sort(quantiles))
        I <- uncertainty <= breaks[1]
        points(data[I, 1], data[I, 2], pch = 16, col = "gray75", 
            cex = 0.5 * CEX)
        I <- uncertainty <= breaks[2] & !I
        points(data[I, 1], data[I, 2], pch = 16, col = "gray50", 
            cex = 1 * CEX)
        I <- uncertainty > breaks[2] & !I
        points(data[I, 1], data[I, 2], pch = 16, col = "black", 
            cex = 1.5 * CEX)
    }, {
        plot(data[, 1], data[, 2], type = "n", xlab = xlab, ylab = ylab, 
            xlim = xlim, ylim = ylim, main = "", ...)
        if (identify) {
            TITLE <- paste(paste(dimens, collapse = ","), "Coordinate Projection")
            title(main = TITLE)
        }
        points(data[, 1], data[, 2], pch = PCH, cex = CEX)
    })
    if (haveParams) {
        for (k in 1:G) mclust::mvn2plot(mu = mu[, k], sigma = sigma[, 
            , k], k = 15)
    }
    invisible()
}
