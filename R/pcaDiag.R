pcaDiag <-
function (spectra, pca, pcs = 3, quantile = 0.975,
plot = c("OD", "SD"), use.sym = FALSE, ...) {

# Function for PCA Diagnostic Plots (modified from Filzmosers version in {chemometrics})
# Part of the ChemoSpec package.  Bryan Hanson, DePauw Univ, Sept 2009

	if ("prcomp" %in% class(pca)) pca <- q2rPCA(pca)
	X <- spectra$data
	X.pca <- pca
	a <- pcs
    if (is.null(a)) a <- 3
    
	SDist <- sqrt(apply(t(t(X.pca$sco[, 1:a]^2)/X.pca$sdev[1:a]^2), 1, sum))
    ODist <- sqrt(apply((X - X.pca$sco[, 1:a] %*% t(X.pca$loa[, 1:a]))^2, 1, sum))
    critSD <- sqrt(qchisq(quantile, a))
    critOD <- (median(ODist^(2/3)) + mad(ODist^(2/3)) * qnorm(quantile))^(3/2)
    
    sub <- paste(pca$method, a, "PCs", sep = " ")
    if ("SD" %in% plot) {
		if (!use.sym) {
			plot(SDist, ylim = c(0, max(SDist)), ylab = "score distance", 
			xlab = spectra$desc, sub = sub, main = "Possible PCA Outliers based on Score Distance",
			col = spectra$colors, pch = 20, ...)
			}
		if (use.sym) {
			plot(SDist, ylim = c(0, max(SDist)), ylab = "score distance", 
			xlab = spectra$desc, sub = sub, main = "Possible PCA Outliers based on Score Distance",
			pch = spectra$sym, ...)
			}
		abline(h = critSD, lty = 2)
	
		y.data <- subset(SDist, SDist > critSD)
		x.data <- which(SDist %in% y.data, arr.ind = TRUE)
		data <- cbind(x.data, y.data)
		if (!length(x.data) == 0) labelExtremes(data, names = spectra$names[x.data], tol = 1.0)
        }
        
    if ("OD" %in% plot) {
		if (!use.sym) {
			plot(ODist, ylim = c(0, max(ODist)), ylab = "orthogonal distance", 
			xlab = spectra$desc, sub = sub, main = "Possible PCA Outliers based on Orthogonal Distance",
			col = spectra$colors, pch = 20, ...)
			}
		if (use.sym) {
			plot(ODist, ylim = c(0, max(ODist)), ylab = "orthogonal distance", 
			xlab = spectra$desc, sub = sub, main = "Possible PCA Outliers based on Orthogonal Distance",
			pch = spectra$sym, ...)
				}
		abline(h = critOD, lty = 2)

		y.data <- subset(ODist, ODist > critOD)
		x.data <- which(ODist %in% y.data, arr.ind = TRUE)
		data <- cbind(x.data, y.data)
		if (!length(x.data) == 0) labelExtremes(data, names = spectra$names[x.data], tol = 1.0)
		}
		
    list(SDist = SDist, ODist = ODist, critSD = critSD, critOD = critOD)
	}

