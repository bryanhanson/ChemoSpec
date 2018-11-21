#'
#' Plot Measures of Central Tendency and Spread for a Spectra Object
#' 
#' Compute and plot various measures of central tendency and
#' spread for a \code{\link{Spectra}} object.  Several different measures/spreads
#' are available.  These are useful as an overview of where a data set varies
#' the most.
#' 
#' For \code{surveySpectra} the method choice works as follows: \code{sd} plots
#' the mean spectrum +/- the standard deviation, \code{sem} plots the mean
#' spectrum +/- the standard error of the mean, \code{sem95} plots the mean
#' spectrum +/- the standard error at the 95 percent confidence interval,
#' \code{mad} plots the median spectrum +/- the median absolute deviation, and
#' finally, \code{iqr} plots the median spectrum + the upper hinge and - the
#' lower hinge.
#' 
#' For \code{surveySpectra2}, the spectra are mean centered and plotted.  Below
#' that, the relative summary statistic is plotted, offset, but on the same
#' scale.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}} to be analyzed.
#'
#' @param method Character.  One of \code{c("sd", "sem", "sem95", "mad",
#' "iqr")}.
#'
#' @param by.gr Logical, indicating if the analysis is to be done by group or
#' not. Applies to \code{surveySpectra} only.
#'
#' @param \dots Additional parameters to be passed to the plotting routines.
#'
#' @param lab.pos Numeric, giving the frequency where the label should be drawn.
#' Applies to \code{surveySpectra2} only.
#'
#' @seealso Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @return None; side effect is a plot
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @keywords hplot
#'
#' @examples
#' 
#' data(SrE.IR)
#' myt <- expression(bolditalic(Serenoa)~bolditalic(repens)~bold(Extract~IR~Spectra))
#' surveySpectra(SrE.IR, method = "iqr", main = myt)
#' surveySpectra2(SrE.IR, method = "iqr", main = myt)
#'
#' @export surveySpectra surveySpectra2
#'
#' @importFrom stats sd
#' @importFrom graphics plot lines text
#' @importFrom plyr aaply
#' @importFrom ChemoSpecUtils sumGroups
#'
#' @describeIn surveySpectra Spectral survey emphasizing mean or median spectrum, optionally by group.
#' 
surveySpectra <- function(spectra, method = c("sd", "sem", "sem95", "mad", "iqr"),
	by.gr = TRUE,  ...) {

	if (!requireNamespace("lattice", quietly = TRUE)) {
		stop("You need to install package lattice to use this function")
	}

	.chkArgs(mode = 11L)
	chkSpectra(spectra)
	
	# Organize and compute the requested data
	
	if (!by.gr) {
		x <- spectra$freq
		if (method == "iqr"){
			y <- aaply(spectra$data, 2, .seXyIqr)
			df <- data.frame(x, y1 = y[,1], y2 = y[,2], y3 = y[,3])
			p <- lattice::xyplot(y1 + y2 + y3 ~ x, data = df,
				col = c("black", "red", "red"), xlab = spectra$unit[1],
				ylab = "Full Data Set, median +/- iqr", type = "l", ...)
			plot(p)
			}
		if (method == "sd") {
			y1 <- aaply(spectra$data, 2, mean)
			s <- aaply(spectra$data, 2, sd)
			y2 <- y1 + s
			y3 <- y1 - s
			df <- data.frame(x, y1, y2, y3)
			p <- lattice::xyplot(y1 + y2 + y3 ~ x, data = df,
				col = c("black", "red", "red"), xlab = spectra$unit[1],
				ylab = "Full Data Set, mean +/- sd", type = "l", ...)
			plot(p)
			}

		if (method == "sem") {
			y <- aaply(spectra$data, 2, .seXy)
			df <- data.frame(x, y1 = y[,1], y2 = y[,2], y3 = y[,3])
			p <- lattice::xyplot(y1 + y2 + y3 ~ x, data = df,
				col = c("black", "red", "red"), xlab = spectra$unit[1],
				ylab = "Full Data Set, mean +/- sem", type = "l", ...)
			plot(p)
			}

		if (method == "mad") {
			y <- aaply(spectra$data, 2, .seXyMad)
			df <- data.frame(x, y1 = y[,1], y2 = y[,2], y3 = y[,3])
			p <- lattice::xyplot(y1 + y2 + y3 ~ x, data = df,
				col = c("black", "red", "red"), xlab = spectra$unit[1],
				ylab = "Full Data Set, median +/- mad", type = "l", ...)
			plot(p)
			}

		if (method == "sem95") {
			y <- aaply(spectra$data, 2, .seXy95)
			df <- data.frame(x, y1 = y[,1], y2 = y[,2], y3 = y[,3])
			p <- lattice::xyplot(y1 + y2 + y3 ~ x, data = df,
				col = c("black", "red", "red"), xlab = spectra$unit[1],
				ylab = "Full Data Set, mean +/- 95% ci sem", type = "l", ...)
			plot(p)
			}
		}

	if (by.gr) {
		gr <- sumGroups(spectra)
		
		# See if any groups should be dropped due to too few members
		rem <- c()
		dropGroups <- FALSE
		for (n in 1:length(gr$group)) {
			if (gr$no.[n] <= 3) {
				message("\nGroup ", gr$group[n],
			" has 3 or fewer members\n so your stats are not very useful...\n This group has been dropped for display purposes!")
				rem <- c(rem, gr$group[n])
				dropGroups <- TRUE
				}
			}
			
		if (dropGroups) {
			spectra <- removeGroup(spectra, rem.group = rem)
			gr <- sumGroups(spectra) # update now that groups have been removed
			}
		
		# Now set up and plot
		x <- spectra$freq
		l.x <- length(x)
		
		if (method == "iqr"){
			df1 <- data.frame(x = NA, y1 = NA, y2 = NA, y3 = NA, z = NA)
			for (n in 1:length(gr$group)) {
				which <- as.character(spectra$groups) == gr$group[n]
				y <- aaply(spectra$data[which, ], 2, .seXyIqr)
				z <- rep(gr$group[n], l.x)
				df2 <- data.frame(x = x, y1 = y[,1], y2 = y[,2], y3 = y[,3], z = z)
				df1 <- rbind(df1, df2)
				}

			df1 <- df1[-1,]
			p <- lattice::xyplot(y1 + y2 + y3 ~ x | z, data = df1,
				col = c("black", "red", "red"), xlab = spectra$unit[1],
				ylab = "median +/- iqr", type = "l",
				strip.left = TRUE, strip = FALSE,
				scales = list(x = "same", y1 = "same", y2 = "same", y3 = "same"),
				layout = c(1, length(gr$group)), ...)
			plot(p)
			}

		if (method == "sd") {
			df1 <- data.frame(x = NA, y1 = NA, y2 = NA, y3 = NA, z = NA)
			for (n in 1:length(gr$group)) {
				which <- as.character(spectra$groups) == gr$group[n]
				y1 <- aaply(spectra$data[which, ], 2, mean)
				s <- apply(spectra$data[which, ], 2, sd)
				y2 <- y1 + s
				y3 <- y1 - s
				z <- rep(gr$group[n], l.x)
				df2 <- data.frame(x = x, y1 = y1, y2 = y2, y3 = y3, z = z)
				df1 <- rbind(df1, df2)
				}

			df1 <- df1[-1,]
			p <- lattice::xyplot(y1 + y2 + y3 ~ x | z, data = df1,
				col = c("black", "red", "red"), xlab = spectra$unit[1],
				ylab = "mean +/- sd", type = "l",
				strip.left = TRUE, strip = FALSE,
				scales = list(x = "same", y1 = "same", y2 = "same", y3 = "same"),
				layout = c(1, length(gr$group)), ...)
			plot(p)
			}

		if (method == "sem") {
			df1 <- data.frame(x = NA, y1 = NA, y2 = NA, y3 = NA, z = NA)
			for (n in 1:length(gr$group)) {
				which <- as.character(spectra$groups) == gr$group[n]
				y <- aaply(spectra$data[which, ], 2, .seXy)
				z <- rep(gr$group[n], l.x)
				df2 <- data.frame(x = x, y1 = y[,1], y2 = y[,2], y3 = y[,3], z = z)
				df1 <- rbind(df1, df2)
				}

			df1 <- df1[-1,]
			p <- lattice::xyplot(y1 + y2 + y3 ~ x | z, data = df1,
				col = c("black", "red", "red"), xlab = spectra$unit[1],
				ylab = "mean +/- sem", type = "l",
				strip.left = TRUE, strip = FALSE,
				scales = list(x = "same", y1 = "same", y2 = "same", y3 = "same"),
				layout = c(1, length(gr$group)), ...)
			plot(p)
			}

		if (method == "mad") {
			df1 <- data.frame(x = NA, y1 = NA, y2 = NA, y3 = NA, z = NA)
			for (n in 1:length(gr$group)) {
				which <- as.character(spectra$groups) == gr$group[n]
				y <- aaply(spectra$data[which, ], 2, .seXyMad)
				z <- rep(gr$group[n], l.x)
				df2 <- data.frame(x = x, y1 = y[,1], y2 = y[,2], y3 = y[,3], z = z)
				df1 <- rbind(df1, df2)
				}

			df1 <- df1[-1,]
			p <- lattice::xyplot(y1 + y2 + y3 ~ x | z, data = df1,
				col = c("black", "red", "red"), xlab = spectra$unit[1],
				ylab = "median +/- mad", type = "l",
				strip.left = TRUE, strip = FALSE,
				scales = list(x = "same", y1 = "same", y2 = "same", y3 = "same"),
				layout = c(1, length(gr$group)), ...)
			plot(p)
			}

		if (method == "sem95") {
			df1 <- data.frame(x = NA, y1 = NA, y2 = NA, y3 = NA, z = NA)
			for (n in 1:length(gr$group)) {
				which <- as.character(spectra$groups) == gr$group[n]
				y <- aaply(spectra$data[which, ], 2, .seXy95)
				z <- rep(gr$group[n], l.x)
				df2 <- data.frame(x = x, y1 = y[,1], y2 = y[,2], y3 = y[,3], z = z)
				df1 <- rbind(df1, df2)
				}

			df1 <- df1[-1,]
			p <- lattice::xyplot(y1 + y2 + y3 ~ x | z, data = df1,
				col = c("black", "red", "red"), xlab = spectra$unit[1],
				ylab = "mean +/- 95 % ci sem", type = "l",
				strip.left = TRUE, strip = FALSE,
				scales = list(x = "same", y1 = "same", y2 = "same", y3 = "same"),
				layout = c(1, length(gr$group)), ...)
			plot(p)
			}
		}
		
	}
