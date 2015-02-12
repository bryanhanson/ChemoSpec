
	
aov_pcaSpectra <-function(spectra, fac) {

#  Function to conduct ANOVA-PCA per Harrington
#  as explained by Pinto
#  Matt Keinsley & Bryan Hanson
#  DePauw University, Nov. 2010 onward (completed July 2011)

# spectra must be an object of class Spectra from ChemoSpec
# fac is a vector character strings giving the names of slots in spectra

	if (missing(spectra)){
		stop("No spectral data set given")}
	if (length(fac) > 3 ) {
		stop("Cannot process more than 3 factors!")}
	chkSpectra(spectra)
	
	nf <- length(fac)

# Naming of matrices follows Harrington 2005, at least some of the time

# Hardwire all possible matrices for each factor 

	MC <- scale(spectra$data, scale = FALSE) # mean centered
	
	GM <- avgFacLvls(matrix = MC,fac = as.factor(rep(1,nrow (MC)))) # grand mean
	
	if (nf == 1) {
		big <- list(DA = MC)
		flist <- list(spectra[[fac[1]]])
		}
		
	if (nf == 2) {
		big <- list(DA = MC, DB = MC, DAB = MC)
		flist <- list(fA = spectra[[fac[1]]], fB = spectra[[fac[2]]],
			fAB = interaction(spectra[[fac[1]]], spectra[[fac[2]]]))
		}
		
	if (nf == 3) {
		big <- list(DA = MC, DB = MC, DC = MC, DAB = MC, DAC = MC, DBC = MC)
		flist <- list(fA = spectra[[fac[1]]], fB = spectra[[fac[2]]],
			fC = spectra[[fac[3]]], fAB = interaction(spectra[[fac[1]]], spectra[[fac[2]]]),
			fAC = interaction(spectra[[fac[1]]], spectra[[fac[3]]]),
			fBC = interaction(spectra[[fac[2]]], spectra[[fac[3]]]))
		}
		
	# Subtract matrices according to Pinto/Harrington
	# Run avgFacLvls on each successive residuals matrix 
	# Number of times avgFacLvls runs depends on number of factors given

	GMR <- MC - GM # grand mean residuals

	# 1 factor aov_pcaSpectra is the same as running regular PCA so issue a warning
	if (nf == 1) { warning("aov_pcaSpectra is the same as regular PCA for 1 factor")}

	if (nf == 1) { 
		big[[1]] <- avgFacLvls(matrix = GMR, flist[[1]])
		DAR <- GMR - big[[1]] 
		LM <- list(DA = big[[1]], DPE = DAR, MC = MC)
		names(LM) <-c(fac[1], "Res.Error", "MC Data")
		} # Harrington eqn (3)

	if (nf == 2) {
		big[[1]] <- avgFacLvls(matrix = GMR, flist[[1]])
		DAR <- GMR - big[[1]]
		big[[2]] <- avgFacLvls(matrix = DAR, flist[[2]])
		DBR <- DAR - big[[2]]
		big[[3]] <- avgFacLvls(matrix = DBR, flist[[3]])
		DABR <- DBR - big [[3]]
		LM <- list(DA = big[[1]], DB = big[[2]], DAB = big[[3]], DPE = DABR, MC = MC)
		names(LM) <- c(fac[1], fac[2], paste(fac[1], "x", fac[2], sep = " "), "Res.Error", "MC Data")
		}
	
	if (nf == 3) { 
		big[[1]] <- avgFacLvls(matrix = GMR, flist[[1]])
		DAR <- GMR - big[[1]]
		big[[2]] <- avgFacLvls(matrix = DAR, flist[[2]])
		DBR <- DAR - big[[2]]
		big[[3]] <- avgFacLvls(matrix = DBR, flist[[3]])
		DCR <- DBR - big[[3]]
		big[[4]] <- avgFacLvls(matrix = DCR, flist[[4]])
		DABR <- DCR - big[[4]]
		big[[5]] <- avgFacLvls(matrix = DABR, flist[[5]])
		DACR <- DABR - big[[5]]
		big[[6]] <- avgFacLvls(matrix = DACR, flist[[6]])
		DBCR <- DACR - big[[6]]
		LM <- list(DA = big[[1]], DB = big[[2]], DC = big[[3]], DAB = big[[4]],
			DAC = big[[5]], DBC = big[[6]], DPE = DBCR, MC = MC)
		names(LM) <- c(fac[1], fac[2], fac[3], paste(fac[1], "x", fac[2], sep = " "),
			paste(fac[1], "x", fac[3], sep = " "),paste(fac[2], "x", fac[3], sep = " "),
			"Res.Error", "MC Data")
		}
	
	LM # return value
	
	}
