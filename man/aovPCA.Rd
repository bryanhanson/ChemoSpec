\name{aovPCA}
\alias{aovPCA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ANOVA-PCA Analysis of Spectra Data
}
\description{
ANOVA-PCA is a combination of both methods developed by Harrington.  The data is partitioned into submatrices corresponding to each experimental factor, which are then subjected to PCA separately after adding the residual error back.  If the effect of a factor is large compared to the residual error, separation along the 1st PC in the score plot should be evident.  With this method, the significance of a factor can be visually determined. (ANOVA-PCA is not blind to group membership.)
}
\usage{
aovPCA(spectra, fac)
}

\arguments{
  \item{spectra}{
An object of S3 class \code{\link{Spectra}}
}
  \item{fac}{
A vector of character strings giving the factors to be used in the analysis.  These should be elements of \code{\link{Spectra}}.  Note that there should be 2 or more factors, because ANOVA-PCA on one factor is the same as standard PCA.  See the example.
}
}
\value{
A list of matrices for each factor and their interactions, along with the residual error and mean centered data matrix.
}

\details{ANOVA-PCA with only one factor is the same as standard PCA and gives no additional separation.
}
\references{
Pinto, Bosc, Nocairi, Barros, and Rutledge. "Using ANOVA-PCA for Discriminant Analysis: ..." Analytica Chimica Acta 629.1-2 (2008): 47-55.

Harrington, Vieira, Espinoza, Nien, Romero, and Yergey. "Analysis of Variance--Principal Component Analysis: ..." Analytica Chimica Acta 544.1-2 (2005): 118-27.
}
\author{
Matthew J. Keinsley and Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\seealso{
This function calls \code{\link{avgFacLvls}}, and the results are used in \code{\link{aovPCAscores}} and \code{\link{aovPCAloadings}}.
}


\examples{
data(CuticleIR)
# Creat new groups from existing groups
# These are used as the factors
n.groups <-list(genotype = c("G", "T"), treatment = c("C", "E"))
NewIR <- splitSpectraGroups(CuticleIR, n.groups)
# run aovPCA
mats <-aovPCA(NewIR, fac = c("genotype", "treatment"))
apca1 <- aovPCAscores(NewIR, mats, plot = 1)
apca2 <- aovPCAscores(NewIR, mats, plot = 2)
apca3 <- aovPCAscores(NewIR, mats, plot = 3)
apca5 <- aovPCAloadings(spectra = NewIR, LM = mats, pca = apca1)
}
\keyword{ multivariate }
\keyword{ htest }
