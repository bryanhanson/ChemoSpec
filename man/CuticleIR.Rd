\name{CuticleIR}
\Rdversion{1.1}
\alias{CuticleIR}
\docType{data}

\title{
IR Spectra of Plant Cuticles by Genotype and Treatment
}

\description{
IR spectra obtained by ATR sampling on leaves of \emph{Portulaca oleracea} (common purslane).  There are 157 spectra, divided into four groups from a G x E experiment.  Two genotypes were studied, golden (G), and tall green (T).  Two temperature regimes were employed, experimental (E, 35C) and control (C, 22C).  Sample name GC10 means golden phenotype, control treatment, plant no. 10.
}

\usage{data(CuticleIR)}

\format{

List of 9\cr
 $ freq   : num [1:1242] 501 503 505 507 509 ...\cr
 $ data   : num [1:157, 1:1242] 0.205 0.247 0.219 0.203 0.234 ...\cr
 $ names  : chr [1:157] "GC10" "GC11" "GC12" "GC14" ...\cr
 $ groups : Factor w/ 4 levels "GC","GE","TC",..: 1 1 1 1 1 1 1 1 1 1 ...\cr
 $ colors : chr [1:157] "dodgerblue4" "dodgerblue4" "dodgerblue4" "dodgerblue4" ...\cr
 $ sym    : num [1:157] 2 2 2 2 2 2 2 2 2 2 ...\cr
 $ alt.sym: chr [1:157] "b" "b" "b" "b" ...\cr
 $ unit   : chr [1:2] "Wavenumbers" "Absorbance"\cr
 $ desc   : chr "Kelly's Complete IR Data Set, Summer 2009"\cr
 - attr(*, "class")= chr "Spectra"\cr
}

\details{
Noisy regions at the extremes of the frequency range have been removed.  The region from 1800 - 2500 wavenumbers was also removed as it is uninformative.}

\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\source{
Data obtained by Kelly Summers at DePauw University, Summer 2009.
}

\examples{
data(CuticleIR)
chkSpectra(CuticleIR)
sumSpectra(CuticleIR) # this also runs chkSpectra() before doing the summary
}
\keyword{ datasets }
