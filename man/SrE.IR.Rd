\name{SrE.IR}
\alias{SrE.IR}
\alias{SrE.NMR}
\docType{data}
\title{IR and NMR Spectra of Serenoa repens (Saw Palmetto) Oil Extracts and Reference Oils}
\description{
A collection of 14 IR and NMR spectra of essential oil extracted from the palm \emph{Serenoa repens} or Saw Palmetto, which is commonly used to treat BPH in men.  The 14 spectra are of different retail samples, and are divided into two categories based upon the label description: adSrE, adulterated extract, and pSrE, pure extract.  The adulterated samples typically have olive oil added to them, which is inactive towards BPH.  There are two additional spectra included as references/outliers: evening primrose oil, labeled EPO in the data set, and olive oil, labeled OO.  These latter two oils are mixtures of triglycerides for the most part, while the SrE samples are largely fatty acids.  As a result, the spectra of these two groups are subtly different.
}
\usage{data(SrE.IR)
data(SrE.NMR)}
\format{
The data is stored as a \code{\link{Spectra}} object.
}
\source{
IR data collected in the author's laboratory.  NMR data collected at Purdue University with the generosity and assistance of Prof. Dan Raftery and Mr. Tao Ye.
}

\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\examples{
data(SrE.IR)
sumSpectra(SrE.IR)
data(SrE.NMR)
sumSpectra(SrE.NMR)
}
\keyword{datasets}
