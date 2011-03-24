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
  The format is:
List of 9\cr
 $ freq   : num [1:1869] 399 401 403 405 407 ...\cr
 $ data   : num [1:16, 1:1869] 0.0914 0.0921 0.0848 0.0858 0.0929 ...\cr
 $ names  : chr [1:16] "CVS_adSrE" "ET_pSrE" "GNC_adSrE" "LF_adSrE" ...\cr
 $ groups : Factor w/ 4 levels "adSrE","EPO",..: 1 4 1 1 4 4 1 1 4 1 ...\cr
 $ colors : chr [1:16] "#984EA3" "#E41A1C" "#984EA3" "#984EA3" ...\cr
 $ sym    : num [1:16] 15 1 15 15 1 1 15 15 1 15 ...\cr
 $ alt.sym: chr [1:16] "d" "a" "d" "d" ...\cr
 $ unit   : chr [1:2] "wavenumber" "absorbance"\cr
 $ desc   : chr "Serenoa repens IR quality study"\cr
 - attr(*, "class")= chr "Spectra"\cr
}
\source{
IR data collected in the author's laboratory.  NMR data collected at Purdue University with the generosity and assistance of Prof. Dan Raftery and Mr. Tao Ye.
}
\examples{
data(SrE.IR)
sumSpectra(SrE.IR)
data(SrE.NMR)
sumSpectra(SrE.NMR)
}
\keyword{datasets}
