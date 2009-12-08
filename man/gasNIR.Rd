\name{gasNIR}
\alias{gasNIR}
\docType{data}
\title{Gasoline NIR Data in Spectra Format}
\description{
This is the same data as in the \code{gasoline} data set that is included with \code{R}.  The data has been reformatted as a \code{"Spectra"} object suitable for use in \code{ChemoSpec}.  In particular, the octane values in the original data set have been recoded as factors into three groups: low, medium, and high.  The samples do not segregate well on PCA.
}
\usage{data(gasNIR)}
\format{
List of 9\cr
 $ freq   : num [1:401] 900 902 904 906 908 910 912 914 916 918 ...\cr
 $ data   : num [1:60, 1:401] -0.0502 -0.0442 -0.0469 -0.0467 -0.0509 ...\cr
  ..- attr(*, "dimnames")=List of 2\cr
  .. ..$ : chr [1:60] "1" "2" "3" "4" ...\cr
  .. ..$ : chr [1:401] "900 nm" "902 nm" "904 nm" "906 nm" ...\cr
 $ names  : chr [1:60] "sample.1" "sample.2" "sample.3" "sample.4" ...\cr
 $ groups : Factor w/ 3 levels "L","M","H": 1 1 3 1 2 1 3 2 3 3 ...\cr
 $ colors : chr [1:60] "forestgreen" "forestgreen" "dodgerblue4" "forestgreen" ...\cr
 $ sym    : num [1:60] 2 2 1 2 3 2 1 3 1 1 ...\cr
 $ alt.sym: chr [1:60] "b" "b" "a" "b" ...\cr
 $ unit   : chr [1:2] "nanometers" "log(1/reflectance)"\cr
 $ desc   : chr "Kalivas' gasoline data set (converted to Spectra object)"\cr
 - attr(*, "class")= chr "Spectra"\cr
}
\source{
J. H. Kalivas, "Two data sets of near infrared spectra" Chemometrics and Intelligent Laboratory Systems, vol. 37, pgs. 255-259 (1997).
}
\examples{
data(gasNIR)
hcaSpectra(gasNIR, title = "NIR of Gasoline Samples by Octane Level")
}
\keyword{datasets}
