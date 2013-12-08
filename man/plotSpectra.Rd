\name{plotSpectra}
\Rdversion{1.1}
\alias{plotSpectra}
\title{
Plot Spectra Object
}
\description{
Plots the spectra stored in a \code{"Spectra"} object.  One may choose which spectra to plot, and the x range to plot.  Spectra may be plotted offset or stacked.  The vertical scale is controlled by a combination of several parameters.
}
\usage{
plotSpectra(spectra, which = c(1), yrange = range(spectra$data),
	offset = 0, amplify = 1, lab.pos = mean(spectra$freq), ...)
}
\arguments{
  \item{spectra}{
An object of S3 class \code{"Spectra"}.
}
  \item{which}{
An integer vector specifying which spectra to plot, and the order.
}
  \item{yrange}{
A vector giving the limits of the y axis desired, for instance \code{c(0, 15)}.  This parameter depends upon the range of values in the stored spectra and defaults to the height of the largest peak in the data set.  Interacts with the next two arguments, as well as the number of spectra to be plotted as given in \code{which}.  Trial and error is used to adjust all these arguments to produce the desired plot.
}
  \item{offset}{
A number specifying the vertical offset between spectra if more than one is plotted.  Set to 0.0 for a stacked plot.
}
  \item{amplify}{
A number specifying an amplification factor to be applied to all spectra.  Useful for magnifying spectra so small features show up (though large peaks will then be clipped, unless you zoom on the x axis).
}
  \item{lab.pos}{
A number giving the location for the identifying label.  Generally, pick an area that is clear in all spectra plotted.  If no label is desired, give \code{lab.pos} outside the plotted x range.
}
  \item{\dots}{
Additional parameters to be passed to plotting functions.
}
}
\value{
None.  Side effect is a plot.
}


\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\examples{
data(CuticleIR)
myt <- expression(bolditalic(Portulaca)~bolditalic(oleracea)~bold(Cuticle~IR~Spectra))
plotSpectra(CuticleIR, main = myt,
	which = c(10:11, 40:41, 100:101, 140:141, 150:151),
	yrange = c(0,10),
	offset = 0.8, amplify = 1.0, lab.pos = 2000) 

}
\keyword{ hplot }
