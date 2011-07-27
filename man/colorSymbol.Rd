\name{colorSymbol}
\Rdversion{1.1}
\alias{colorSymbol}
\title{
Colors and Symbols in ChemoSpec and Spectra Objects
}
\description{
In \code{ChemoSpec}, the user may use any color name/format known to R.  For ease of comparison, it would be nice to plan ahead and use the same color scheme for all your plots.  However, a difficulty to bear in mind is that R plots are generally on a white background, so pale colors should be avoided, while \code{GGobi}, which is used by function \code{\link{plotScoresG}}, plots on a black background (interactively), so dark colors should be avoided.  Hence some compromise is necessary.

In addition to colors, \code{"Spectra"} objects also contain a list of symbols, and alternative symbols.  These are useful for plotting in black and white, or when color-blind individuals will be viewing the plots.  The alternative symbols are simply lower-case letters, as these are needed for \code{\link{plotScoresRGL}}, which cannot plot traditional symbols. 

If you plan to use \code{rggobi} and \code{GGobi} to view the data later, keep in mind that \code{ggobi} only uses certain color schemes (although there are many options) and in interactive operation plots on a black background.  In the case of \code{ChemoSpec}, two particular options have been hard-coded into the function \code{\link{plotScoresG}} for simplicity.  If you plan to use \code{plotScoresG}, you may wish to choose from one of these two color schemes before you begin if you want all your graphics to use the same scheme.  Keep in mind that these colors must be used in order (though you can use the order of argument \code{gr.crit} to associate a particular group with a particular color:

primary scheme: \code{c("red3", "dodgerblue4", "forestgreen",
"purple4", "orangered", "yellow", "orangered4", "violetred2")}

pastel scheme: \code{c("seagreen", "brown2", "skyblue2", "hotpink3",
"chartreuse3", "darkgoldenrod2", "lightsalmon3", "gray48")}

If you want to see what these colors look like, use \code{display.brewer.pal(8, "Set1")} or \code{display.brewer.pal(8, "Set2")}.

Finally, the current color scheme of a \code{"Spectra"} object may be determined using \code{\link{sumGroups}} or changed using \code{\link{conColScheme}}. Also, \code{\link{splitSpectraGroups}} has another means of changing the color scheme but this is intended for the situations when you are creating new categories for your samples.
}

\references{
\url{http://academic.depauw.edu/~hanson/ChemoSpec/ChemoSpec.html}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\keyword{ utilities }
\keyword{ color }