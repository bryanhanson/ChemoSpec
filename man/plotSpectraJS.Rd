\name{plotSpectraJS}
\alias{plotSpectraJS}
\title{
Plot a Spectra Object Interactively
}
\description{
This function uses the d3.js JavaScript library to plot a \code{Spectra} object interactively.  This is most useful for data exploration.  For high quality plots, consider \code{\link{plotSpectra}}.  
}
\usage{
plotSpectraJS(spectra, browser = NULL, minify = TRUE)
}
\arguments{
  \item{spectra}{
An object of S3 class \code{\link{Spectra}} to be checked.
}
\item{browser}{
Character.  Something that will make sense to your OS.  Only
necessary if you want to override your system specified browser as understood by
\code{R}.  See below for further details.
}
  \item{minify}{
Logical.  Should the JavaScript be minified (compressed) using package \code{js}?  This gives code that executes faster, but is pretty-much impossible to read.
}
}

\section{Browser Choice}{
The browser is called by \code{\link[utils]{browseURL}}, which
in turn uses \code{options("browser")}.  Exactly how this is handled
is OS dependent.
}

\section{RStudio Viewer}{
If browser is \code{NULL}, you are using RStudio, and a viewer is specified, the viewer will be called instead of a browser.  You can stop this by with \code{options(viewer = NULL)}.
}

\section{Browser Choice/Mac}{
On a Mac, the default browser is called
by \code{/bin/sh/open}
which in turn looks at which browser you have set in the system settings.  You can
override your default with
\code{browser = "/usr/bin/open -a 'Google Chrome'"} for example.
Testing shows that on a Mac, Safari and Chrome perform correctly,
but in Firefox the mouse cursor is slightly offset from the guides.  While it
doesn't look quite right, the value of the cursor displayed is correct.
}

\section{Browser Choice/Other Systems}{
  \code{plotSpectraJS} has been tested
on a Windows 7
professional instance running in VirtualBox using Firefox and Chrome, and
runs correctly (Firefox has the same mouse position issue as mentioned above).
}


\value{
None; side effect is an interactive web page.  The temporary directory
containing the files that drive the web page is written to the console in case
you wish to use those files.  This directory is deleted when you quit R.  If you wish to use
the file, don't minify the code, it will be unreadable.
}

\references{
\url{https://github.com/bryanhanson/ChemoSpec}
}

\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
\code{\link{plotSpectra}} for non-interactive plotting.
}
\examples{
\dontrun{
require("jsonlite")
require("js")
data(metMUD2)
plotSpectraJS(metMUD2)
}
}


\keyword{ plot }
