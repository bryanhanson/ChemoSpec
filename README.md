[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

## What is ChemoSpec?

`ChemoSpec` is  collection of functions for plotting spectra (NMR, IR, Raman) and carrying out various forms of top-down exploratory data analysis, such as HCA, PCA, model-based clustering and STOCSY analysis.  Robust methods appropriate for this type of high-dimensional data are available.  Designed with metabolomics data sets in mind, where the samples fall into groups such as treatment and control.  Graphical output is formatted consistently for publication quality plots.  ChemoSpec is intended to be very user friendly and help you get usable results quickly.  A vignette illustrating typical operations is available.

## How to install ChemoSpec

### To install from Github using R:

````r
install.packages("devtools")
library("devtools")
install_github(repo = "bryanhanson/ChemoSpec@master")
library("ChemoSpec")
````
If you use `@some_other_branch` you can get other branches that might be available.  They may or may not pass CRAN checks and thus may not install automatically using the method above.  Check the NEWS file to see what's up.

### From CRAN using R:

````r
chooseCRANmirror() # choose a CRAN mirror
install.packages("ChemoSpec")
library("ChemoSpec")
````

### To view the Vignette:

````r
browseVignettes("ChemoSpec")
````
### License Information

`ChemoSpec` is distributed under the GPL-3 license, as stated in the DESCRIPTION file.  For more info, see the [GPL site.](https://gnu.org/licenses/gpl.html)

Questions?  hanson@depauw.edu
