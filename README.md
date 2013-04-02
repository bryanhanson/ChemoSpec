*NOTE:  ChemoSpec requires mclust 4.0 as of version 1.51-2.*

## How to install ChemoSpec

### To install from Github using R:

````r
install.packages("devtools")
library("devtools")
install_github(repo = "ChemoSpec", username = "bryanhanson", ref = "master")
library("ChemoSpec")
````
NOTE: The current version, 1.60-3, passes all checks and can be installed using the command above.

If you use `ref = "devel"` you can get the development branch if it is available.  Development branches have new, possibly incompletely tested features.  They may may also not be ready to pass checks and thus may not install automatically using the method above.  Check the news file to see what's up.  If you are interested in a particular feature in the devel branch, you can probably just grab the function of interest and source it into an existing package installation.

### From CRAN using R:

````r
chooseCRANmirror() # choose a CRAN mirror
install.packages("ChemoSpec")	
library("ChemoSpec")
````

### To get to the Vignette:

````r
vignette("ChemoSpec")
````

Questions?  hanson@depauw.edu