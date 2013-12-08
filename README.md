
## How to install ChemoSpec

### To install from Github using R:

````r
install.packages("devtools")
library("devtools")
install_github(repo = "ChemoSpec", username = "bryanhanson", ref = "master")
library("ChemoSpec")
````
If you use `ref = "some_other_branch"` you can get other branches that might be available.  They may or may not pass CRAN checks and thus may not install automatically using the method above.  Check the NEWS file to see what's up.

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
### License Information

See the DESCRIPTION file.

Questions?  hanson@depauw.edu