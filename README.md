
## How to install ChemoSpec

### To install from Github using R:

````r
install.packages("devtools")
library("devtools")
install_github(repo = "bryanhanson/ChemoSpec", ref = "master")
library("ChemoSpec")
````
If you use `ref = "some_other_branch"` you can get other branches that might be available.  They may or may not pass CRAN checks and thus may not install automatically using the method above.  Check the NEWS file to see what's up.

### From CRAN using R:

````r
chooseCRANmirror() # choose a CRAN mirror
install.packages("ChemoSpec")
library("ChemoSpec")
````

### To view the Vignette:

````r
vignette("ChemoSpec")
````
### License Information

ChemoSpec is distributed under the GPL-3 license, as stated in the DESCRIPTION file.  For more info, see the [GPL site.](https://gnu.org/licenses/gpl.html)

Questions?  hanson@depauw.edu
