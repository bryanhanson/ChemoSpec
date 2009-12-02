
# Script & instructions to install ChemoSpec on Windows
# Bryan A. Hanson, DePauw University, Nov 2009

# THIS IS FOR WINDOWS MACHINES ONLY  (tested on Windows XP)

# First, you must get the ChemoSpec_x.xx.zip file from GitHub
# and unzip it: download to your machine, right-click on it,
# select "expand all..." & do what the Wizard asks.  Then find the
# "ChemoSpec" folder (not "ChemoSpec_x.xx") and move it into the R
# library, which is located at C:\Program Files\R\R-2.10.0\library

# The list of things to install would be longer except that chemometrics calls for quite a few other packages!

install.packages(pkgs = c("chemometrics", "RColorBrewer", "plyr", "mvoutlier", "rgl", "ChemoSpec"))

library(ChemoSpec)

# each time you start R, you will have to issue the library(ChemoSpec)
# command unless you put it in your .Rprofile