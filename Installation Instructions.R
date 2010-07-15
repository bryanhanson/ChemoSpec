
# Script & instructions to install ChemoSpec
# Bryan A. Hanson, DePauw University, Dec 2009

# Files are located at github.com/bryanhanson/ChemoSpec

# FOR EVERYONE
# 1st you have to install several packages that ChemoSpec requires, then install ChemoSpec. Here is the commands:

install.packages(pkgs = c("chemometrics", "lattice", "RColorBrewer", "plyr",
"mvoutlier", "rgl", "R.utils", "mclust"))

# The list above would be longer, but when you install chemometrics, it calls for a number
# of other packages that ChemoSpec needs.

# FOR WINDOWS MACHINES  (tested on Windows XP)

# First, you must get the ChemoSpec_x.xx.zip file from GitHub.  Click 
# on the name and then at the bottom of the page click "view raw".
#The ChemoSpec archive will be downloaded.
# Now unzip it: right-click on it,
# select "expand all..." & do what the Wizard asks.  Then find the
# "ChemoSpec" folder (not "ChemoSpec_x.xx") and move it into the R
# library, which is located at C:\Program Files\R\R-2.10.0\library

# FOR MACINTOSH USERS

# Get the ChemoSpec_x.xx.tar.gz file from GitHub.  Click on the name
# and then at the bottom of the page click "view raw".  The ChemoSpec
# archive will be downloaded.
# In the GUI, go to "Packages & Data..." then "Package Installer"
# In the pull down, choose "Local Source Package"
# Select any other options in the dialog box that you usually use.
# Click "Install", navigate to the file you downloaded, and select it.
# Everything else should be automatic.

# Once ChemoSpec is installed

library(ChemoSpec)

# each time you start R, you will have to issue the library(ChemoSpec)
# command unless you put it in your .Rprofile

# TO GET STARTED with ChemoSpec
# You can print the vignette which explains most of the functions and their use with

vignette("ChemoSpec")

# Getting the vignette ready to view takes a bit of time, 30 seconds perhaps.