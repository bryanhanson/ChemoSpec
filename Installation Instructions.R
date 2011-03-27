
# Script & instructions to install ChemoSpec
# Bryan A. Hanson, DePauw University, March 2011

# Files are located at github.com/bryanhanson/ChemoSpec

# FOR EVERYONE
# 1st you have to install several packages that ChemoSpec requires, then install ChemoSpec. Here are the commands:

install.packages(pkgs = c("chemometrics", "lattice", "RColorBrewer", "plyr",
"mvoutlier", "rgl", "R.utils", "mclust", "seriation", "mvbutils"))

# The list above would be longer, but when you install chemometrics, it calls for a number
# of other packages that ChemoSpec needs.

# FOR WINDOWS MACHINES  (tested on Windows XP)

# First, you must get the ChemoSpec_x.xx.zip file from GitHub.  Look for the
# button called Downloads and click it. The ChemoSpec archive will be downloaded.
# Now unzip it: right-click on it,
# select "expand all..." & do what the Wizard asks.  Then find the
# "ChemoSpec" folder (not "ChemoSpec_x.xx") and move it into the R
# library, which is located at C:\Program Files\R\R-2.10.0\library

# FOR MACINTOSH USERS

# Get the ChemoSpec_x.xx.tar.gz file from GitHub.  Look for the button
# called Downloads and click it.  The ChemoSpec archive will be downloaded.

# If you are running OSX 10.4, you will probably need to install X11.
# You can try the next section if you like first.  If it gives an error when
# it gets to the point where it is installing package rgl, you need X11
# before proceeding.  If that is the case, you can get X11 from the
# from this site:
# roundhere.net/journal/install-x11-on-mac-os-x-10-4-tiger-without-dvd
# Follow the instructions there; you need two files from the site.

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