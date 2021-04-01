# Guidelines for Contributors **DRAFT / WIP**

This document explains how one can contribute to `ChemoSpec`.

### Code of Conduct

This project is released with a [Contributor Code of Conduct](https://bryanhanson.github.io/ChemoSpec/CODE_OF_CONDUCT.html).  By contributing, you agree to abide by its terms.

### Infrastructure

* The `master` branch is basically a release branch and the code is always updated and tagged for a CRAN release.  Documentation is based on the `master` branch.  Between CRAN releases, the version on the `master` branch may advance in small ways (and hence the documented version may be ahead of the CRAN version).  The `devel` branch is used as a pre-release branch and ideally always works, and will be the basis for the next CRAN release.  Additional branches will be created as needed; see below.
* Github Actions are used to automate certain tasks.  Pushing the `master` or `devel` branches will cause a build and check to occur.  Pushing the `master` branch will cause the documentation to be updated. The scripts controlling these actions are in `.github/workflows`.

### Bugs & Feature Requests

If you think you have found a bug, or at least undesirable behavior, please file an [issue](https://github.com/bryanhanson/ChemoSpec/issues) (after checking the list of active issues).  Be sure that your version of `R` and all packages are up-to-date. Then include a reproducible example demonstrating the problem, preferably using one of the built-in data sets.    Feature requests may be made in the same manner.

### At a Larger Scale

#### Set Up Your Work Environment

* Make sure your version of `R` is up-to-date (the current release or the release candidate).
* Make sure all needed packages are installed and up-to-date.  You can find them in `DESCRIPTION`.  Some packages rely on packages from Bioconductor, so be sure to run the standard updates from there as well.
* Read the Infrastructure section above.
* You will need to know how to use Git and how to build and check `R` packages.  If you are using RStudio there are some handy menu actions provided.  There are also varous `R` packages that will do some of the work.  Or you can go old school.  You'll need to be familiar with `roxygen2` style documentation.

#### Making Changes

* Fork the package and clone to your computer.  Keep your local copy sync'ed with upstream changes.
* Create a branch for your upcoming contribution (usually this will be named ...)
* Before any changes, build and check using the `--as-cran` setting, and fix any issues (which hopefully don't involve code problems, but rather additional tweaks to your local build environment).
* Make the changes you have in mind.
  + Please use frequent git commit messages that clearly state what you are doing.
  + Does your work require changes to `.Rbuildignore`, `.gitignore`?
  + Update the documentation as needed.  We use `roxgyen2`
  + Style your code using `styler` with the default settings.  Style only the file you are working on.
  + Write any unit tests if needed.  Unit tests go into the function file using the `roxygen2` style tags provided by `roxut`.  Unit tests use the `tinytest` framework.
  + If your changes are user-facing, add a bullet to the `NEWS.md` file.
* Once again, build and check locally using the `--as-cran` setting.  Fix any problems.  Do not skip this step.
* If your code (may possibly) introduce platform-dependent behavior, check the package using win-builder.
* Create a pull request. The body of your pull request should include language along the lines of "fixes issue #xyz".
* Wait for feedback.
