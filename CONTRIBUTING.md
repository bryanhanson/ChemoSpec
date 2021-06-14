# Guidelines for Contributors

This document explains how one can contribute to `ChemoSpec`.

## Ground Rules

### Code of Conduct

This project is released with a [Contributor Code of Conduct](https://bryanhanson.github.io/ChemoSpec/CODE_OF_CONDUCT.html).  By contributing, you agree to abide by its terms.

### License

`ChemoSpec` is distributed under the GPL-3 license, as stated in the DESCRIPTION file.  For more info, see the [GPL site.](https://gnu.org/licenses/gpl.html)  By contributing, you agree that your contributions will be licensed in the same way.

## Simple Contributions

### Reporting Bugs & Suggesting Features

One of the easiest ways to contribute is to let us know when you think you have found a bug, or if you think a new feature would be desirable.  Both can be submitted using [issues](https://github.com/bryanhanson/ChemoSpec/issues). Be sure that your version of `R` and all packages are up-to-date. Then include a reproducible example demonstrating the problem, preferably using one of the built-in data sets.

## Infrastructure

### Branches

The `master` branch is basically a release branch and the code is always updated and tagged for a CRAN release.  Documentation is based on the `master` branch.  Between CRAN releases, the version on the `master` branch may advance in small ways (and hence the documented version may be ahead of the CRAN version).  The `devel` branch is used as a pre-release branch and ideally always works, and will be the basis for the next CRAN release.  Additional branches will be created as needed; see below.

### Continuous Integration

Github Actions are used to automate certain tasks.  Pushing the `master` or `devel` branches will cause a build and check to occur.  Pushing the `master` branch will cause the documentation to be updated. The scripts controlling these actions are in `.github/workflows`.

### Versioning

We following [semantic versioning practices](https:semver.org).  We liberally increment the patch version (z in x.y.z), but if you are contributing let the maintainers worry about this aspect.

## Larger-Scale Contributions

### Set Up Your Work Environment

* Make sure your version of `R` is up-to-date (the current release or the release candidate).
* Make sure all needed packages are installed and up-to-date.  You can find them in `DESCRIPTION`.  Some packages rely on packages from Bioconductor, so be sure to run the standard updates from there as well.
* You will need to know how to use Git and how to build and check `R` packages.  If you are using RStudio there are some handy menu actions provided.  There are also varous `R` packages that will do some of the work.  Or you can go old school.  You'll need to be familiar with `roxygen2` style documentation.

### Making Changes

* Fork the package and clone to your computer.  Keep your local copy sync'ed with upstream changes.
* Starting from the `devel` branch, create a new branch for your upcoming contribution (usually this will be named something like `issue/#27` if you are fixing an issue, for example, or `feature/feature_name` if you are contributing a new feature).  Generally if you have a feature you'd like to add, file an issue describing the proposed feature so it can be discussed and tracked.
* Before making any changes, build using the following flags:
  + `--as-cran`
  + `--no-init-file`
  + `--resave-data`
  + `--compact-vignettes="both"`
Fix any problems that arise (Which shouldn't involve code problems, but rather additional tweaks to your local build environment.  If you see any errors or warnings at this point, contact the maintainers before proceeding.).
* Check using the following flags:
  + `--as-cran`
  + `--no-init-file`
Fix any problems that arise (Which shouldn't involve code problems, but rather additional tweaks to your local build environment.  If you see any errors or warnings at this point, contact the maintainers before proceeding.).
* Make the changes you have in mind.
  + Please use super-frequent git commit messages that clearly state what you are doing.
  + Does your work require changes to `.Rbuildignore`, `.gitignore`?
  + Update the documentation as needed.  We use `roxgyen2`.
  + Style your code using `styler` with the default settings.  Style only the file you are working on.
  + Write any unit tests if needed. Unit tests use the `tinytest` framework.
  + Run `roxygen2::document()` to update the help files and `NAMESPACE` if your changes alter the help entries or add additional `@importFrom` entries.
  + If your changes are user-facing, fix a bug, change behavior or add a new feature, add a bullet to the `NEWS.md` file.  This file helps us keep track over time.  Only trivial changes do not go to `NEWS.md`.
  + If your contribution is significant, add yourself to `DESCRIPTION`.
* Once again, build and check locally with the flags listed earlier.  Fix any problems.  Do not skip this step.
* If your code (may possibly) introduce platform-dependent behavior, check the package using win-builder.
* Create a pull request. The body of your pull request should include language along the lines of "fixes issue #xyz".  See other special words that cause the associated issue to automatically be closed on successful pul can be seen [here](https://docs.github.com/en/github/managing-your-work-on-github/linking-a-pull-request-to-an-issue#linking-a-pull-request-to-an-issue-using-a-keyword).
* Wait for feedback.
