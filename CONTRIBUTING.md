# Guidelines for Contributors **DRAFT / WIP**

This document explains how one can contribute to `ChemoSpec`.

### Code of Conduct

This project is released with a [Contributor Code of Conduct](https://bryanhanson.github.io/ChemoSpec/CODE_OF_CONDUCT.html).  By contributing, you agree to abide by its terms.

### Infrastructure

* The `master` branch is basically a release branch and the code is always updated and tagged for a CRAN release.  Documentation is based on the `master` branch.  Between CRAN releases, the version on the `master` branch may advance in small ways (and hence the documented version may be ahead of the CRAN version).  The `devel` branch is used as a pre-release branch and ideally always works, and will be the basis for the next CRAN release.  Additional branches will be created as needed; see below.
* Github Actions are used to automate certain tasks.  Pushing the `master` or `devel` branches will cause a build and check to occur.  Pushing the `master` branch will cause the documentation to be updated. The scripts controlling these actions are in `.github/workflows`.
