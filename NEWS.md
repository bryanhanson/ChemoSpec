# ChemoSpec 6.1.0 2021-12-xx
## New Features
* Tab completion for `Spectra` objects implemented.  In RStudio typing `SrE.IR$` will show a list of possible completions.  In the `R` console, typing `SrE.IR$` followed by the tab key shows possible completions.

## Misc.
* `evalClusters` disabled due to a problem with `clusterCrit `.
* `hmapSpectra` removed temporarily due to a problem with `seriation`.
* Fix example in `hmapSpectra` (the plot `p` was not called).

# ChemoSpec 6.0.1 2021-10-09
## Significant Changes to Graphics Output
* Courtesy GSOC and Tejasvi Gupta, the following plotting functions gain the ability to produce either `base` graphics, `ggplot2` or `plotly` graphics:
  - `plotSpectra`
  - `surveySpectra`
  - `surveySpectra2`
  - `loopThruSpectra` (which has been renamed to `reviewAllSpectra`)
  - `plotScree` (resides in `ChemoSpecUtils`)
  - `plotScores` (resides in `ChemoSpecUtils`)
  - `plotLoadings` (uses `patchwork` and hence `plotly` is not relevant)
  - `plot2Loadings`
  - `sPlotSpectra`
  - `pcaDiag`
  - `plotSampleDist`
  - `aovPCAscores`
  - `aovPCAloadings` (uses `patchwork` and hence `plotly` is not relevant)

* The `ggplot2` graphics output are generally similar in layout and spirit to the `base` graphics output, but significant improvements have been made in labeling data points using the `ggrepel` package.  And of course the `ggplot2` graphics can be modified after creation through the usual mechanisms.
* The `plotly` graphics output are interactive plots which can be used for better understanding the data. `plotly` provides tools such as zoom, interactive labels and custom positioning which are very useful.
* The graphics output options can be chosen by `options(ChemoSpecGraphics = "option")`.
* See the details in new help file `?GraphicsOptions`.
* `loopThruSpectra` has been renamed `reviewAllSpectra` to better reflect what it does under the new graphics options.

## Misc.
* `plotScree2` which has been deprecated for some time now, was removed.
* Functions with the `leg.loc` argument now allow much more flexibility with regard to how the legend is positioned.  See the documentation.
* `plotSpectra`, when using `base` graphics, now positions the sample names by a different mechanism, but gives a similar result.

# ChemoSpec 5.3.21 2021-07-05
## Misc.
* Update `hmapSpectra` as the wrapped function in `seriation` has changed.

# ChemoSpec 5.3.11 2021-03-24
## Changes in ChemoSpecUtils that affect ChemoSpec
* Documentation of `...` in `sumSpectra` corrected to show how to pass `tol` to `check4Gaps`.
* Option to use `Col7` a palette of 7 colorblind-friendly colors added to `files2SpectraObject` documentation.

## Misc.
* Checked against `R` 4.04 RC

# ChemoSpec 5.3.2 2020-08-09
## Misc.
* Vignettes updated, and moved to html format.
* Continuous integration for building and checking, as well as automated building of the `pkgdown` documentation site.
* The "Reference Spectrum" panel label in `plotLoadings` shortened to "Ref. Spectrum" in order to fit the allotted space better.

# ChemoSpec 5.2.12 2020-01-23
## Misc.
* Fixed class-checking issues due to changes coming in R 4.0 coming soon.
* Improved documentation in various places.
* Added documentation for `updateGroups` which has been in `ChemoSpecUtils` for a while but effectively hidden from users of `ChemoSpec`.
* Fixed the example in `plotSpectraDist` which had strange limits.
* Fixed the example in `mclustSpectra` which had an error and used data that was not a good illustration.
* `sampleDistSpectra` was renamed `sampleDist` and moved to `ChemoSpecUtils`.  The internal workings and plot details have changed.

## Changes in ChemoSpecUtils that affect ChemoSpec
* New color and symbol schemes are now provided for using during the import process.
* `removeFreq` now accepts a formula for `rem.freq`.  The old syntax remains valid.  See the documentation for `ChemoSpec2D` for examples of how to construct formulas.
* New functions `sampleDist`, `sampleDist.Spectra` and `sampleDist.Spectra2D`.

# ChemoSpec 5.1.88 2019-11-14
## Improvements
* Documentation for `hypTestScores` now shows several ways to look at the results.
* `files2SpectraObject` now will pass the argument `SOFC` to `readJDX` if JCAMP-DX files are being processed.

## Bug Fixes
* In `hmapSpectra` the return values (updated in version 5.1.48) were labeled incorrectly. Noticed by Gabriele Beltrame.  The function now returns everything returned by `seriation::hmap` making it easier for the user to extract the information they desire. Documentation was improved, and additional examples were added illustrating how to pass arguments downstream to customize the plot.
* Fixed a bug in `plotScores3D` and `plotScoresRGL` in which the wrong percent variance was plotted in the axes labels.  Reported by Owen Horsfall.

# ChemoSpec 5.1.48 2019-07-25
## Improvements
* `plotScoresRGL` gains an argument `axes` which allows one to control the drawing of the reference axes.
* `hmapSpectra` now returns the carpet matrix in addition to the sample and frequency rankings.

## New Features
* A new vignette, Introduction to PCA, was added.

# ChemoSpec 5.1.34 2019-06-23
## Improvements
* `matrix2SpectraObject` can now handle multiple input matrices (i.e. argument `in.file` can be a vector of file names). The function now includes a progress bar, similar to `files2SpectraObject`.

## Bug Fixes
* Fixed a buglet in `hcaSpectra` that involved an undesirable cast to data frame, which upset the cosine calculation.
* `evalClusters` was not returning the result in the case of `pkg = "NbClust"`.

## Misc.
* Unit test framework shifted to `tinytest`.
* Additional unit tests added.

# ChemoSpec 5.1.9 2019-06-09
## New Features
* Function `s_pcaSpectra` which performs sparse PCA added.
* Function `irlba_pcaSpectra` which performs IRLBA PCA added.

## Improvements
* Numerous internal changes to the handling of PCA results were made to accommodate the introduction of sparse PCA features, and allow for easier future introduction of other PCA methods.

## Bug Fixes
* An issue with the setting of x and y limits in `plotScores` was fixed.  This was a long standing bug that somehow escaped notice from the early days of this package. Note that `plotScores` is actually in `ChemoSpecUtils` but is called from `ChemoSpec`, affecting the results here.

## Misc.
* Function `conColScheme` moved to package `ChemoSpecUtils`.
* Documentation `colorSymbol` was moved to package `ChemoSpecUtils`.
* Vignette updated regarding color issues.

# ChemoSpec 5.0.229 2019-02-28
## Misc.
* Unit tests added for `.cleanArgs`.

# ChemoSpec 5.0.215 2018-12-06
## Improvements
* `files2SpectraObject` can now accept *any* arguments to `list.files` or `read.table` via the ... mechanism.  Arguments are sanitized more robustly.  However, it is possible to pass arguments that may not be of real utility.  For instance `na.strings` can be specified, but any `NA` in the data imported will trigger warnings and errors.

## Misc.
* `plotScree` added to `r_pcaSpectra` example.

# ChemoSpec 5.0.207 2018-11-28

## Improvements
* `files2SpectraObject` can now accept optional arguments `path` and `recursive` to allow reading through a directory substructure.  In addition, a progress bar is now displayed unless `debug = TRUE`. Both features suggested by Reinhard Kerschner along with preliminary code (thanks!).

# ChemoSpec 5.0.193 2018-11-20

## Bug Fixes
* Fixed a problem with color assignment in `.groupNcolor` (the problem seems to have been present from the very beginning).  Reported by Reinhard Kerschner (many thanks!). Note `.groupNcolor` now resides in `ChemoSpecUtils`.  Reported here for convenience.

## Improvements
* `matrix2SpectraObject` now uses `tryCatch()` to more gracefully fail and offer suggestions to user.
* `files2SpectraObject`

## Notices
* Older news may be found via `news(package = "ChemoSpec")`.
* The changelog/NEWS file has moved to [https://bryanhanson.github.io/ChemoSpec/](https://bryanhanson.github.io/ChemoSpec/), but you must know that since you are viewing it!
