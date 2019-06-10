
# ChemoSpec 5.1.8 2019-06-09
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
