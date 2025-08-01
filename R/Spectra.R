#'
#' Spectra Objects
#'
#' In \code{ChemoSpec}, spectral data sets are stored in an S3 class called
#' \code{Spectra}, which contains a variety of information in addition to the
#' spectra themselves.  \code{Spectra} objects are created by
#' \code{\link{files2SpectraObject}} or \code{\link{matrix2SpectraObject}}.
#'
#'
#' @section Structure: The structure of a \code{Spectra} object is a list of 9
#' elements and an attribute as follows:
#'
#' * **$freq** (numeric) A common frequency (or wavelength) axis for all the spectra.
#' * **$data** (numeric) The intensities for the spectra.  A matrix of dimension no. samples x no. frequency points.
#' * **$names** (character) The sample names for the spectra; length must be no. samples.
#' * **$groups** (factor) The group classification of the samples; length must be no. samples.
#' * **$colors** (character) The colors for each sample; length must be no. samples. Groups and colors correspond.
#' * **$sym** (integer) As for \code{colors}, but symbols for plotting (if b/w is desired).
#' * **$alt.sym** (character) Lower-case letters as alternate symbols for plotting.
#' * **$unit** (character) Two entries, the first giving the x axis unit, the second the y axis unit.
#' * **$desc** (character) A character string describing the data set.  This appears on plots and therefore should probably be kept to 40 characters or less.
#' * attribute: character with value "Spectra" The S3 class designation.
#'
#' @author `r .writeDoc_Authors("BH")`
#'
#' @seealso \code{\link{sumSpectra}} to summarize a \code{Spectra} object.
#' \code{\link{sumGroups}} to summarize group membership of a \code{Spectra}
#' object. \code{\link{chkSpectra}} to verify the integrity of a
#' \code{Spectra} object. \code{\link{colorSymbol}} for a discussion of color
#' options. Finally, additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#'
#' @name Spectra
#'
#' @keywords classes
NULL
