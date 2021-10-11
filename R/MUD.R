#'
#' Made Up NMR Data Sets
#'
#' These data sets are simulated 300 MHz NMR spectra.  They are designed mainly
#' to illustrate certain chemometric methods and are small enough that they
#' process quickly.
#'
#' \code{alignMUD} is a series of mis-aligned spectra of a single small organic
#' molecule.
#'
#' \code{metMUD1} is composed of 20 samples, each a mixture of four typical
#' small organic compounds (we'll leave it to the reader as an exercise to
#' deduce the spin systems!).  These compounds are present in varying random
#' amounts.  Ten of the samples are control samples, and ten are treatment
#' samples.  Thus you can run PCA and other methods on this data set, and
#' expect to see a separation.  This data set is normalized.
#'
#' \code{metMUD2} also consists of 20 samples of mixtures of the same four
#' compounds.  However, the concentrations of some of the compounds are
#' correlated with other compounds, both positively and negatively, and some
#' concentrations are random. \code{metMUD2} is divided into different sample
#' groups which correspond conceptually to two genes, each active or knocked
#' out.  This data set is designed to be similar to a metabolomics data set in
#' which the concentrations of some compounds co-vary, and others are
#' independent. This data set is normalized.
#'
#' @template authors-BH
#'
#' @name metMUD1
#'
#' @aliases alignMUD metMUD1 metMUD2
#'
#' @docType data
#'
#' @format The data is stored as a \code{\link{Spectra}} object.
#'
#' @source Created using various tools.  Contact the author for a script if
#' interested.
#'
#' @seealso Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#' @keywords datasets
#'
#' @examples
#' data(metMUD1)
#' sumSpectra(metMUD1)
#' #
#' data(metMUD2)
#' sumSpectra(metMUD2)
NULL
