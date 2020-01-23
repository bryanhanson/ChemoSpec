#'
#' Create New Groups from an Existing Spectra Object
#'
#' This function takes an existing \code{\link{Spectra}} object and uses your
#' instructions to split the existing \code{spectra$groups} into new groups.
#' The new groups are added to the existing \code{\link{Spectra}} object (a
#' list) as new elements.  This allows one to use different combinations of
#' factors than were originally encoded in the \code{\link{Spectra}} object.
#' The option also exists to replace the color scheme with one which
#' corresponds to the new factors.
#'
#' The items in the character vector are grepped among the existing
#' \code{spectra$groups} entries; when found, they are placed in a new element
#' of \code{\link{Spectra}}.  In the example, all \code{spectra$groups} entries
#' containing "G" are coded as "G" in a new element called \code{spectra$env},
#' and any entries containing "T" are handled likewise.  This amounts to a sort
#' of recoding of factors (the example demonstrates this).  Every entry in
#' \code{spectra$groups} should be matched by one of the entries in the
#' character vector.  If not, you will get <NA> entries.  Also, if the targets
#' in the character vector are not unique, your results will reflect the order
#' of the levels.  Since this is a grep process, you can pass any valid grep
#' string as the target.
#'
#' If \code{rep.cols} is provided, these colors are mapped one for one onto the
#' levels of the the first element of \code{inst}.  This provides a different
#' means of changing the sample color encoding than \code{\link{conColScheme}}.
#'
#' @param spectra An object of S3 class \code{\link{Spectra}}.
#'
#' @param inst A list giving the name of the new element to be created from a
#' set of target strings given in a character vector.  See the example for the
#' syntax.
#'
#' @param rep.cols Optional.  A vector giving new colors which correspond to
#' the levels of \code{inst}.  Only possible if \code{inst} has only one
#' element, as the possible combinations of levels and colors may get
#' complicated.
#'
#' @param \dots Additional arguments to be passed downstream. Currently not
#' used.
#'
#' @return An object of S3 class \code{\link{Spectra}}, modified to have
#' additional elements as specified by \code{inst}.
#'
#' @author Bryan A. Hanson, DePauw University.
#'
#' @seealso \code{\link{conColScheme}}
#' Additional documentation at \url{https://bryanhanson.github.io/ChemoSpec/}
#'
#' @keywords utilities
#'
#' @examples
#'
#' data(metMUD2)
#' levels(metMUD2$groups) # original factor encoding
#'
#' # Split those original levels into 2 new ones (re-code them)
#' new.grps <- list(geneBb = c("B", "b"), geneCc = c("C", "c"))
#' res <- splitSpectraGroups(metMUD2, new.grps)
#' str(res) # note two new elements, "geneBb" and "geneCc"
#' sumSpectra(res) # reports on extra elements
#'
#' # Note that if you want to use a newly created group in
#' # plotScores and other functions to drive the color scheme
#' # and labeling, you'll have to update the groups element:
#' res$groups <- as.factor(paste(res$geneBb, res$geneCc, sep = ""))
#' @export splitSpectraGroups
#'
splitSpectraGroups <- function(spectra, inst = NULL, rep.cols = NULL, ...) {
  .chkArgs(mode = 11L)
  chkSpectra(spectra)
  if (is.null(inst)) stop("No splitting instructions provided")

  # inst(ructions) must be a list of strings to
  # be grepped and placed into a new element

  lg <- length(spectra$groups)
  li <- length(inst)

  # initialize df to carry new groups

  tmp <- data.frame(rep(NA, lg))
  colnames(tmp) <- "dummy"

  for (i in 1:li) {
    tmp <- data.frame(tmp, rep(NA, lg))
  }
  colnames(tmp) <- c("dummy", names(inst))

  # now load tmp with the actual values

  for (i in 1:length(inst)) {
    l <- length(inst[[i]])

    for (j in 1:l) {
      which <- grep(inst[[i]][j], spectra$groups)
      tmp[which, i + 1] <- inst[[i]][j]
    }
    tmp[, i + 1] <- as.factor(tmp[, i + 1])
  }

  spectra <- c(spectra, tmp)
  d <- grep("dummy", names(spectra))
  spectra <- spectra[-d]
  class(spectra) <- "Spectra"

  if (!is.null(rep.cols)) {
    if (li > 1) stop("rep.cols can only be used with an instruction composed of one list element\n")
    if (!length(inst[[1]]) == length(rep.cols)) stop("No. repl. colors doesn't equal levels in inst\n")

    # Match rep.cols one for one with inst
    l <- length(inst[[1]])
    tmp <- rep(NA, length(spectra$colors))

    for (i in 1:l) {
      which <- grep(inst[[1]][i], spectra[[names(inst)[1]]])
      tmp[which] <- rep.cols[i]
    }
    spectra$colors <- tmp
  }

  chkSpectra(spectra)
  spectra
}
