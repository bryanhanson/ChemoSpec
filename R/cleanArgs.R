#' Clean Up Dots Arguments to Pass Elsewhere
#'
#' @param args A list of args as produced by \code{as.list(match.call())[-1]}.
#'   These are the arguments as passed to the calling function.
#'
#' @param func The name of the function that will receive the cleaned arguments.
#'
#' @return A modified list of arguments.
#'
#' @export
#'
#' @noRd
#'
.cleanArgs <- function(args, func = NULL) {

  if (is.null(func)) stop("You must supply a reference function")
  
  form <- names(formals(files2SpectraObject))
  refForm <- names(formals(func))

  # Basic procedure is to
  #   1. Remove the files2SpectraObject arguments
  #   2. Remove arguments that func cannot accept
  #      (these would be args intended for a function
  #       other than func)
  #   3. Override selected arguments (these will be assigned
  #      manually in files2SpectraObject)

  # Step 1
  args[form] <- NULL
  
  # Step 2
  keep <- names(args) %in% refForm
  args[!keep] <- NULL
  
  # Step 3 
  
  if (func == "read.table") {
  	if ("file" %in% names(args)) args$file <- NULL
  }
  
  if (func == "list.files") {
  	if ("pattern" %in% names(args)) args$pattern <- NULL
  	if ("full.names" %in% names(args)) args$full.names <- NULL
  }
  
  return(args)
}	
