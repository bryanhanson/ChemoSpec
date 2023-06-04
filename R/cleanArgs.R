#'
#' Clean Up Dots Arguments to Pass Elsewhere
#'
#' @param args A list of args as produced by \code{as.list(match.call())[-1]}.
#'   These are the arguments as passed to the calling function.
#'
#' @param func The name of the function that will receive the cleaned arguments.
#'
#' @return A modified list of arguments.
#' @keywords internal
#' @noRd
#'
#' @tests tinytest
#' 
#' ### Unit tests for cleanArgs in ChemoSpec
#' 
#' tf <- function(func = "read.table", ...) {
#' 	args <- as.list(match.call())[-1]
#' 	args <- ChemoSpec:::.cleanArgs(args, func)
#' 	return(args)
#' }
#' 
#' # Verify that cleanArgs strips args intended for files2SpectraObject and list.files
#' # when called with read.table as an argument
#' 
#' args <- tf(x = 1:20, y = LETTERS[1:5], z = 0.78,
#'   header = TRUE, gr.crit = ".", recursive = TRUE,
#'   sep = ",", path = ".", pattern = "xyz",
#'   full.names = TRUE, out.file = "myfile", func = "read.table")
#'   
#' expect_true(length(args) == 2L)
#' expect_true("header" %in% names(args))
#' expect_true("sep" %in% names(args))
#' expect_false("recursive" %in% names(args))
#' expect_false("gr.crit" %in% names(args))
#' 
#' # Verify that cleanArgs strips args intended for files2SpectraObject and read.table
#' # when called with list.files as an argument
#' args <- tf(x = 1:20, y = LETTERS[1:5], z = 0.78,
#'   header = TRUE, gr.crit = ".", recursive = TRUE,
#'   sep = ",", path = ".", pattern = "xyz",
#'   full.names = TRUE, out.file = "myfile", func = "list.files")
#' 
#' expect_true(length(args) == 2L)
#' expect_true("recursive" %in% names(args))
#' expect_true("path" %in% names(args))
#' expect_false("header" %in% names(args))
#' expect_false("gr.crit" %in% names(args))
#' 
#' # Test 3. Verify that cleanArgs strips args intended for files2SpectraObject and list.files
#' # when called with readJDX as an argument
#' 
#' if (requireNamespace("readJDX", quietly = TRUE)) {
#'   args <- tf(x = 1:20, y = LETTERS[1:5], z = 0.78, func = "readJDX",
#'     gr.crit = ".", out.file = "myfile",
#'     recursive = TRUE, path = ".", pattern = "xyz", full.names = TRUE,
#'     SOFC = FALSE)
#' 
#'   expect_true(length(args) == 1L)
#'   expect_true("SOFC" %in% names(args))
#' }
#' 
#'
.cleanArgs <- function(args, func = NULL) {
  if (is.null(func)) stop("You must supply a reference function")

  f2SOformals <- names(formals(files2SpectraObject))
  funcFormals <- names(formals(func))

  # Basic procedure is to
  #   1. Remove the files2SpectraObject arguments
  #   2. Remove arguments that func cannot accept
  #      (these would be args intended for a function
  #       other than func)
  #   3. Override selected arguments (these will be assigned
  #      manually in files2SpectraObject)

  # Step 1
  args[f2SOformals] <- NULL

  # Step 2
  keep <- names(args) %in% funcFormals
  args[!keep] <- NULL # remove any remaining arguments

  # Step 3

  if (func == "read.table") {
    if ("file" %in% names(args)) args$file <- NULL
  }

  if (func == "readJDX") {
    if ("file" %in% names(args)) args$file <- NULL
  }

  if (func == "list.files") {
    if ("pattern" %in% names(args)) args$pattern <- NULL
    if ("full.names" %in% names(args)) args$full.names <- NULL
  }

  return(args)
}
