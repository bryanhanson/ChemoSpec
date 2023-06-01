# File created by roxut; edit the function definition file, not this file

# Test found in cleanArgs.R:13 (file:line)
  

### Unit tests for cleanArgs in ChemoSpec

tf <- function(func = "read.table", ...) {
	args <- as.list(match.call())[-1]
	args <- ChemoSpec:::.cleanArgs(args, func)
	return(args)
}

# Verify that cleanArgs strips args intended for files2SpectraObject and list.files
# when called with read.table as an argument

args <- tf(x = 1:20, y = LETTERS[1:5], z = 0.78,
  header = TRUE, gr.crit = ".", recursive = TRUE,
  sep = ",", path = ".", pattern = "xyz",
  full.names = TRUE, out.file = "myfile", func = "read.table")
  
expect_true(length(args) == 2L)
expect_true("header" %in% names(args))
expect_true("sep" %in% names(args))
expect_false("recursive" %in% names(args))
expect_false("gr.crit" %in% names(args))

# Verify that cleanArgs strips args intended for files2SpectraObject and read.table
# when called with list.files as an argument
args <- tf(x = 1:20, y = LETTERS[1:5], z = 0.78,
  header = TRUE, gr.crit = ".", recursive = TRUE,
  sep = ",", path = ".", pattern = "xyz",
  full.names = TRUE, out.file = "myfile", func = "list.files")

expect_true(length(args) == 2L)
expect_true("recursive" %in% names(args))
expect_true("path" %in% names(args))
expect_false("header" %in% names(args))
expect_false("gr.crit" %in% names(args))

# Test 3. Verify that cleanArgs strips args intended for files2SpectraObject and list.files
# when called with readJDX as an argument

if (requireNamespace("readJDX", quietly = TRUE)) {
  args <- tf(x = 1:20, y = LETTERS[1:5], z = 0.78, func = "readJDX",
    gr.crit = ".", out.file = "myfile",
    recursive = TRUE, path = ".", pattern = "xyz", full.names = TRUE,
    SOFC = FALSE)

  expect_true(length(args) == 1L)
  expect_true("SOFC" %in% names(args))
}

