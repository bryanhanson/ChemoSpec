readJEOLascii <- function(file = "") {
  # ChemoSpec, Bryan Hanson, June 2016

  # This function reads the JEOL ascii NMR format
  # The data consists of X, Real & Imaginary fields tab separated.

  DF <- read.table("JEOL_ascii.asc", header = TRUE, sep = "\t")
  res <- data.frame(x = DF$X, y = DF$Real)
}
