.r2qPCA <- function(x) {

  # Modifies objects of class princomp (R-mode PCA) to more closely resemble class prcomp (Q-mode PCA)

  if (!inherits(x, "princomp")) stop("The PCA object was not of class princomp")

  # sdev, center and scale for both classes are the same; no change necessary
  # Other list elements carried along unchanged
  # Can fully pass as class prcomp

  x$rotation <- x$loadings
  x$loadings <- NULL
  x$x <- x$scores
  x$scores <- NULL
  class(x) <- c("converted_from_princomp", "prcomp")
  x
}
