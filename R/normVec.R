
# Function to normalize a vector into a sphere
# Part of the ChemoSpec package
# Bryan Hanson, DePauw University, Dec 2009

normVec <- function(x) x/sqrt(sum(x^2))
