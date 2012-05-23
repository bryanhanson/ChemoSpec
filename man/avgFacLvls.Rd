\name{avgFacLvls}
\alias{avgFacLvls}
\title{
Average levels of a factor in a data matrix
}
\description{
\code{\link{avgFacLvls}} takes a matrix and calculates the column means for each level of each factor given. It then replaces the original matrix rows with the means corresponding to the factor/level memership of a particular sample (row). 
}
\usage{
avgFacLvls(matrix, fac)
}

\arguments{
  \item{matrix}{
A matrix.
}
  \item{fac}{
A vector of character strings with length = nrow(matrix)}
}

\value{
A matrix whose rows are composed of the column means for each level of the factor. 
}

\author{
Matthew J. Keinsley and Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}

\seealso{
\code{\link{aovPCA}} for full details.
}
\examples{
M1 <- matrix(rnorm(100), nrow = 20, byrow = TRUE)
facs <- factor(c(rep("A",5), rep("B",5), rep("C", 5), rep("D", 5)))
M2 <- avgFacLvls(M1, fac = facs)
}

\keyword{ multivariate }
\keyword{ htest }
