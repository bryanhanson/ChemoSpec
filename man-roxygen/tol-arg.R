#'
#' @param tol A number describing the fraction of points to be labeled.
#' \code{tol = 1.0} labels all the points; \code{tol = 0.05} labels
#' \emph{approximately} the most extreme 5 percent. Set to \code{"none"} to
#' completely suppress labels. Note that a simple approach
#' based upon quantiles is used, assumes that both x and y are each normally
#' distributed, and treats x and y separately.  Thus, this is not a formal
#' treatment of outliers, just a means of labeling points. Groups are lumped
#' together.
