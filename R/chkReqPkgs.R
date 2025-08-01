#'
#' Verify Required Packages are Available.
#'
#' *Internal function*.
#'
#' @param pkgs Character. Vector of package names.
#' @return Logical \code{TRUE}, invisibly, if all packages are locally available.
#'         Otherwise, one or more messages about missing packages and \code{FALSE}.
#'
#' @author `r .writeDoc_Authors(c("BH"))`
#' @export
#' @keywords internal
#'
.chkReqPkgs <- function(pkgs) {
  all_msgs <- NA_character_
  for (i in 1:length(pkgs)) {
    if (!requireNamespace(pkgs[i], quietly = TRUE)) {
      next_msg <- paste("You need to install package", pkgs[i], "to use this function\n")
      all_msgs <- c(all_msgs, next_msg)
    }
  }
  all_msgs <- na.omit(all_msgs)
  ans <- FALSE
  if (length(all_msgs) == 0L) {
    ans <- TRUE
  }
  if (length(all_msgs) > 0L) {
    message(all_msgs)
    ans <- FALSE
  }
  return(ans)
}
