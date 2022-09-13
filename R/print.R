# Print contents of \code{topdown} object.
#' @author Roman Hornung
#' @export
print.topdown <- function(x, ...) {
  cat("topdown object\n\n")
  cat("Number of classes:           ", length(x$allclasses), "\n")
  nleaf <- sum(sapply(x$modellist, function(x2) is.na(x2$subclasses[1])))
  cat("Number of leaf node-classes: ", nleaf, "\n")
}
