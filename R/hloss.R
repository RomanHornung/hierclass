#' H-loss measure defined by Cesa-Bianchi et al. (2006).
#' This measure takes
#' into account that after a hierarchical prediction rule makes its first error
#' in the top-down prediction process, it is not expected that the remaining predictions
#' at lower levels of the category tree are correct, which is why only the first
#' errors made should be penalized. Moreover, errors made in the upper levels of the
#' category tree should be penalized more strongly than errors occurring at the
#' lower levels. This loss measure particularly rewards early stoppings without
#' errors. See the 'Details' section for more details.
#'
#' We define the weights used in the calculation of the H-loss in such a way
#' that errors made in the upper levels of the category are penalized more strongly
#' than in the lower levels. More precisely, the errors are weighted by w0^l,
#' where l is the level the error occurred (starting with 1 for the highest
#' levels in the category tree) and w0 is the input parameter \code{w0}.\cr\cr
#' A more technical detail: In contrast to Cesa-Bianchi et al. (2006), we do not calculate the sum of
#' the errors occurred (error (yes) = 1 vs. error (no) = 0), but the mean. But
#' the procedure is analogous to that of Cesa-Bianchi et al. (2006) if we
#' define the weights as the  w0^l divided by their sum over all observations.
#'
#' @title H-loss measure
#' @param truth True (observed) labels. Must have the same length as \code{response}. Can be factor (recommended) or character.
#' These need to be structured in the same way as required for \code{\link{topdown}}. See the 'Details' section of \code{\link{topdown}}
#' for details.
#' @param response Predicted response labels. Must have the same length as \code{truth}. Can be factor (recommended) or character.
#' These need to be structured in the same way as required for \code{\link{topdown}}. See the 'Details' section of \code{\link{topdown}}
#' for details.
#' @param w0 Number in ]0,1]. This parameter controls how much stronger errors
#' in the upper levels of the category tree are penalized than errors in
#' the lower levels. The smaller the value of this parameter is chosen, the
#' stronger the penalization of errors in the upper levels of the category tree
#' becomes. See the 'Details' section below for technical details. Choosing \code{w0} equal to 1 has the
#' effect that all errors are weighted the same, irrespective of the
#' level the error occurred. However, this is, in general, not recommended
#' because it favors classifiers that stop very early, which thus do not allow
#' obtaining predictions of finer classes at the lower levels of the category tree.
#' Default is 0.75.
#' @return The value of the H-loss measure.
#' @examples
#' \dontrun{
#'
#' ## Load package:
#'
#' library("hierclass")
#'
#'
#' ## Load the example data set 'datasim':
#'
#' data(datasim)
#'
#'
#' ## Set seed to make results reproducible:
#'
#' set.seed(1234)
#'
#'
#' ## Split data set into training and test data:
#'
#' trainind <- sample(1:nrow(datasim), size=round((3/4)*nrow(datasim)))
#' datatrain <- datasim[trainind,]
#' datatest <- datasim[-trainind,]
#'
#'
#' ## Construct a top-down hierarchical prediction rule using the training data:
#'
#' object <- topdown(ydepvar ~ ., data=datatrain, num.trees=50)
#' # NOTE: In practice 'num.trees' should in general be larger
#' # to achieve better performance (default is 500).
#' # We use 50 trees here only for computational efficiency of
#' # the example.
#'
#'
#' ## Predict the classes of observations in the test data (without
#' ## early stopping because 'confid=1' by default):
#'
#' preds <- predict(object, data=datatest)
#'
#'
#' ## Compute the H-loss measure for the predictions of the test data using
#' ## different values for 'w0':
#'
#' hloss(truth=datatest$ydepvar, response=preds, w0=0.9)
#' hloss(truth=datatest$ydepvar, response=preds)
#' hloss(truth=datatest$ydepvar, response=preds, w0=0.1)
#' }
#'
#' @author Roman Hornung
#' @references
#' \itemize{
#'   \item Cesa-Bianchi, N., Gentile, C., Zaniboni, L. (2006).
#'   Incremental algorithms for hierarchical classification.
#'   Journal of Machine Learning Research 7:31–54.
#'   }
#' @seealso \code{\link{hierre}}, \code{\link{hierpr}}, \code{\link{hierfbeta}}, \code{\link{spath}}
#' @encoding UTF-8
#' @export
hloss <- function(truth, response, w0=0.75) {

  # Convert the factors to characters, because we perform string
  # operations on them:
  truth <- as.character(truth)
  response <- as.character(response)

  # Add the root node class, which makes the calculations
  # of the H-loss easier:
  naind <- is.na(response)

  truth <- paste0("rootnode.", truth)
  response <- paste0("rootnode.", response)

  response[naind] <- "rootnode"

  truthall <- sapply(truth, strsplit, split="\\.")
  responseall <- sapply(response, strsplit, split="\\.")

  # Calculation of the H-loss:
  result <- mean(mapply(function(x, y) {
    if(length(y) <= length(x) & y[length(y)]==x[length(y)] & length(y) > 1)
      return(0)
    inddeepanc <- suppressWarnings(max(which(x==y)))
    res <- w0^inddeepanc
    return(res)
  }, truthall, responseall))

  return(result)

}
