#' The hierarchical F-score defined by Kiritchenko et al. (2005). This is a (weighted) geometric
#' mean of the hierarchical recall and the hierarchical precision. Takes the ancestors of the true and
#' predicted classes into account, but has the disadvantage that it overpenalizes errors in nodes with many
#' ancestors (Kosmopoulos et al., 2015).
#'
#' @title Hierarchical F-score
#' @param truth True (observed) labels. Must have the same length as \code{response}. Can be factor (recommended) or character.
#' These need to be structured in the same way as required for \code{\link{topdown}}. See the 'Details' section of \code{\link{topdown}}
#' for details.
#' @param response Predicted response labels. Must have the same length as \code{truth}. Can be factor (recommended) or character.
#' These need to be structured in the same way as required for \code{\link{topdown}}. See the 'Details' section of \code{\link{topdown}}
#' for details.
#' @param beta Positive number (minimum zero). The larger this value is chosen, the larger
#' the weight of the hierarchical recall becomes in comparison to that of the hierarchical precision.
#' The default is 1, which corresponds to weighting hierarchical recall and hierarchical precision equally.
#' @param type Type of average to use: The micro-average (\code{type="micro"}) and the macro-average (\code{type="macro"}) give
#' the same weight to all observations and all classes, respectively. Default is "micro".
#' @return The value of the hierarchical F-score.
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
#' ## Compute the micro-averaged hierarchical precision for the predictions
#' ## of the test data using different value of 'beta':
#'
#' hierfbeta(truth=datatest$ydepvar, response=preds)
#'
#' hierfbeta(truth=datatest$ydepvar, response=preds, beta=0.5)
#'
#' ## For 'beta=0', 'hierfbeta' delivers the same result as 'hierpr':
#' hierfbeta(truth=datatest$ydepvar, response=preds, beta=0)
#' hierpr(truth=datatest$ydepvar, response=preds)
#'
#' ## For large values of 'beta', 'hierfbeta' delivers results very
#' ## similar to 'hierre':
#' hierfbeta(truth=datatest$ydepvar, response=preds, beta=7)
#' hierre(truth=datatest$ydepvar, response=preds)
#' }
#'
#' @author Roman Hornung
#' @references
#' \itemize{
#'   \item Kiritchenko, S., Matwin, S., Famili, A. F. (2005). Functional annotation of genes using hierarchical text categorization. In: Proceedings of the BioLINK SIG: Linking Literature, Information and Knowledge for Biology, Detroit, Michigan, USA.
#'   \item Kosmopoulos, A., Partalas, I., Gaussier, E., Paliouras, G., Androutsopoulos, I. (2015). Evaluation measures for hierarchical classification: A unified view and novel approaches. Data Mining and Knowledge Discovery 29(3):820–865, <\doi{10.1007/s10618-014-0382-x}>.
#'   }
#' @seealso \code{\link{hierre}}, \code{\link{hierpr}}, \code{\link{hloss}}, \code{\link{spath}}
#' @encoding UTF-8
#' @export
hierfbeta <- function(truth, response, beta = 1, type="micro") {
  # First calculate hierarchical precision and recall:
  pr <- hierpr(truth=truth, response=response, type=type)
  re <- hierre(truth=truth, response=response, type=type)
  # Then calculate the F-score:
  beta2 <- beta^2
  nominator <- (1 + beta2) * pr * re
  denominator <- beta2 * pr + re
  nominator / denominator
}
