#' @title Shortest path loss measure
#' 
#' @description The shortest path loss measure. The unweighted shortest path loss measure,
#' introduced by Wang et al. (1999), is simply the distance between the true
#' and the predicted class in the category tree. The weighted shortest path
#' loss measure (Blockeel et al., 2002) is the measure by Wang et al. (1999) but with depth-specific
#' weights of the edges in the path, where smaller weights are used for lower
#' levels of the category tree. While the unweighted version is easier interpretable
#' it has the disadvantage that it does not take into account that
#' misclassifications at lower levels of the hierarchy are more forgivable than
#' those at upper levels.
#'
#' In the weighted shortest path distance the weight assigned to an edge is
#' w0^l, where l is the depth of the edge (starting with 1 for the highest
#' edges in the category tree) and w0 is the input parameter \code{w0}.
#'
#' @param truth True (observed) labels. Must have the same length as \code{response}. Can be factor (recommended) or character.
#' These need to be structured in the same way as required for \code{\link{topdown}}. See the 'Details' section of \code{\link{topdown}}
#' for details.
#' @param response Predicted response labels. Must have the same length as \code{truth}. Can be factor (recommended) or character.
#' These need to be structured in the same way as required for \code{\link{topdown}}. See the 'Details' section of \code{\link{topdown}}
#' for details.
#' @param type Type of the shortest path loss measure: \code{type="unweighted"}
#' for the unweighted version and \code{type="weighted"} for the weighted version.
#' Default is "weighted".
#' @param w0 Number in ]0,1[. This parameter controls how much stronger edges
#' in the upper levels of the category tree are penalized than edges in
#' the lower levels. The smaller the value of this parameter is chosen, the
#' stronger the penalization of edges in the upper levels of the category tree
#' becomes. See the 'Details' section below for technical details.
#' Default is 0.75 (as in Blockeel et al. (2002)).
#' @return The value of the shortest path loss measure.
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
#' ## Compute the weighted and unweighted shortest path distance for the
#' ## predictions of the test data:
#'
#' ## weighted version:
#' spath(truth=datatest$ydepvar, response=preds, w0=0.9)
#' spath(truth=datatest$ydepvar, response=preds)
#' spath(truth=datatest$ydepvar, response=preds, w0=0.1)
#'
#' ## unweighted version:
#' spath(truth=datatest$ydepvar, response=preds, type="unweighted")
#' }
#'
#' @author Roman Hornung
#' @references
#' \itemize{
#'   \item Wang, K., Zhou, S., Liew, S. C. (1999). Building hierarchical
#'   classifiers using class proximity. In: Atkinson, M. P. et al. (ed)
#'   Proceedings of the 25th conference on very large data bases, Detroit,
#'   pp 363–374, Edinburgh, UK. Morgan Kaufmann Publishers, San Francisco,
#'   US.
#'   \item Blockeel, H., Bruynooghe, M., Džeroski, S., Ramon, J., Struyf, J.
#'   (2002). Hierarchical multi-classification. In: Workshop Notes of the
#'   KDD’02 Workshop on Multi-Relational Data Mining, Detroit, pp 21-35,
#'   Edmonton, Canada.
#'   }
#' @seealso \code{\link{hierre}}, \code{\link{hierpr}}, \code{\link{hierfbeta}}, \code{\link{hloss}}
#' @encoding UTF-8
#' @export
spath <- function(truth, response, type="weighted", w0=0.75) {

  if (!(type %in% c("weighted", "unweighted")))
    stop("'type' has to be either 'weighted' or 'unweighted'.")

  # Convert the factors to characters, because we perform string
  # operations on them:
  truth <- as.character(truth)
  response <- as.character(response)

  # Add the root node class, which makes the calculations
  # of the shortest path distance easier:
  naind <- is.na(response)

  truth <- paste0("rootnode.", truth)
  response <- paste0("rootnode.", response)

  response[naind] <- "rootnode"

  truthall <- sapply(truth, strsplit, split="\\.")
  responseall <- sapply(response, strsplit, split="\\.")

  if(type=="unweighted") {

    # The unweighted shortest path distance:
    result <- mean(mapply(function(x, y) {
      if(x[length(x)]==y[length(y)])
        return(0)
      # The index of the deepest common ancestor:
      inddeepanc <- suppressWarnings(max(which(x==y)))
      # The shortest path distance:
      res <- length(x) + length(y) - 2*inddeepanc
      return(res)
    }, truthall, responseall))

  }

  if(type=="weighted") {

    # The weighted shortest path distance:
    result <- mean(mapply(function(x, y) {
      if(x[length(x)]==y[length(y)])
        return(0)
      inddeepanc <- suppressWarnings(max(which(x==y)))
      part_x <- part_y <- 0
      # Weighted distance of the true class from the deepest
      # common ancestor:
      if(length(x) > inddeepanc)
        part_x <- sum(w0^((1:(length(x)-inddeepanc) + (inddeepanc-1))))
      # Weighted distance of the predicted class from the deepest
      # common ancestor:
      if(length(y) > inddeepanc)
        part_y <- sum(w0^((1:(length(y)-inddeepanc) + (inddeepanc-1))))
      # The sum of the above two quantities is the weightest shortest
      # path distance:
      res <- part_x + part_y
      return(res)
    }, truthall, responseall))

  }

  return(result)

}
