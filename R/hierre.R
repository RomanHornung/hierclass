#' @title Hierarchical recall
#' 
#' @description 
#' Hierarchical recall defined by Kiritchenko et al. (2005).
#' Takes the ancestors of the true and
#' predicted classes into account, but has the disadvantage that it overpenalizes errors in nodes with many
#' ancestors (Kosmopoulos et al., 2015).
#' @param truth True (observed) labels. Must have the same length as \code{response}. Can be factor (recommended) or character.
#' These need to be structured in the same way as required for \code{\link{topdown}}. See the 'Details' section of \code{\link{topdown}}
#' for details.
#' @param response Predicted response labels. Must have the same length as \code{truth}. Can be factor (recommended) or character.
#' These need to be structured in the same way as required for \code{\link{topdown}}. See the 'Details' section of \code{\link{topdown}}
#' for details.
#' @param type Type of average to use: The micro-average (\code{type="micro"}) and the macro-average (\code{type="macro"}) give
#' the same weight to all observations and all classes, respectively. Default is "micro".
#' @return The value of the hierarchical recall.
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
#' ## Compute the micro- and macro-average hierarchical precision
#' ## for the predictions of the test data:
#'
#' hierre(truth=datatest$ydepvar, response=preds)
#' hierre(truth=datatest$ydepvar, response=preds, type="macro")
#' }
#'
#' @author Roman Hornung
#' @references
#' \itemize{
#'   \item Kiritchenko, S., Matwin, S., Famili, A. F. (2005). Functional annotation of genes using hierarchical text categorization. In: Proceedings of the BioLINK SIG: Linking Literature, Information and Knowledge for Biology, Detroit, Michigan, USA.
#'   \item Kosmopoulos, A., Partalas, I., Gaussier, E., Paliouras, G., Androutsopoulos, I. (2015). Evaluation measures for hierarchical classification: A unified view and novel approaches. Data Mining and Knowledge Discovery 29(3):820–865, <\doi{10.1007/s10618-014-0382-x}>.
#'   }
#' @seealso \code{\link{hierpr}}, \code{\link{hierfbeta}}, \code{\link{hloss}}, \code{\link{spath}}
#' @encoding UTF-8
#' @export
hierre <- function(truth, response, type="micro") {

  if (!(type %in% c("micro", "macro")))
    stop("'type' has to be either 'micro' or 'macro'.")

  # Convert the factors to characters, because we perform string
  # operations on them:
  truth <- as.character(truth)
  response <- as.character(response)

  # Replace the NA predictions to predictions of the root class:
  response[is.na(response)] <- "rootclass"

  truthall <- sapply(truth, strsplit, split="\\.")
  responseall <- sapply(response, strsplit, split="\\.")

  if (type=="micro") {

    # Micro average:

    nominator <- sum(mapply(function(x, y) length(intersect(x, y)), truthall, responseall))
    denominator <- length(unlist(truthall))

    result <- nominator/denominator

  } else {

    # Macro average:

    truthleaf <- sapply(truthall, function(x) x[length(x)])
    truthleafun <- unique(truthleaf)

    rel <- sapply(truthleafun, function(l) {
      nominator <- sum(mapply(function(x, y) length(intersect(x, y)), truthall[truthleaf==l], responseall[truthleaf==l]))
      denominator <- length(unlist(truthall[truthleaf==l]))
      nominator/denominator
    })

    result <- mean(rel)

  }

  return(result)

}
