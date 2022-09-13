#' Prediction with new data and a saved hierarchical top-down prediction rule from \code{topdown}.
#' Optional or mandatory leaf-node prediction (see for example: Costa et al. (2007)) can be performed. See the 'Details' section
#' for explanation.
#'
#' Each non-leaf node in the category tree is associated with a local classifier (learned
#' by \code{\link{topdown}}). For prediction, starting with the root node, each new test observation is dropped down
#' the category tree in the following way.
#' At each node, the local classifier associated with that node is used to make
#' a decision on the next sub-class. For example, let the prediction of the classifier
#' for the test observation at the root node be 'animalia'. Then, in the next step, the classifier at the node
#' associated with 'animalia' is applied to the test observation. Let the prediction of this classifier be
#' 'chordata'. Then, in the next step, the classifier at the node
#' associated with 'chordata' is applied to the test observation (at this point the prediction would be 'animalia.chordata'). This process
#' can be continued until a leaf node is reached or it can be stopped as soon
#' as a prediction of a local classifier is not sufficiently reliable ("early stopping" or
#' "optional leaf-node prediction").\cr
#' The parameter \code{confid} determines how reliable a prediction needs to be
#' for the iterative prediction process described above to continue at each node rather
#' than stopping. The local classifiers at each node output a predicted probability
#' for each of the sub-classes, where these probabilities sum up to one. The predicted
#' sub-class is chosen as the sub-class with the maximum predicted probability. The value
#' of \code{confid} is associated with a specific threshold t_confid this maximum predicted
#' probability needs to exceed for the prediction of the next sub-class to be performed. If this probability
#' is below the threshold t_confid, the prediction process stops. More precisely, the value
#' of \code{confid} can be interpreted as follows: If the predicted probabilities were
#' random, where each sub-class would have an expected predicted probability of 1/L,
#' where L is the number of sub-classes, \code{confid} would be the probability that
#' the maximum predicted probability exceeds the threshold t_confid. Thus, the smaller
#' \code{confid} is, the larger t_confid becomes and the earlier the prediction process tends
#' to stop. Thus smaller \code{confid} values correspond to more coarse predictions with less
#' depth, which, however, tend to be more reliable than finer predictions.\cr
#' In cases in which the maximum probability would be smaller than t_confid already at the highest
#' node, we would not obtain a prediction at all, which we want to avoid. Therefore,
#' we always obtain predictions using the first classifier and consider early
#' stopping only after this first prediction. In the example from above, we would predict
#' 'animalia' even if the probability for 'animalia' would be smaller than t_confid.\cr\cr
#' Some more technical details: The vector of L random predicted probabilities with expected values
#' of 1/L (L: number of sub-classes) follows a Dirichlet distribution, but the maximum
#' of that vector, which we are interested in, does not follow a closed-form distribution.
#' Therefore, we were not able to determine the values of t_confid analytically. Instead,
#' we approximated each t_confid value using 10^7 draws from a Dirichlet distribution
#' and rounded the results to three decimal places. Note that the value t_confid does
#' not only depend on \code{confid}, but also on the number of sub-classes L. Therefore,
#' we approximated the value of t_confid for each combination of \code{confid} values on a grid
#' \{0.01, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 1\}
#' and L values on a grid \{2, 3, ..., 15\}. For L values larger than 15 we always stop the
#' prediction process (assuming that L values larger than 15 are rare in practice).
#'
#' @title Prediction function for top-down hierarchical prediction rules.
#' @param object Object of class \code{topdown}. The top-down hierarchical prediction rule.
#' @param data New test data of class \code{data.frame} for which predictions should be obtained.
#' @param confid The parameter controlling the early stopping of the predictions. Has to
#' be one of the following values: 0.01, 0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 1. The smaller this value,
#' the more prone the prediction algorithm is to stop early, which leads to coarser
#' predictions with less depth, which, however, tend to be more reliable than finer
#' predictions. Selecting a value of 1 leads to no early stopping ("mandatory leaf-node prediction").
#' See the 'Details' section for more details on this parameter and the prediction process in general. Per default no early
#' stopping is performed (\code{confid=1}).
#' @param ... further arguments passed to or from other methods.
#' @return A \code{factor} variable that provides a prediction for each observation
#' in the new data \code{data}.
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
#' ## Compare the first predictions with the true labels:
#'
#' head(data.frame(preds=preds, truth=datatest$ydepvar))
#' }
#'
#' @author Roman Hornung
#' @references
#' \itemize{
#'   \item Costa, E. P., Lorena, A. C., Carvalho, A. C. P. L. F., Freitas, A. A. (2007) A review of performance evaluation measures for hierarchical classifiers. In: C. Drummond, W. Elazmeh, N. Japkowicz, S.A. Macskassy (Ed.), Evaluation Methods for Machine Learning II: papers from the AAAI-2007 Workshop, AAAI Technical Report WS-07-05. AAAI.
#'   }
#' @seealso \code{\link{topdown}}
#' @encoding UTF-8
#' @import mlr3
#' @export
predict.topdown <- function(object, data, confid=1, ...) {

  # Check whether the 'confid' parameter is specified appropriately. If it
  # is not on the prespecified grid, set it to the value closest  on the
  # grid.
  if(!is.numeric(confid) | confid <= 0 | confid > 1)
    stop("'confid' must be a numeric value in the interval ]0, 1].")

  if(!(confid %in% as.numeric(colnames(thresholdmat)))) {
    confidvalues <- as.numeric(colnames(thresholdmat))
    confid <- confidvalues[nnet::which.is.max(1-abs(confidvalues-confid))]
    warning(paste0("'confid' must take one of the following values: ",
                   paste(colnames(thresholdmat), collapse=", "),
                   ". 'confid' set to ", confid))
  }

  # Check whether all independent variables are present in the data:
  if (sum(!(object$modellist[[max(which(sapply(object$modellist, function(x) class(x$learner)[1]!="logical")))]]$learner$model$forest$independent.variable.names %in% colnames(data))) > 0) {
    stop("One or more independent variables not found in data.")
  }

  # We obtain the predictions of all forests for all observations, even though
  # each forest only will see a fraction of the observations.
  # However, it is computationally more efficient to call 'ranger::predict' on
  # all observations beforehand than separately for each observation while
  # dropping the observations down the category tree.
  for(i in seq(along=object$modellist)) {
    if(length(object$modellist[[i]]$subclasses) > 1)
      object$modellist[[i]]$preds <- predict(object$modellist[[i]]$learner, newdata=data, predict_type = "prob")
  }

  # Drop the observations down the category tree:

  thresholds <- thresholdmat[,as.character(confid)]

  preds <- vector(mode="character", length=nrow(data))

  for(i in 1:nrow(data)) {

    # Obtain a prediction using the classifier in the root node:

    predprobs <- object$modellist[[1]]$preds[i,]

    predclass <- names(predprobs)[nnet::which.is.max(predprobs)]
    preds[i] <- predclass

    # Drop the observation further down the category tree:

    if(!is.na(preds[i])) {

      repeat {

        # If, for the current class length(object$modellist[[predclass]]$subclasses)==1,
        # this can mean that is.na(object$modellist[[predclass]]$subclasses)==TRUE
        # which indicates that the current class is a leaf node or
        # the current class only has one subclass:
        stoploop <- FALSE
        while(length(object$modellist[[predclass]]$subclasses)==1) {
          # In case the current class has no subclass
          # (indicated by is.na(object$modellist[[predclass]]$subclasses)==TRUE),
          # the class is a leaf node and we stop the prediction process:
          if(is.na(object$modellist[[predclass]]$subclasses)) {
            stoploop <- TRUE
            break
          }
          else {
            # In rare cases a class only has a single subclass. In that case,
            # of course, the next predicted class is that single subclass:
            predclass <- object$modellist[[predclass]]$subclasses
            preds[i] <- paste0(preds[i], ".", predclass)
          }
        }
        if(stoploop)
          break

        # If the current class has more than one subclasses we obtain a prediction
        # using the classifier associated with the current node:

        predprobs <- object$modellist[[predclass]]$preds[i,]

        if(as.numeric(names(thresholds)) <= length(object$modellist[[predclass]]$subclasses))
          threshold <- thresholds[as.character(length(object$modellist[[predclass]]$subclasses))]
        else
          break

        # We obtain a prediction for the next subclass only if the maximum
        # probability exceeds the threshold:
        if(max(predprobs) > threshold) {

          predclass <- names(predprobs)[nnet::which.is.max(predprobs)]
          preds[i] <- paste0(preds[i], ".", predclass)

        } else {
          break
        }

      }

    }

  }

  # Assign the class levels obtained from the training
  # data:
  preds <- factor(preds, levels=object$allclasses)

  return(preds)

}
