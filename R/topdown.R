#' Implements top-down hierarchical classification (see for example: Naik & Rangwala (2018)) with random forests as local classifiers.
#' Single-label data are supported and the outcome classes have to be organized
#' as a category tree. An example of a hierarchical classification problem of this
#' kind would be species categorization, where the kingdom would be broadest category,
#' followed by phylum, class, order, family,
#' genus, and species. Humans would be categorized as follows: 1) animalia (kingdom),
#' 2) chordata (phylum), 3) mammalia (class), 4) primates (order), 5) hominidae (family),
#' 6) homo (genus), 7) sapien (species). All this information has to be present for each observation
#' in the data via a single outcome variable in the \code{data.frame} provided via the
#' function argument \code{data}. The entries of this variable have to be structured in
#' a certain form, please see the 'Details' section below for explanation.
#'
#' As stated above, the outcome variable provides the information on the outcome
#' class for each observation. Each entry of the outcome variable must start with
#' the broadest category, followed by a dot ("."), followed by the second-broadest
#' category, followed by a dot ("."), and so on. Consider, for example, the
#' species categorization described above. Here, an entry for a human would look
#' like this: 'animalia.chordata.mammalia.primates.hominidae.homo.sapien'.
#' In the above example, the categorization has depth 7. That is, there are
#' seven degrees of fineness (first degree "kingdom", second degree "phylum",
#' and so on). The function can also deal with hierarchical classification
#' problems, for which the different branches of the category tree have different lengths
#' (or "depths"). For example, for one observation the categorization may have
#' depth 7, but for another observation the categorization (from a different branch
#' of the category tree) may have only depth 3.\cr
#' IMPORTANT: The names of the classes from all levels in the category tree need to be unique.
#' For example, it is not possible that hierarchy level 5 contains a class that
#' has the same name as a class from hierarchy level 3.\cr\cr
#' The outcome variable can be a factor (recommended) or a character.
#' The covariates (or "input features") can be categorical or numeric.
#' For training each local classifier, only the observations from the
#' category associated with the current node are used. For example, at the node
#' of class 'mammalia', only the observations from the class 'mammalia' are used
#' for learning a classifier that distinguishes between the sub-classes of
#' 'mammalia' (e.g., 'primates').\cr
#' The random forests used as local classifiers perform probability predictions.
#' At the prediction stage, the sub-class with the largest predicted probability
#' is selected. If several classes have the maximum probability, a random
#' decision is made for one of these classes with maximum probability.
#'
#' @title Construct a top-down hierarchical classification prediction rule.
#' @param formula Object of class \code{formula} or \code{character} describing the model to fit.
#' @param data Training data of class \code{data.frame}. See the 'Details' section for
#' the structure required for the outcome variable.
#' @param dependent.variable.name Name of outcome variable, needed if no formula given.
#' @param num.trees Number of trees used in the random forests. The default is 500.
#' @param mtry Number of variables to possibly split at in each node in the random forests. Default is the (rounded down) square root of the number variables.
#' @param sample.fraction Fraction of observations to sample. Default is 1 for sampling with replacement and 0.667 for sampling without replacement. For classification, this can be a vector of class-specific values.
#' @param maxnthreads Should the maximum number of threads available be used (\code{TRUE}) or should
#' the default parallelization from the package \code{mlr3} be used (\code{FALSE}).
#' Default is \code{TRUE}.
#' @return Object of class \code{topdown} with elements
#'   \item{\code{modellist}}{This list contains the local classifiers, which are used
#'   in prediction through the function \code{\link{predict.topdown}}. It is a list
#'   of length equal to the number of nodes in the category tree, including the root node.
#'   Each list element is a list itself with two elements: \code{sub-classes} and \code{learner}.
#'   The first element, \code{sub-classes}, contains the sub-classes (or "direct descendants")
#'   of the node associated with the corresponding entry of \code{modellist}. For so-called
#'   leaf nodes, that is, classes with no descendants, \code{sub-classes} is NA. The second element,
#'   \code{learner}, provides the classifier object, which is a \code{mlr3} learner, containing
#'   a random forest prediction rule fitted by the \code{ranger} package. For leaf nodes, \code{learner}
#'   is NA because for leaf nodes no further classifications have to be performed.}
#'   \item{\code{allclasses}}{This is a factor variable, which contains all levels
#'   the predictions of test data can have. In prediction, we do not necessarily
#'   continue the top-down classification until the leaf nodes are reached, but
#'   the process is stopped as soon as the certainty of a classification is below
#'   a certain threshold (see \code{\link{predict.topdown}} for details).
#'   Therefore, classes can be predicted that did not occur in training. For example,
#'   for a human, we may predict 'animalia.chordata.mammalia.primates'
#'   instead of 'animalia.chordata.mammalia.primates.hominidae.homo.sapien'.
#'   We return the set of all possible class predictions \code{allclasses} because
#'   these are the factor levels assigned to the predictions in \code{\link{predict.topdown}}.}
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
#' object
#' }
#'
#' @author Roman Hornung
#' @references
#' \itemize{
#'   \item Naik, A., Rangwala, H. (2018). Large scale hierarchical classification: State of the art. Springer, Berlin, <\doi{10.1007/978-3-030-01620-3}>.
#'   }
#' @seealso \code{\link{predict.topdown}}
#' @encoding UTF-8
#' @import stats
#' @import mlr3learners
#' @export
topdown <- function(formula, data, dependent.variable.name, num.trees=500,
                    mtry, sample.fraction=0.667, maxnthreads=TRUE) {

  ## Formula interface. Use whole data frame is no formula provided and depvarname given
  if (is.null(formula)) {
    if (is.null(dependent.variable.name)) {
      stop("Error: Please give formula or outcome variable name.")
    }
    response <- data[, dependent.variable.name, drop = TRUE]
    data.selected <- data
    yvarname <- dependent.variable.name
  } else {
    formula <- formula(formula)
    if (!inherits(formula, "formula")) {
      stop("Error: Invalid formula.")
    }
    data.selected <- parse.formula(formula, data, env = parent.frame())
    response <- data.selected[, 1]
    yvarname <- names(data.selected)[1]
  }


  ## Check missing values
  if (any(is.na(data.selected))) {
    offending_columns <- colnames(data.selected)[colSums(is.na(data.selected)) > 0]
    stop("Missing data in columns: ",
         paste0(offending_columns, collapse = ", "), ".", call. = FALSE)
  }

  # Set mtry to the default value if it is missing:
  if(missing(mtry))
    mtry <- floor(sqrt(ncol(data.selected)-1))


  # Get the unique classes and order them according their depth:

  yun <- as.character(unique(response))
  yun <- yun[order(sapply(yun, function(x) length(strsplit(x, split="\\.")[[1]])))]


  # Make a matrix which contains the class hierarchy, where each column
  # coorresponds to a specific level of the hierarchy (used lateron for determening
  # the direct descendants of the internal nodes):

  ylist <- sapply(yun, function(x) strsplit(x, split="\\.")[[1]])

  # all classes and sub classes, used to assign the
  # appropriate class levels to the predictions performed by predict.topdown:
  allclasses <- unique(unlist(sapply(ylist, function(x) sapply(1:length(x), function(y) paste(x[1:y], collapse=".")))))

  mdepth <- max(sapply(ylist, length))

  ymat <- t(sapply(ylist, function(x) {
    c(x, rep(NA, mdepth-length(x)))
  }))



  # All classes, external and internal ones:

  yclasses <- unique(as.vector(ymat))
  yclasses <- yclasses[!is.na(yclasses)]



  # For each class get its direct descendants:

  modellist <- vector(mode = "list", length = length(yclasses)+1)

  modellist[[1]] <- list()
  modellist[[1]]$subclasses <- unique(ymat[,1])

  for(i in seq(along=yclasses)) {

    modellist[[i+1]] <- list()

    indtemp <- which(apply(ymat, 2, function(x) yclasses[i] %in% x))

    if(sum(ymat[,indtemp]==yclasses[i], na.rm=TRUE) > 1)
      modellist[[i+1]]$subclasses <- unique(ymat[,indtemp+1][!is.na(ymat[,indtemp]) & ymat[,indtemp]==yclasses[i]])
    else
      modellist[[i+1]]$subclasses <- NA

  }

  names(modellist) <- "zeronode"
  names(modellist)[-1] <- yclasses



  # Add to the data set new columns, where each corresponds to one of the classification
  # problems in the category tree (these will be used by ranger lateron):

  yall <- sapply(as.character(response), function(x) strsplit(x, split="\\.")[[1]])

  for(i in seq(along=modellist)) {

    if(length(modellist[[i]]$subclasses) > 1) {

      data.selected$newvariable <- NA

      for(j in seq(along=modellist[[i]]$subclasses)) {
        data.selected$newvariable[sapply(yall, function(x) modellist[[i]]$subclasses[j] %in% x)] <- modellist[[i]]$subclasses[j]
      }
      data.selected$newvariable <- factor(data.selected$newvariable, levels=modellist[[i]]$subclasses)
      names(data.selected)[length(names(data.selected))] <- names(modellist)[i]

    }

  }


  # Learn a random forest for each classification problem in the tree:

  for(i in seq(along=modellist)) {

    if(length(modellist[[i]]$subclasses) > 1) {

      datatemp <- data.selected[,!(names(data.selected) %in% c(yvarname, names(modellist)[-i]))]
      names(datatemp)[names(datatemp)==names(modellist)[i]] <- "yvartemp"
      datatemp <- datatemp[!is.na(datatemp$yvartemp),]

      tasktemp <- mlr3::as_task_classif(yvartemp ~ ., data = datatemp)

      if(maxnthreads)
        learner <- mlr3::lrn("classif.ranger", predict_type = "prob", num.threads=parallel::detectCores())
      else
        learner <- mlr3::lrn("classif.ranger", predict_type = "prob")
      learner$param_set$values <- mlr3misc::insert_named(learner$param_set$values,
                                                         list(num.trees = num.trees, mtry = mtry, replace = FALSE, sample.fraction = sample.fraction))

      learner$train(tasktemp)

      modellist[[i]]$learner <- learner

    } else
      modellist[[i]]$learner <- NA

  }

  output <- list(modellist=modellist, allclasses=allclasses)

  # Assign class to the output:
  class(output) <- "topdown"

  # Return the output:
  return(output)

}
