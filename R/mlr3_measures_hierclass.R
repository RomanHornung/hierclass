#' @title Hierarchical Performance/Loss Measures
#' @author Florian Pfisterer
#' @name mlr_measures_hierclass
#'
#' @include hierfbeta.R
#' @include hierre.R
#' @include hierpr.R
#' @include hloss.R
#' @include spath.R
#'
#' @description
#' To Do Florian?
#'
#' @seealso \code{\link{hierre}}, \code{\link{hierpr}}, \code{\link{hierfbeta}}, \code{\link{hloss}}, \code{\link{spath}}
#' @export
#' @examples
#' # Load the required packages:
#' library("mlr3")
#' library("hierclass")
#'
#' # Set seed for reproducibility:
#' set.seed(1234)
#'
#' # Load example dataset:
#' data(datasim)
#'
#' # Define the task for the top-down classification rule:
#' task = as_task_classif(ydepvar ~ ., data = datasim)
#'
#' # Initialize the learner for the top-down classification rule:
#' learner = lrn("classif.topdown")
#'
#' # Train a model of this learner for a subset of the task:
#' learner$train(task, row_ids = 1:300)
#' learner$model
#'
#' # Obtain predictions for the remaining observations:
#' predictions = learner$predict(task, row_ids = 301:560)
#' predictions
#'
#' # Measure the performance according to different performance/loss metrics:
#'
#' # Hierarchical F-score:
#' measure = msr("classif.hierfbeta")
#' predictions$score(measure)
#'
#' # Hierarchical precision:
#' measure = msr("classif.hierpr")
#' predictions$score(measure)
#'
#' # Hierarchical recall:
#' measure = msr("classif.hierre")
#' predictions$score(measure)
#'
#' # H-loss measure:
#' measure = msr("classif.hloss")
#' predictions$score(measure)
#'
#' # Shortest path loss measure:
#' measure = msr("classif.spath")
#' predictions$score(measure)
#'
#' # Classification accuracy (not a hierarchical performance/loss measure):
#' measure = msr("classif.acc")
#' predictions$score(measure)
#' @export
# Abstract Base Class for Hierarchical Classification Measures
MeasureClassifHierarchical = R6::R6Class("MeasureClassifHierarchical",
    inherit = mlr3::MeasureClassif,
    public = list(
        fun = NULL,
        param_set = NULL,
        initialize = function(fun, param_set = paradox::ps()) {
            super$initialize(
                # custom id for the measure
                id = paste0("classif", deparse(substitute(fun))),

                # additional packages required to calculate this measure
                packages = character(),

                # properties, see below
                properties = character(),

                # required predict type of the learner
                predict_type = "response",

                # feasible range of values
                range = c(0, Inf),

                # minimize during tuning?
                minimize = TRUE
            )
            self$fun = checkmate::assert_function(fun)
            self$param_set = param_set
        }
    ),
    private = list(
        .score = function(prediction, ...) {
            mlr3misc::invoke(self$fun, .args = self$param_set$get_values(),
                truth = prediction$truth, response = prediction$response
            )
        }
    )
)

#' @templateVar id hierfbeta
#' @template measure_hierarchical
measure_hierfbeta = MeasureClassifHierarchical$new(
    fun = hierfbeta,
    param_set = paradox::ps(
        beta = paradox::p_int(lower = 0),
        type = paradox::p_fct(levels = c("micro", "macro"))
    )
)

#' @templateVar id hierpr
#' @template measure_hierarchical
measure_hierpr = MeasureClassifHierarchical$new(
    fun = hierpr,
    param_set = paradox::ps(
        type = paradox::p_fct(levels = c("micro", "macro"))
    )
)

#' @templateVar id hierre
#' @template measure_hierarchical
measure_hierre = MeasureClassifHierarchical$new(
    fun = hierre,
    param_set = paradox::ps(
        type = paradox::p_fct(levels = c("micro", "macro"))
    )
)

#' @templateVar id hloss
#' @template measure_hierarchical
measure_hloss = MeasureClassifHierarchical$new(
    fun = hloss,
    param_set = paradox::ps(
        w0 = paradox::p_dbl(lower = 0, upper = 1)
    )
)

#' @templateVar id spath
#' @template measure_hierarchical
measure_spath = MeasureClassifHierarchical$new(
    fun = spath,
    param_set = paradox::ps(
        type = paradox::p_fct(levels = "weighted"),
        w0 = paradox::p_dbl(lower = 0, upper = 1)
    )
)
