#' @title Hierarchical Top-down Classification Learner
#' @author RomanHornung
#' @name mlr_learners_classif.topdown
#'
#' @description
#' Calls [hierclass::topdown()] from \CRANpkg{hierclass}.
#'
#' @templateVar id classif.topdown
#' @template learner
#'
# @references
# `r format_bib(FIXME: ONE OR MORE REFERENCES FROM bibentries.R)`
#'
#' @template seealso_learner
#' @template example
#' @export
LearnerClassiftopdown = R6::R6Class("LearnerClassiftopdown",
  inherit = mlr3::LearnerClassif,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = paradox::ps(
        mtry = paradox::p_int(lower = 1L, tags = "train"),
        sample.fraction = paradox::p_dbl(lower = 0, upper = 1, default = 0.667, tags = "train"),
        confid = paradox::p_dbl(lower = 0.0001, upper = 1, default = 1, tags = "predict")
      )

      param_set$values = list(sample.fraction=0.667, confid=1.)

      super$initialize(
        id = "classif.topdown",
        packages = "hierclass",
        feature_types = c("logical", "integer", "numeric", "factor", "ordered"),
        predict_types = c("response"),
        param_set = param_set,
        properties = c("multiclass"),
        man = "mlr3extralearners::mlr_learners_classif.topdown",
        label = "Hierarchical Top-down Classification"
      )
    }
  ),
  private = list(
    .train = function(task) {
      # get parameters for training
      pars = self$param_set$get_values(tags = "train")

      task = set_hlevels(task)

      # FIXME: CREATE OBJECTS FOR THE TRAIN CALL
      # AT LEAST "data" AND "formula" ARE REQUIRED
      formula = task$formula()
      data = task$data()
	
      mlr3misc::invoke(
        hierclass::topdown,
        formula = formula,
        data = as.data.frame(data),
        .args = pars
      )
    },
    .predict = function(task) {
      # get parameters with tag "predict"
      pars = self$param_set$get_values(tags = "predict")

      # get newdata and ensure same ordering in train and predict
      newdata = ordered_features(task, self)

      # Calculate predictions for the selected predict type.
      type = self$predict_type

      pred = mlr3misc::invoke(predict, self$model, data = as.data.frame(newdata), type = type, .args = pars)

      # FIXME: ADD PREDICTIONS TO LIST BELOW
      list(response = pred)
    }
  )
)

mlr_learners$add("classif.topdown", LearnerClassiftopdown)
