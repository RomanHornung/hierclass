#' @import checkmate
#' @import data.table
#' @import paradox
#' @import mlr3
#' @import mlr3misc
#' @import mlr3learners
#' @importFrom R6 R6Class is.R6
#' @importFrom stats predict



register_mlr3 = function() {
    mlr_learners$add("classif.topdown", LearnerClassiftopdown)
    mlr_measures$add("classif.hierfbeta", measure_hierfbeta)
    mlr_measures$add("classif.hierpr", measure_hierpr)
    mlr_measures$add("classif.hierre", measure_hierre)
    mlr_measures$add("classif.hloss", measure_hloss)
    mlr_measures$add("classif.spath", measure_spath)
}

.onLoad = function(libname, pkgname) {  # nocov start
  register_mlr3()
  setHook(packageEvent("mlr3", "onLoad"), function(...) register_mlr3(), action = "append")
  backports::import(pkgname)
}  # nocov end

.onUnload = function(libpath) { # nocov start
   event = packageEvent("mlr3", "onLoad")
   hooks = getHook(event)
   pkgname = vapply(hooks[-1], function(x) environment(x)$pkgname, NA_character_)
   setHook(event, hooks[pkgname != "hierclass"], action = "replace")
} # nocov end
