test_that("learner works", {
    ###devtools::load_all()
    data(datasim)
    tsk  = as_task_classif(datasim, target = "ydepvar")
    ll = lrn("classif.topdown")
    ll$train(tsk)

    ll$predict(tsk)
}
)
