test_that("learner works", {
    data(datasim)
    trainind <- sample(1:nrow(datasim), size=round((3/4)*nrow(datasim)))
    datatrain <- datasim[trainind,]
    datatest <- datasim[-trainind,]
    object <- topdown(ydepvar ~ ., data=datatrain, num.trees=50)
    expect_class(object, 'topdown')

    predict(object, datatest)
})