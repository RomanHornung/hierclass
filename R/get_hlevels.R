get_hlevels = function(x, splitter = '.') {
    assert_factor(x)
    assert_character(splitter)
    # Split into levels
    dt = rbindlist(mlr3misc::map(mlr3misc::map(strsplit(levels(x), splitter, fixed=TRUE), t), data.table), fill = TRUE)
    # Create all level combinations
    dt = data.table(t(dt))
    newlvls = unique(unlist(
        dt[, mlr3misc::map(.SD, function(x) {
        mlr3misc::map(seq_len(ncol(dt)), function(idx){
            x = x[seq_len(idx)]
            x = x[!is.na(x)]
            paste0(x, collapse = splitter)
        })
    })]))
    return(newlvls)
}

set_hlevels = function(task, splitter = '.') {
      newlvls = mlr3misc::map(task$target_names, function(x) get_hlevels(task$data(cols = x)[[1]], splitter = splitter))
      names(newlvls) = task$target_names
      task$set_levels(newlvls)
      return(task)
}

ordered_features = function(task, learner) {
  task$data(cols = intersect(names(learner$state$task_prototype), task$feature_names))
}