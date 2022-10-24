
# Hierarchical Classification and Hierarchical Performance Metrics

Classification of hierarchically structured outcomes and 
corresponding, suitable performance and loss measures. Currently, top-down
hierarchical classification using random forests as local classifiers is
implemented.
The performance/loss measures implemented include the hierarchical F-score,
the (weighted) shortest path distance, and the H-loss.

The classification approach and the hierarchical performance and loss measures can be used
in conjunction with the R package 'mlr3'. By loading the 'mlr3' package together
with the 'hierclass' package, the top-down classification approach is available
as an mlr3 learner and the hierarchical performance and loss measures are
available as mlr3 measures.

## Installation

The package can be installed from GitHub:

``` r
remotes::install_github("RomanHornung/hierclass")
```

## Example

### Constructing a Task and Learner for Top-Down Hierarchical Classification

``` r
# Load the required packages:
library("mlr3")
library("hierclass")

# Set seed for reproducibility:
set.seed(1234)

# Load example dataset:
data(datasim)

# Define the task for the top-down classification rule:
task = as_task_classif(ydepvar ~ ., data = datasim)

# Initialize the learner for the top-down classification rule:
learner = lrn("classif.topdown")
```

### Train and Predict

``` r
# Train a model using the above learner for a subset of the task:
learner$train(task, row_ids = 1:300)
learner$model
```

    ## topdown object
    ## 
    ## Number of classes:            23 
    ## Number of leaf node-classes:  14

``` r
# Obtain predictions for the remaining observations:
predictions = learner$predict(task, row_ids = 301:560)
predictions
```

    ## <PredictionClassif> for 260 observations:
    ##     row_ids                 truth              response
    ##         301          c1.c4.c9.c12     c2.c6.c11.c16.c20
    ##         302                 c1.c3     c2.c6.c11.c16.c20
    ##         303      c1.c4.c9.c14.c18      c1.c4.c9.c14.c18
    ## ---                                                    
    ##         558              c1.c4.c8 c2.c6.c11.c16.c21.c22
    ##         559 c2.c6.c11.c16.c21.c22 c2.c6.c11.c16.c21.c22
    ##         560      c1.c4.c9.c14.c18      c1.c4.c9.c14.c18

### Measure the Performance According to Various Hierarchical Performance/Loss Metrics

``` r
# Hierarchical F-score:
measure = msr("classif.hierfbeta")
predictions$score(measure)
```

    ## classifhierfbeta 
    ##        0.5721372 

``` r
# Hierarchical precision:
measure = msr("classif.hierpr")
predictions$score(measure)
```

    ## classifhierpr
    ##     0.5187406  

``` r
# Hierarchical recall:
measure = msr("classif.hierre")
predictions$score(measure)
```

    ## classifhierre
    ##      0.637788

``` r
# H-loss measure:
measure = msr("classif.hloss")
predictions$score(measure)
```

    ## classifhloss
    ##     0.376924  

``` r
# Shortest path loss measure:
measure = msr("classif.spath")
predictions$score(measure)
```

    ## classifspath
    ##     1.543976

``` r
# Classification accuracy (not a hierarchical performance/loss measure):
measure = msr("classif.acc")
predictions$score(measure)
```

    ## classif.acc 
    ##        0.15 
