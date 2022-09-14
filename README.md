# Hierarchical Classification and Hierarchical Performance Metrics

Classification of hierarchically structured outcomes and 
corresponding, suitable performance and loss measures. Currently, top-down
hierarchical classification using random forests as local classifiers is
implemented.
The performance/loss measures implemented include the hierarchical F-score,
the (weighted) shortest path distance, and the H-loss.



```r
library(mlr3)
library(hierclass)
```


```r
# Get help on a metric with
?mlr_measures_classif.hierfbeta
```