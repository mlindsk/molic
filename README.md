[![Build Status](https://travis-ci.com/mlindsk/molic.svg?token=AuXvB5mAnHuxQxKszxph&branch=master)](https://travis-ci.com/mlindsk/molic)
  
# molic: Multivariate OutLIerdetection In Contingency Tables

An **R** package to perform outlier detection in contingency tables using decomposable graphical models; models for which the underlying association between all variables can be depicted by an undirected graph. The main functions are 

## Outlier Detection
Fit a model from which outlier detection can be conducted

- `outlier_model(df, adj = NULL, nsim = 1000, ncores = 1, meta_name = "")` 

Argument      |Description
------------- |----------------
```df```     |     Data frame
```adj```     |     Adjacency list of a decomposable graph
```nsim```     |     Number of simulations
```ncores```     |     Number of cores to use in parallelization
```meta_name```     |     A meta name to keep track of different outlier models

## Efficient Forward Selection in Decomposable Graphical Models
Learn a decomposable graph

- `efs(df, x = efs_init(df), trace = TRUE, stop_crit = "mdl1", thres = 5)`:

Argument      |Description
------------- |----------------
```df```     |     Dataframe
```x```     |     An efs object
```trace```     |     Logical indidcating whether or not to trace the procedure
```stop_crit```     |     Stopping criterion (mdl1, mdl2, aic or bic)
```thres```     |     A threshold mechanism for choosing between two different ways of calculating the entropy

## Example
TBA

# Installation

You can install the development version of the package by using the `devtools` package:

```r
devtools::install_github("mlindsk/molic")
```

# References
Deshpande, A., Garofalakis, M. and Jordan, M. I. (2001) Efficient stepwise selection in decomposable models. In Proceedings of the Seventeenth conference on Uncertainty in artificial intelligence, 128–135. Morgan Kaufmann Publishers Inc. https://arxiv.org/abs/1301.2267

Altmueller, S. M. and Haralick, R. M. (2004) Practical aspects of efficient forward selection in decomposable graphical models. In 16th IEEE International Conference on Tools with Artificial Intelligence, 710–715. IEEE. URL: https://doi.org/10.1109/ictai.2004.100
