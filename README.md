# **M**ultivariate **O**ut**l**ierdetection **I**n **C**ontingency Tables

An **R** package to perform outlier detection in contingency tables. The methodology relies on a given decomposable graph. The main functions are 

- `outlier_model` fit a model from which outlier detection can be conducted
- `efs`: Learn a decomposable graph (as an input to `outlier_model`)

# `outlier_model`

## Description

 Outlier tests in contingency tables using decomposable graphical models

## Usage

```r
outlier_model(df, adj = NULL, nsim = 1000, ncores = 1, meta_name = "")
```

## Arguments

Argument      |Description
------------- |----------------
```df```     |     Data frame
```adj```     |     Adjacency list
```nsim```     |     Number of simulations
```ncores```     |     Number of cores to use in parallelization
```meta_name```     |     A meta name to keep track of different outlier models

# `efs`

## Description

 Efficient Forward Selection in Decomposable Graphical Models

## Usage

```r
efs(df, x = efs_init(df), trace = TRUE, stop_crit = "mdl1", d = 3, thres = 5)
```

## Arguments

Argument      |Description
------------- |----------------
```df```     |     Dataframe
```x```     |     An efs object
```trace```     |     Logical indidcating whether or not to trace the procedure
```stop_crit```     |     Stopping criterion (mdl1, mdl2, aic or bic)
```d```     |     Number of bits to encode a single parameter
```thres```     |     A threshold mechanism for choosing between two different ways of calculating the entropy

# Installation

You can install the development version of the package by using the `devtools` package:

```r
devtools::install_github("mlindsk/molic")
```

# References
To come...
