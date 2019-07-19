molic: Multivariate OutLIerdetection In Contingency Tables
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.com/mlindsk/molic.svg?token=AuXvB5mAnHuxQxKszxph&branch=master)](https://travis-ci.com/mlindsk/molic)

An **R** package to perform outlier detection in contingency tables using decomposable graphical models (DGMs); models for which the underlying association between all variables can be depicted by an undirected graph. **molic** also offers algorithms for fitting undirected decomposable graphs. Compute-intensive procedures are implementet using [Rcpp](http://www.rcpp.org/)/C++ for better run-timer performance.

Getting Started
---------------

If you want to learn the "behind the scenes" of the model it is recommended to go through the [vignette tutorial](https://mlindsk.github.io/molic/articles/) and look at the [documentation](https://mlindsk.github.io/molic/reference/index.html) as you read along. The material has been written in hope that laymen (non-statisticians) can follow along and comprehend the core idea.

Installation
------------

You can install the development version of the package by using the `devtools` package:

``` r
devtools::install_github("mlindsk/molic", build_vignettes = TRUE)
```

Main Functions
--------------

A list of some core functions in the **molic** package is listed below

| Function        | Description                                                 |
|:----------------|:------------------------------------------------------------|
| `outlier_model` | Fits an outlier detection model                             |
| `efs`           | Fits a decomposable graph using forward selection.          |
| `cl_tree`       | Fits a tree (decomposable graph) using the Chow-Liu method. |
| `p_val`         | Calculates the p-value of an observation being an outlier   |

Example
-------

TBA

References
==========

### Outlier Detection in Contingency Tables Using Decomposable Graphical Models

TBA

### Efficient Forward Selection

Deshpande, A., Garofalakis, M. and Jordan, M. I. (2001) Efficient stepwise selection in decomposable models. In Proceedings of the Seventeenth conference on Uncertainty in artificial intelligence, 128–135. Morgan Kaufmann Publishers Inc. <https://arxiv.org/abs/1301.2267>

Altmueller, S. M. and Haralick, R. M. (2004) Practical aspects of efficient forward selection in decomposable graphical models. In 16th IEEE International Conference on Tools with Artificial Intelligence, 710–715. IEEE. URL: <https://doi.org/10.1109/ictai.2004.100>
