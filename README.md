molic: Multivariate OutLIerdetection In Contingency Tables
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis Build Status](https://travis-ci.com/mlindsk/molic.svg?token=AuXvB5mAnHuxQxKszxph&branch=master)](https://travis-ci.com/mlindsk/molic) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/mlindsk/molic?branch=master&svg=true)](https://ci.appveyor.com/project/mlindsk/molic)

About molic
-----------

An **R** package to perform outlier detection in contingency tables using decomposable graphical models (DGMs); models for which the underlying association between all variables can be depicted by an undirected graph. **molic** also offers algorithms for fitting undirected decomposable graphs. Compute-intensive procedures are implementet using [Rcpp](http://www.rcpp.org/)/C++ for better run-timer performance.

Getting Started
---------------

If you want to learn the "behind the scenes" of the model it is recommended to go through the [vignette tutorial](https://mlindsk.github.io/molic/articles/) and look at the [documentation](https://mlindsk.github.io/molic/reference/index.html) as you read along.

Installation
------------

You can install the development version of the package by using the `devtools` package:

``` r
devtools::install_github("mlindsk/molic", build_vignettes = TRUE)
```

Main Functions
--------------

A list of some core functions in the **molic** package is listed below

<table style="width:67%;">
<colgroup>
<col width="25%" />
<col width="41%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">Function</th>
<th align="left">Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left"><code>outlier_model</code></td>
<td align="left">Fits an outlier detection model</td>
</tr>
<tr class="even">
<td align="left"><code>p_val</code></td>
<td align="left">Calculates the p-value of an observation being an outlier</td>
</tr>
<tr class="odd">
<td align="left"><code>efs</code></td>
<td align="left">Fits a decomposable graph using forward selection.</td>
</tr>
<tr class="even">
<td align="left"><code>cl_tree</code></td>
<td align="left">Fits a tree (decomposable graph) using the Chow-Liu method.</td>
</tr>
</tbody>
</table>

Example
-------

TBA

How To Cite
-----------

TBA
