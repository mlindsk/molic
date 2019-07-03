<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build
Status](https://travis-ci.com/mlindsk/molic.svg?token=AuXvB5mAnHuxQxKszxph&branch=master)](https://travis-ci.com/mlindsk/molic)

molic: Multivariate OutLIerdetection In Contingency Tables
==========================================================

An **R** package to perform outlier detection in contingency tables
using decomposable graphical models; models for which the underlying
association between all variables can be depicted by an undirected
graph. The main functions are described below.

### Outlier Detection

A model based on decomposable graphical models for outlier detection.

-   `outlier_model(A, adj, nsim = 1000, ncores = 1, meta_name = "", validate_A = TRUE)`

<table style="width:43%;">
<colgroup>
<col width="19%" />
<col width="23%" />
</colgroup>
<thead>
<tr class="header">
<th>Argument</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>A</code></td>
<td>Character matrix (data)</td>
</tr>
<tr class="even">
<td><code>adj</code></td>
<td>Adjacency list of a decomposable graph</td>
</tr>
<tr class="odd">
<td><code>nsim</code></td>
<td>Number of simulations</td>
</tr>
<tr class="even">
<td><code>ncores</code></td>
<td>Number of cores to use in parallelization</td>
</tr>
<tr class="odd">
<td><code>meta_name</code></td>
<td>A meta name to keep track of different outlier models</td>
</tr>
<tr class="even">
<td><code>validate_A</code></td>
<td>If TRUE, it is checked if all values in A are characters with <code>nchar == 1</code> (which is required)</td>
</tr>
</tbody>
</table>

It is assumed that all cell values in `A` , for all are represented as a
single character. If `validate_A` is `TRUE` this is checked. If cell
values are not single characters, one may exploit `letters` and
`LETTERS` e.g.

### Model Selection in DGMs

Efficient forward selection in decomposable graphical models.

-   `efs(df, x = efs_init(df), trace = TRUE, stop_crit = "mdl", thres = 5)`

<table style="width:43%;">
<colgroup>
<col width="19%" />
<col width="23%" />
</colgroup>
<thead>
<tr class="header">
<th>Argument</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>df</code></td>
<td>data.frame</td>
</tr>
<tr class="even">
<td><code>x</code></td>
<td>A efs object</td>
</tr>
<tr class="odd">
<td><code>trace</code></td>
<td>Logical indicating whether or not to trace the procedure</td>
</tr>
<tr class="even">
<td><code>stop_crit</code></td>
<td>Stopping criterion (&quot;mdl&quot;, &quot;aic&quot; or &quot;bic&quot;)</td>
</tr>
<tr class="odd">
<td><code>thres</code></td>
<td>A threshold mechanism for choosing between two different ways of calculating the entropy. Can Speed up the procedure with the &quot;correct&quot; value.</td>
</tr>
</tbody>
</table>

Use `efs_adj_list` or `efs_adj_matrix` to extract the fitted graph.

### Simulation

-   `dgm_sim(A, adj, nsim = 1000, ncores = 1)`:

<table>
<thead>
<tr class="header">
<th>Argument</th>
<th>Description</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>A</code></td>
<td>Character matrix (data)</td>
</tr>
<tr class="even">
<td><code>adj</code></td>
<td>Adjacency list of a decomposable graph</td>
</tr>
<tr class="odd">
<td><code>nsim</code></td>
<td>Number of simulations</td>
</tr>
<tr class="even">
<td><code>ncores</code></td>
<td>Number of cores to use in parallelization</td>
</tr>
</tbody>
</table>

This function returns a matrix of dimension `nsim x ncol(A)` where each
row correspond to a simulated observation from a DGM represented by
`adj`.

Example
=======

TBA

Installation
============

You can install the development version of the package by using the
`devtools` package:

    devtools::install_github("mlindsk/molic")

References
==========

Outlier Detection in Contingency Tables Using Decomposable Graphical Models
---------------------------------------------------------------------------

TBA

Efficient Forward Selection
---------------------------

Deshpande, A., Garofalakis, M. and Jordan, M. I. (2001) Efficient
stepwise selection in decomposable models. In Proceedings of the
Seventeenth conference on Uncertainty in artificial intelligence,
128–135. Morgan Kaufmann Publishers Inc.
<https://arxiv.org/abs/1301.2267>

Altmueller, S. M. and Haralick, R. M. (2004) Practical aspects of
efficient forward selection in decomposable graphical models. In 16th
IEEE International Conference on Tools with Artificial Intelligence,
710–715. IEEE. URL: <https://doi.org/10.1109/ictai.2004.100>
