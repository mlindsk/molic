
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.com/mlindsk/molic.svg?token=AuXvB5mAnHuxQxKszxph&branch=master)](https://travis-ci.com/mlindsk/molic)

molic: Multivariate OutLIerdetection In Contingency Tables
==========================================================

An **R** package to perform outlier detection in contingency tables using decomposable graphical models; models for which the underlying association between all variables can be depicted by an undirected graph. The main functions are described below.

### Outlier Detection

-   `outlier_model(A, adj, nsim = 1000, ncores = 1, meta_name = "")`

| Argument    | Description                                           |
|-------------|-------------------------------------------------------|
| `A`         | Character Matrix (data)                               |
| `adj`       | Adjacency list of a decomposable graph                |
| `nsim`      | Number of simulations                                 |
| `ncores`    | Number of cores to use in parallelization             |
| `meta_name` | A meta name to keep track of different outlier models |

The function fits an outlier model from which outlier detection can be conducted

### Model Selection in DGMs

-   `efs(df, x = efs_init(df), trace = TRUE, stop_crit = "mdl1", thres = 5)`:

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
<td>Dataframe</td>
</tr>
<tr class="even">
<td><code>x</code></td>
<td>An efs object</td>
</tr>
<tr class="odd">
<td><code>trace</code></td>
<td>Logical indidcating whether or not to trace the procedure</td>
</tr>
<tr class="even">
<td><code>stop_crit</code></td>
<td>Stopping criterion (mdl1, mdl2, aic or bic)</td>
</tr>
<tr class="odd">
<td><code>thres</code></td>
<td>A threshold mechanism for choosing between two different ways of calculating the entropy</td>
</tr>
</tbody>
</table>

Efficient forward selection in decomposable graphical models. It is for example used to obtain the input graph `adj` in `outlier_model`.

### Simulation

-   `dgm_sim(A, adj, nsim = 1000, ncores = 1)`:

| Argument | Description                               |
|----------|-------------------------------------------|
| `A`      | Character Matrix (data)                   |
| `adj`    | Adjacency list of a decomposable graph    |
| `nsim`   | Number of simulations                     |
| `ncores` | Number of cores to use in parallelization |

This function returns a matrix of dimension `nsim x ncol(A)` where each row correspond to a simulated observation from a DGM represented by `adj`.

### Example

TBA

Installation
============

You can install the development version of the package by using the `devtools` package:

``` r
devtools::install_github("mlindsk/molic")
```

References
==========

Deshpande, A., Garofalakis, M. and Jordan, M. I. (2001) Efficient stepwise selection in decomposable models. In Proceedings of the Seventeenth conference on Uncertainty in artificial intelligence, 128–135. Morgan Kaufmann Publishers Inc. <https://arxiv.org/abs/1301.2267>

Altmueller, S. M. and Haralick, R. M. (2004) Practical aspects of efficient forward selection in decomposable graphical models. In 16th IEEE International Conference on Tools with Artificial Intelligence, 710–715. IEEE. URL: <https://doi.org/10.1109/ictai.2004.100>
