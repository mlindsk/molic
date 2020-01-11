# molic 1.2.0.9999

* `subgraph` function is now provided. 

# molic 1.2.0

 * `pmf` no longer plots the density of the deviances of a `outlier_model` object. Use `plot` for this instead; this is now consistent with the other related functions like `fit_outlier`. Instead `pmf` is used to construct the probability mass function of a decomposable graphical model which can be used to obtain probabilities of observing specific cells/observations/configurations.

# molic 1.1.0

**Development Model**

From this release we adopt the branching model introduced by Vincent Driessen

 * [Git branching model](https://nvie.com/posts/a-successful-git-branching-model/)

This means, that there are now two branches: the **master branch** is always the current stable version, and the **develop branch** is the develop version.

**New API**

 * Functions like `fit_outlier` that depends on an adjacency list no accept `gengraph` objects returned from `fit_graph` - i.e. no need to use `adj_lst()` first.

**New functions**

 * `generate_multiple_models`
     + Given a class variable with $1,2\ldots, l$ levels and a new observation $y$, this function is a convenient wrapper around `fit_graph` and `fit_outlier` that conducts all the hypothesis $H_k:$ $y$ has level $k$ for $k = 1,2,\ldots, l$.
 * `plot.multiple_models`
     + Given an object returned from `fit_multiple_models` this function is used to visualize all the hypothesis tests for a single observation simultaneously. It is a `ggplot2` object
 * `plot.outlier`
     + Given an object returned from `fit_outlier` this function is used to visualize the approximated density of the deviance under the null hypothesis. It is a `ggplot2` object.
 * `components`
     + Return a list with all components of a graph
 * `fit_components` 

**Misc**
 * All deviances are now non-negative as they should be! Before, a constant was neglected which could potentially confuse the users since a deviance is per definition non-negative.
 
# molic 1.0.0

 * First release.
