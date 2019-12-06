# molic 1.0.0.9000

**Development Model**

From this release we adopt the branching model introduced by Vincent Driessen

 * [Git branching model](https://nvie.com/posts/a-successful-git-branching-model/)

This means, that there are now two branches: the **master branch** is always the current stable version, and the **develop branch** is the develop version (indicated with the suffix number 9000 in the version number).

**New API**

 * API for `fit_outlier` has changed slightly (see hidden functions below for an explanation)
 * API for `fit_mixed_outlier` has changed slightly and is improved considerably - much less work for the user now!

**New functions**

 * `generate_multiple_models`
     + Given a class variable with $1,2\ldots, l$ levels and a new observation $y$, this function is a convenient wrapper around `fit_graph` and `fit_outlier` that conducts all the hypothesis $H_k:$ $y$ has level $k$ for $k = 1,2,\ldots, l$.
 * `plot.multiple_models`
     + Given an object returned from `fit_multiple_models` this function is used to visualize all the hypothesis tests for a single observation simultaneously. It is a `ggplot2` object
 * `plot.outlier_model`
     + Given an object returned from `fit_outlier` this function is used to visualize the approximated density of the deviance under the null hypothesis. It is a `ggplot2` object.
 * `components`
     + Return a list with all components of a graph

# molic 1.0.0

 * First release.
