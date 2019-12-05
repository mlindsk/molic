# molic 1.0.0.9000

**Development Model**

From this release we adopt the branching model introduced by Vincent Driessen

 * [Git branching model](https://nvie.com/posts/a-successful-git-branching-model/)

This means, that there are now two branches: the **master branch** is always the current stable version, and the **develop branch** is the develop version (indicated with the suffix number 9000 in the version number).

**New functions**

 * `generate_multiple_models`
     + Given a class variable with $1,2\ldots, l$ levels and a new observation $y$, this function is a convenient wrapper around `fit_graph` and `fit_outlier` that conducts all the hypothesis $H_k:$ $y$ has level $k$ for $k = 1,2,\ldots, l$.
 * `plot_models`
     + Given an object returned from `generate_multiple_models` this function is used to visualize all the hypothesis tests for a single observation simultaneously
 * `components`
     + Return a list with all components of a graph

**Deprecated functions**

 * `outlier_models`
     + the function can still be accessed by `:::`. But the user should prefer `fit_outlier`
     + the problem was, that in `outlier_model` the user had to append the new observation to be tested in data on their own. 
	 + the function `fit_outlier` uses both `fit_graph` and `outlier_model` but in both situations the observation needs to be appended to data; this is taken care of automatically in `fit_outlier`.

# molic 1.0.0

 * First release.
