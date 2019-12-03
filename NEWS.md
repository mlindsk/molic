# molic 1.1.0.9000

**Development Model**

From this release we adopt the branching model introduced by Vincent Driessen

 * [Git branching model](https://nvie.com/posts/a-successful-git-branching-model/)

This means, that there are now two branches: the **master branch** is always the current stable version, and the **develop branch** is the develop version (indicated with the suffix number 9000 in the version number).

**New functions**

 * `components`
     + Return a list with all components of a graph

<!--  * `generate_all_models` -->
<!--      + Given a class variable with $1,2\ldots, l$ levels and a new observation $y$, this function is a convenient wrapper around `fit_graph` and `fit_outlier` that conducts all the hypothesis $H_k:$ $y$ has level $k$ for $k = 1,2,\ldots, l$. As default, the Bonferroni correction is used -->
<!--  * `plot_models` -->
<!--      + Given an object returned from `generate_all_models` this function is used to visualize all the hypothesis tests simultaneously -->


<!-- **Extended API:** -->

<!--  * The `outlier_model` and `fit_outlier` functions also accepts `data.frame` objects now. -->

# molic 1.0.0

 * First release.
