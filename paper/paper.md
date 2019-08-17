---
title: 'molic: An R package for multivariate outlier detection in contingency tables'
authors:
  - name: Mads Lindskou
    orcid: 0000-0002-1033-697X
    affiliation: "1, 2"
date: "16 August 2019"
tags:
  - R
  - Rcpp
  - outlier detection
  - contingency tables
  - graphical models
  - decomposable graphs
affiliations:
 - name: Department of Mathematical Sciences, Aalborg University, Denmark
   index: 1
 - name: Section of Forensic Genetics, Department of Forensic Medicine, Faculty of Health and Medical Sciences, University of Copenhagen, Denmark
   index: 2
bibliography: paper.bib
output: pdf_document
---

<!-- pandoc --filter pandoc-citeproc --bibliography=paper.bib --variable papersize=a4paper -s paper.md -o paper.pdf & evince paper.pdf -->

# Summary

The **molic** package have been written to facilitate the novel outlier detection method in [@outlier_detection_in_contingency_tables]. The method can be used to detect anomalies in high-dimensional contingency tables. In other words, the method works for data sets in which all variables can only take on a finite set of values (also called _levels_). We also say, that such variables are _discrete variables_ or _categorical variables_. 

The method can be described by the **outlier test** procedure below. Assume we are interested in whether or not a new observation $z$ is an outlier in some data set $D$. First an _interaction graph_ $G$ is fitted to the variables in $D$; a decomposable undirected graph that describes the association structure between variables in $D$. If the assumption that $z$ belongs to $D$ is true, $z$ should be included in $D$. Denote by $D_z$ the new data set including $z$. Finally the outlier model $M$ is constructed using $G$ and $D_z$ from which we can query the p-value, $p$, for the test about $z$ belonging to $D$. If $p$ is less than some chosen threshold (significance level), say $0.05$, $z$ is declared an outlier in $D$.
![](outlier_test_alg.pdf)\
The `efs` algorithm is an implementation of the efficient step-wise selection procedure [@deshpande01_efficient] used for model selection in decomposable graphs. This function can be used to explore dependencies between any kind of discrete variables and make statements about conditional dependencies and independencies. A thorough description of the outlier detection method and how to use the software can be found at 

 - [Getting Started](https://mlindsk.github.io/molic/)
 - [The Outlier Model](https://mlindsk.github.io/molic/articles/outlier_model.html)

# References
