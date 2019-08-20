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

Outlier detection is an important task in statistical analyses. An outlier is a case-specific unit since it may be interpreted as natural extreme noise in some applications, whereas in other applications it may be the most interesting observation. The **molic** package have been written to facilitate the novel outlier detection method in high-dimensional contingency tables [@outlier_detection_in_contingency_tables]. In other words, the method works for data sets in, which all variables are _categorical_, implying that they can only take on a finite set of values (also called _levels_).

The software uses decomposable graphical models (DGMs), where the probability mass function can be associated with an interaction graph, from which conditional independences among the variables can be inferred. This gives a way to investigate the underlying nature of outliers. This is also called _understandability_ in the litterature. Outlier detection has many applications including areas such as

 - Fraud detection
 - Medical and public health
 - Anomaly detection in text data
 - Fault detection (on critical systems) 

# The Method

The method can be described by the **outlier test** procedure below. Assume we are interested in whether or not a new observation $z$ is an outlier in some data set $D$. First an _interaction graph_ $G$ is fitted to the variables in $D$; a decomposable undirected graph that describes the association structure between variables in $D$. If the assumption that $z$ belongs to $D$ is true, $z$ should be included in $D$. Denote by $D_z$ the new data set including $z$. Finally the outlier model $M$ is constructed using $G$ and $D_z$ from which we can query the p-value, $p$, for the test about $z$ belonging to $D$. If $p$ is less than some chosen threshold (significance level), say $0.05$, $z$ is declared an outlier in $D$.

\vspace{0.2cm}

![](outlier_test_alg.pdf)\

The `efs` algorithm is an implementation of the efficient step-wise selection procedure [@deshpande01_efficient] used for model selection in decomposable graphs. This function can be used to explore dependencies between any kind of discrete variables and make statements about conditional dependencies and independencies. A thorough description of the outlier detection method and how to use the software can be found at 

\begin{center}
https://mlindsk.github.io/molic/
\end{center}

# A Use Case in Genetics

Recently, advances in DNA sequencing has made it possible to sequence short segments of DNA ($< 200$ basepairs) including two or more SNPs. These are called \textsl{microhaplotypes} (or microhaps for short) [@kidd2014microhaplo]. They have been demonstrated to be well suited for ancestry assessment. The short distance between SNPs within a microhap implies that recombination among them rarely occurs. Hence, the methodology of @tvedebrink2018aims can not be used as this assumes mutually independence of the SNPs within a population.

In @outlier_detection_in_contingency_tables the **molic** package was used to detect outliers in microhap data from the 1000 Genomes Project [@10002015global]. This data contains DNA profiles from five different continental regions (CRs); Europe (EUR), America (AMR), East Asia (EAS), South Asia (SAS) and Africa (AFR). 

Consider for example the region SAS as the hypothesized region and all profiles in AFR as profiles to be tested against the hypothesis that their origin is SAS. Two different interaction graphs are used; $G$ which is the result of using the `efs` algorithm and $G^{\emptyset}$ where all microhap SNPs are assumed to be independent (a graph with no edges). The proportion of profiles from AFR that are outliers in SAS according to the model, is $1$ for $G$ and only $0.834$ for $G^{\emptyset}$, see Table 1. The outlier test was conducted for all pairs of continental regions. It is seen, that $G$ outperforms $G^{\emptyset}$ in general and the dependency between microhap SNPs cannot be neglected. All tests was conducted on a significance level of $0.05$.

![](performance_matrix.pdf)\


# References
