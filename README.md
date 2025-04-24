
# embed <a href='https://embed.tidymodels.org'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/tidymodels/embed/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidymodels/embed/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/tidymodels/embed/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tidymodels/embed?branch=main)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/embed)](https://CRAN.r-project.org/package=embed)
[![Downloads](https://CRANlogs.r-pkg.org/badges/embed)](https://CRAN.r-project.org/package=embed)
![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)
<!-- badges: end -->

## Introduction

`embed` has extra steps for the
[`recipes`](https://recipes.tidymodels.org/) package for embedding
predictors into one or more numeric columns. Almost all of the
preprocessing methods are *supervised*.

These steps are available here in a separate package because the step
dependencies, [`rstanarm`](https://CRAN.r-project.org/package=rstanarm),
[`lme4`](https://CRAN.r-project.org/package=lme4), and
[`keras3`](https://CRAN.r-project.org/package=keras3), are fairly heavy.

Some steps handle categorical predictors:

- `step_lencode_glm()`, `step_lencode_bayes()`, and
  `step_lencode_mixed()` estimate the effect of each of the factor
  levels on the outcome and these estimates are used as the new
  encoding. The estimates are estimated by a generalized linear model.
  This step can be executed without pooling (via `glm`) or with partial
  pooling (`stan_glm` or `lmer`). Currently implemented for numeric and
  two-class outcomes.

- `step_embed()` uses `keras3::layer_embedding` to translate the
  original *C* factor levels into a set of *D* new variables (\< *C*).
  The model fitting routine optimizes which factor levels are mapped to
  each of the new variables as well as the corresponding regression
  coefficients (i.e., neural network weights) that will be used as the
  new encodings.

- `step_woe()` creates new variables based on weight of evidence
  encodings.

- `step_feature_hash()` can create indicator variables using feature
  hashing.

For numeric predictors:

- `step_umap()` uses a nonlinear transformation similar to t-SNE but can
  be used to project the transformation on new data. Both supervised and
  unsupervised methods can be used.

- `step_discretize_xgb()` and `step_discretize_cart()` can make binned
  versions of numeric predictors using supervised tree-based models.

- `step_pca_sparse()` and `step_pca_sparse_bayes()` conduct feature
  extraction with sparsity of the component loadings.

Some references for these methods are:

- Francois C and Allaire JJ (2018) [*Deep Learning with
  R*](https://www.manning.com/books/deep-learning-with-r), Manning
- Guo, C and Berkhahn F (2016) “[Entity Embeddings of Categorical
  Variables](https://arxiv.org/abs/1604.06737)”
- Micci-Barreca D (2001) “[A preprocessing scheme for high-cardinality
  categorical attributes in classification and prediction
  problems](https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=A+preprocessing+scheme+for+high-cardinality+categorical+attributes+in+classification+and+prediction+problems&btnG=),”
  ACM SIGKDD Explorations Newsletter, 3(1), 27-32.
- Zumel N and Mount J (2017) “[`vtreat`: a `data.frame` Processor for
  Predictive Modeling](https://arxiv.org/abs/1611.09477)”
- McInnes L and Healy J (2018) [UMAP: Uniform Manifold Approximation and
  Projection for Dimension Reduction](https://arxiv.org/abs/1802.03426)
- Good, I. J. (1985), “[Weight of evidence: A brief
  survey](https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=Weight+of+evidence%3A+A+brief+survey&btnG=)”,
  Bayesian Statistics, 2, pp.249-270.

## Getting Started

There are two articles that walk through how to use these embedding
steps, using [generalized linear
models](https://embed.tidymodels.org/articles/Applications/GLM.html) and
[neural networks built via
TensorFlow](https://embed.tidymodels.org/articles/Applications/Tensorflow.html).

## Installation

To install the package:

``` r
install.packages("embed")
```

Note that to use some steps, you will also have to install other
packages such as `rstanarm` and `lme4`. For all of the steps to work,
you may want to use:

``` r
install.packages(c("rpart", "xgboost", "rstanarm", "lme4"))
```

To get a bug fix or to use a feature from the development version, you
can install the development version of this package from GitHub.

``` r
# install.packages("pak")
pak::pak("tidymodels/embed")
```

## Contributing

This project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

- For questions and discussions about tidymodels packages, modeling, and
  machine learning, please [post on RStudio
  Community](https://forum.posit.co/new-topic?category_id=15&tags=tidymodels,question).

- If you think you have encountered a bug, please [submit an
  issue](https://github.com/tidymodels/embed/issues).

- Either way, learn how to create and share a
  [reprex](https://reprex.tidyverse.org/articles/articles/learn-reprex.html)
  (a minimal, reproducible example), to clearly communicate about your
  code.

- Check out further details on [contributing guidelines for tidymodels
  packages](https://www.tidymodels.org/contribute/) and [how to get
  help](https://www.tidymodels.org/help/).
