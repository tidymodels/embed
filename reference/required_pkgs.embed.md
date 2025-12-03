# S3 methods for tracking which additional packages are needed for steps.

Recipe-adjacent packages always list themselves as a required package so
that the steps can function properly within parallel processing schemes.

## Usage

``` r
# S3 method for class 'step_collapse_cart'
required_pkgs(x, ...)

# S3 method for class 'step_collapse_stringdist'
required_pkgs(x, ...)

# S3 method for class 'step_discretize_cart'
required_pkgs(x, ...)

# S3 method for class 'step_discretize_xgb'
required_pkgs(x, ...)

# S3 method for class 'step_embed'
required_pkgs(x, ...)

# S3 method for class 'step_lencode'
required_pkgs(x, ...)

# S3 method for class 'step_lencode_bayes'
required_pkgs(x, ...)

# S3 method for class 'step_lencode_glm'
required_pkgs(x, ...)

# S3 method for class 'step_lencode_mixed'
required_pkgs(x, ...)

# S3 method for class 'step_pca_sparse'
required_pkgs(x, ...)

# S3 method for class 'step_pca_sparse_bayes'
required_pkgs(x, ...)

# S3 method for class 'step_pca_truncated'
required_pkgs(x, ...)

# S3 method for class 'step_umap'
required_pkgs(x, ...)

# S3 method for class 'step_woe'
required_pkgs(x, ...)
```

## Arguments

- x:

  A recipe step

## Value

A character vector
