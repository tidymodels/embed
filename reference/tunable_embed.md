# tunable methods for embed

These functions define what parameters *can* be tuned for specific
steps. They also define the recommended objects from the `dials` package
that can be used to generate new parameter values and other
characteristics.

## Usage

``` r
# S3 method for class 'step_discretize_cart'
tunable(x, ...)

# S3 method for class 'step_discretize_xgb'
tunable(x, ...)

# S3 method for class 'step_embed'
tunable(x, ...)

# S3 method for class 'step_pca_sparse'
tunable(x, ...)

# S3 method for class 'step_pca_sparse_bayes'
tunable(x, ...)

# S3 method for class 'step_umap'
tunable(x, ...)

# S3 method for class 'step_woe'
tunable(x, ...)
```

## Arguments

- x:

  A recipe step object

- ...:

  Not used.

## Value

A tibble object.
