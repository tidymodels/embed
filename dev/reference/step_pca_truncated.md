# Truncated PCA Signal Extraction

`step_pca_truncated()` creates a *specification* of a recipe step that
will convert numeric data into one or more principal components. It is
truncated as it only calculates the number of components it is asked
instead of all of them as is done in
[`recipes::step_pca()`](https://recipes.tidymodels.org/reference/step_pca.html).

## Usage

``` r
step_pca_truncated(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  num_comp = 5,
  options = list(),
  res = NULL,
  columns = NULL,
  prefix = "PC",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("pca_truncated")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables for this step. See
  [`selections()`](https://recipes.tidymodels.org/reference/selections.html)
  for more details.

- role:

  For model terms created by this step, what analysis role should they
  be assigned? By default, the new columns created by this step from the
  original variables will be used as *predictors* in a model.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- num_comp:

  The number of components to retain as new predictors. If `num_comp` is
  greater than the number of columns or the number of possible
  components, a smaller value will be used. If `num_comp = 0` is set
  then no transformation is done and selected variables will stay
  unchanged, regardless of the value of `keep_original_cols`.

- options:

  A list of options to the default method for
  [`irlba::prcomp_irlba()`](https://rdrr.io/pkg/irlba/man/prcomp_irlba.html).
  Argument defaults are set to `retx = FALSE`, `center = FALSE`,
  `scale. = FALSE`, and `tol = NULL`. **Note** that the argument `x`
  should not be passed here (or at all).

- res:

  The
  [`irlba::prcomp_irlba()`](https://rdrr.io/pkg/irlba/man/prcomp_irlba.html)
  object is stored here once this preprocessing step has be trained by
  [recipes::prep](https://recipes.tidymodels.org/reference/prep.html).

- columns:

  A character string of the selected variable names. This field is a
  placeholder and will be populated once
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html) is
  used.

- prefix:

  A character string for the prefix of the resulting new variables. See
  notes below.

- keep_original_cols:

  A logical to keep the original variables in the output. Defaults to
  `FALSE`.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`bake()`](https://recipes.tidymodels.org/reference/bake.html)? While
  all operations are baked when
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html) is run,
  some operations may not be able to be conducted on new data (e.g.
  processing the outcome variable(s)). Care should be taken when using
  `skip = TRUE` as it may affect the computations for subsequent
  operations.

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of any existing operations.

## Details

Principal component analysis (PCA) is a transformation of a group of
variables that produces a new set of artificial features or components.
These components are designed to capture the maximum amount of
information (i.e. variance) in the original variables. Also, the
components are statistically independent from one another. This means
that they can be used to combat large inter-variables correlations in a
data set.

It is advisable to standardize the variables prior to running PCA. Here,
each variable will be centered and scaled prior to the PCA calculation.
This can be changed using the `options` argument or by using
[`recipes::step_center()`](https://recipes.tidymodels.org/reference/step_center.html)
and
[`recipes::step_scale()`](https://recipes.tidymodels.org/reference/step_scale.html).

The argument `num_comp` controls the number of components that will be
retained (the original variables that are used to derive the components
are removed from the data). The new components will have names that
begin with `prefix` and a sequence of numbers. The variable names are
padded with zeros. For example, if `num_comp < 10`, their names will be
`PC1` - `PC9`. If `num_comp = 101`, the names would be `PC1` - `PC101`.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step two things can happen depending the `type` argument. If
`type = "coef"` a tibble returned with 4 columns `terms`, `value`,
`component` , and `id`:

- terms:

  character, the selectors or variables selected

- value:

  numeric, variable loading

- component:

  character, principle component

- id:

  character, id of this step

If `type = "variance"` a tibble returned with 4 columns `terms`,
`value`, `component` , and `id`:

- terms:

  character, type of variance

- value:

  numeric, value of the variance

- component:

  integer, principle component

- id:

  character, id of this step

## Tuning Parameters

This step has 1 tuning parameters:

- `num_comp`: \# Components (type: integer, default: 5)

## Case weights

This step performs an unsupervised operation that can utilize case
weights. As a result, case weights are only used with frequency weights.
For more information, see the documentation in
[recipes::case_weights](https://recipes.tidymodels.org/reference/case_weights.html)
and the examples on `tidymodels.org`.

## References

Jolliffe, I. T. (2010). *Principal Component Analysis*. Springer.

## See also

[`recipes::step_pca()`](https://recipes.tidymodels.org/reference/step_pca.html),
[`recipes::step_kpca()`](https://recipes.tidymodels.org/reference/step_kpca.html),
[`recipes::step_kpca_poly()`](https://recipes.tidymodels.org/reference/step_kpca_poly.html),
[`recipes::step_kpca_rbf()`](https://recipes.tidymodels.org/reference/step_kpca_rbf.html),
[`step_pca_sparse_bayes()`](https://embed.tidymodels.org/dev/reference/step_pca_sparse_bayes.md),
[`step_pca_sparse()`](https://embed.tidymodels.org/dev/reference/step_pca_sparse.md)

## Examples

``` r
rec <- recipe(~., data = mtcars)
pca_trans <- rec |>
  step_normalize(all_numeric()) |>
  step_pca_truncated(all_numeric(), num_comp = 2)
pca_estimates <- prep(pca_trans, training = mtcars)
pca_data <- bake(pca_estimates, mtcars)

rng <- extendrange(c(pca_data$PC1, pca_data$PC2))
plot(pca_data$PC1, pca_data$PC2,
  xlim = rng, ylim = rng
)


tidy(pca_trans, number = 2)
#> # A tibble: 1 × 4
#>   terms         value component id                 
#>   <chr>         <dbl> <chr>     <chr>              
#> 1 all_numeric()    NA NA        pca_truncated_H3Jtu
tidy(pca_estimates, number = 2)
#> # A tibble: 22 × 4
#>    terms  value component id                 
#>    <chr>  <dbl> <chr>     <chr>              
#>  1 mpg    0.363 PC1       pca_truncated_H3Jtu
#>  2 cyl   -0.374 PC1       pca_truncated_H3Jtu
#>  3 disp  -0.368 PC1       pca_truncated_H3Jtu
#>  4 hp    -0.330 PC1       pca_truncated_H3Jtu
#>  5 drat   0.294 PC1       pca_truncated_H3Jtu
#>  6 wt    -0.346 PC1       pca_truncated_H3Jtu
#>  7 qsec   0.200 PC1       pca_truncated_H3Jtu
#>  8 vs     0.307 PC1       pca_truncated_H3Jtu
#>  9 am     0.235 PC1       pca_truncated_H3Jtu
#> 10 gear   0.207 PC1       pca_truncated_H3Jtu
#> # ℹ 12 more rows
```
