# Discretize numeric variables with XgBoost

`step_discretize_xgb()` creates a *specification* of a recipe step that
will discretize numeric data (e.g. integers or doubles) into bins in a
supervised way using an XgBoost model.

## Usage

``` r
step_discretize_xgb(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  outcome = NULL,
  sample_val = 0.2,
  learn_rate = 0.3,
  num_breaks = 10,
  tree_depth = 1,
  min_n = 5,
  rules = NULL,
  skip = FALSE,
  id = rand_id("discretize_xgb")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose which variables are affected
  by the step. See
  [recipes::selections](https://recipes.tidymodels.org/reference/selections.html)
  for more details.

- role:

  Defaults to `"predictor"`.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- outcome:

  A call to `vars` to specify which variable is used as the outcome to
  train XgBoost models in order to discretize explanatory variables.

- sample_val:

  Share of data used for validation (with early stopping) of the learned
  splits (the rest is used for training). Defaults to 0.20.

- learn_rate:

  The rate at which the boosting algorithm adapts from
  iteration-to-iteration. Corresponds to `eta` in the xgboost package.
  Defaults to 0.3.

- num_breaks:

  The *maximum* number of discrete bins to bucket continuous features.
  Corresponds to `max_bin` in the xgboost package. Defaults to 10.

- tree_depth:

  The maximum depth of the tree (i.e. number of splits). Corresponds to
  `max_depth` in the xgboost package. Defaults to 1.

- min_n:

  The minimum number of instances needed to be in each node. Corresponds
  to `min_child_weight` in the xgboost package. Defaults to 5.

- rules:

  The splitting rules of the best XgBoost tree to retain for each
  variable.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`recipes::bake()`](https://recipes.tidymodels.org/reference/bake.html)?
  While all operations are baked when
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html)
  is run, some operations may not be able to be conducted on new data
  (e.g. processing the outcome variable(s)). Care should be taken when
  using `skip = TRUE` as it may affect the computations for subsequent
  operations

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of any existing operations.

## Details

`step_discretize_xgb()` creates non-uniform bins from numerical
variables by utilizing the information about the outcome variable and
applying the xgboost model. It is advised to impute missing values
before this step. This step is intended to be used particularly with
linear models because thanks to creating non-uniform bins it becomes
easier to learn non-linear patterns from the data.

The best selection of buckets for each variable is selected using an
internal early stopping scheme implemented in the xgboost package, which
makes this discretization method prone to overfitting.

The pre-defined values of the underlying xgboost learns good and
reasonably complex results. However, if one wishes to tune them the
recommended path would be to first start with changing the value of
`num_breaks` to e.g.: 20 or 30. If that doesn't give satisfactory
results one could experiment with modifying the `tree_depth` or `min_n`
parameters. Note that it is not recommended to tune `learn_rate`
simultaneously with other parameters.

This step requires the xgboost package. If not installed, the step will
stop with a note about installing the package.

Note that the original data will be replaced with the new bins.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms`, `value`, and `id`:

- terms:

  character, the selectors or variables selected

- value:

  numeric, location of the splits

- id:

  character, id of this step

## Tuning Parameters

This step has 5 tuning parameters:

- `sample_val`: Proportion of data for validation (type: double,
  default: 0.2)

- `learn_rate`: Learning Rate (type: double, default: 0.3)

- `num_breaks`: Number of Cut Points (type: integer, default: 10)

- `tree_depth`: Tree Depth (type: integer, default: 1)

- `min_n`: Minimal Node Size (type: integer, default: 5)

## Case weights

This step performs an supervised operation that can utilize case
weights. To use them, see the documentation in
[recipes::case_weights](https://recipes.tidymodels.org/reference/case_weights.html)
and the examples on `tidymodels.org`.

## See also

[`recipes::step_discretize()`](https://recipes.tidymodels.org/reference/step_discretize.html),
[`step_discretize_cart()`](https://embed.tidymodels.org/reference/step_discretize_cart.md)

## Examples

``` r
library(rsample)
library(recipes)
data(credit_data, package = "modeldata")

set.seed(1234)
split <- initial_split(credit_data[1:1000, ], strata = "Status")

credit_data_tr <- training(split)
credit_data_te <- testing(split)

xgb_rec <-
  recipe(Status ~ Income + Assets, data = credit_data_tr) |>
  step_impute_median(Income, Assets) |>
  step_discretize_xgb(Income, Assets, outcome = "Status")

xgb_rec <- prep(xgb_rec, training = credit_data_tr)

bake(xgb_rec, credit_data_te, Assets)
#> # A tibble: 251 × 1
#>    Assets     
#>    <fct>      
#>  1 [3000,4000)
#>  2 [3000,4000)
#>  3 [9500, Inf]
#>  4 [3000,4000)
#>  5 [-Inf,2500)
#>  6 [-Inf,2500)
#>  7 [-Inf,2500)
#>  8 [4000,4500)
#>  9 [-Inf,2500)
#> 10 [3000,4000)
#> # ℹ 241 more rows
```
