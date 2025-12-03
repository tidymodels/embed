# Discretize numeric variables with CART

`step_discretize_cart()` creates a *specification* of a recipe step that
will discretize numeric data (e.g. integers or doubles) into bins in a
supervised way using a CART model.

## Usage

``` r
step_discretize_cart(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  outcome = NULL,
  cost_complexity = 0.01,
  tree_depth = 10,
  min_n = 20,
  rules = NULL,
  skip = FALSE,
  id = rand_id("discretize_cart")
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
  train CART models in order to discretize explanatory variables.

- cost_complexity:

  The regularization parameter. Any split that does not decrease the
  overall lack of fit by a factor of `cost_complexity` is not attempted.
  Corresponds to `cp` in
  [`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html). Defaults
  to 0.01.

- tree_depth:

  The *maximum* depth in the final tree. Corresponds to `maxdepth` in
  [`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html). Defaults
  to 10.

- min_n:

  The number of data points in a node required to continue splitting.
  Corresponds to `minsplit` in
  [`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html). Defaults
  to 20.

- rules:

  The splitting rules of the best CART tree to retain for each variable.
  If length zero, splitting could not be used on that column.

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

`step_discretize_cart()` creates non-uniform bins from numerical
variables by utilizing the information about the outcome variable and
applying a CART model.

The best selection of buckets for each variable is selected using the
standard cost-complexity pruning of CART, which makes this
discretization method resistant to overfitting.

This step requires the rpart package. If not installed, the step will
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

This step has 3 tuning parameters:

- `cost_complexity`: Cost-Complexity Parameter (type: double, default:
  0.01)

- `tree_depth`: Tree Depth (type: integer, default: 10)

- `min_n`: Minimal Node Size (type: integer, default: 20)

## Case weights

This step performs an supervised operation that can utilize case
weights. To use them, see the documentation in
[recipes::case_weights](https://recipes.tidymodels.org/reference/case_weights.html)
and the examples on `tidymodels.org`.

## See also

[`recipes::step_discretize()`](https://recipes.tidymodels.org/reference/step_discretize.html),
[`step_discretize_xgb()`](https://embed.tidymodels.org/reference/step_discretize_xgb.md)

## Examples

``` r
library(modeldata)
#> 
#> Attaching package: ‘modeldata’
#> The following object is masked from ‘package:datasets’:
#> 
#>     penguins
data(ad_data)
library(rsample)

split <- initial_split(ad_data, strata = "Class")

ad_data_tr <- training(split)
ad_data_te <- testing(split)

cart_rec <-
  recipe(Class ~ ., data = ad_data_tr) |>
  step_discretize_cart(
    tau, age, p_tau, Ab_42,
    outcome = "Class", id = "cart splits"
  )

cart_rec <- prep(cart_rec, training = ad_data_tr)

# The splits:
tidy(cart_rec, id = "cart splits")
#> # A tibble: 24 × 3
#>    terms value id         
#>    <chr> <dbl> <chr>      
#>  1 tau   5.89  cart splits
#>  2 tau   6.00  cart splits
#>  3 tau   6.17  cart splits
#>  4 tau   6.25  cart splits
#>  5 tau   6.31  cart splits
#>  6 tau   6.36  cart splits
#>  7 tau   6.66  cart splits
#>  8 age   0.986 cart splits
#>  9 age   0.987 cart splits
#> 10 age   0.988 cart splits
#> # ℹ 14 more rows

bake(cart_rec, ad_data_te, tau)
#> # A tibble: 84 × 1
#>    tau          
#>    <fct>        
#>  1 [-Inf,5.886) 
#>  2 [5.995,6.175)
#>  3 [5.995,6.175)
#>  4 [5.995,6.175)
#>  5 [-Inf,5.886) 
#>  6 [-Inf,5.886) 
#>  7 [6.363,6.664)
#>  8 [6.175,6.249)
#>  9 [-Inf,5.886) 
#> 10 [6.308,6.363)
#> # ℹ 74 more rows
```
