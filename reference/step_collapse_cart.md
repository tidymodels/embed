# Supervised Collapsing of Factor Levels

`step_collapse_cart()` creates a *specification* of a recipe step that
can collapse factor levels into a smaller set using a supervised tree.

## Usage

``` r
step_collapse_cart(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  outcome = NULL,
  cost_complexity = 1e-04,
  min_n = 5,
  results = NULL,
  skip = FALSE,
  id = rand_id("step_collapse_cart")
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
  for more details. For the `tidy` method, these are not currently used.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- outcome:

  A call to `vars` to specify which variable is used as the outcome to
  train CART models in order to pool factor levels.

- cost_complexity:

  A non-negative value that regulates the complexity of the tree when
  pruning occurs. Values near 0.1 usually correspond to a tree with a
  single splits. Values of zero correspond to unpruned tree.

- min_n:

  An integer for how many data points are required to make further
  splits during the tree growing process. Larger values correspond to
  less complex trees.

- results:

  A list of results to convert to new factor levels.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [recipes::bake](https://recipes.tidymodels.org/reference/bake.html)?
  While all operations are baked when
  [recipes::prep](https://recipes.tidymodels.org/reference/prep.html) is
  run, some operations may not be able to be conducted on new data (e.g.
  processing the outcome variable(s)). Care should be taken when using
  `skip = TRUE` as it may affect the computations for subsequent
  operations

- id:

  A character string that is unique to this step to identify it.

## Value

An updated recipe step.

## Details

This step uses a CART tree (classification or regression) to group the
existing factor levels into a potentially smaller set. It changes the
levels in the factor predictor (and the
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) method can be
used to understand the translation).

There are a few different ways that the step will not be able to
collapse levels. If the model fails or, if the results have each level
being in its own split, the original factor levels are retained. There
are also cases where there is "no admissible split" which means that the
model could not find any signal in the data.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms`, `old`, `new`, and
`id`:

- terms:

  character, the selectors or variables selected

- old:

  character, the old levels

- new:

  character, the new levels

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## See also

[`step_collapse_stringdist()`](https://embed.tidymodels.org/reference/step_collapse_stringdist.md)

## Examples

``` r
data(ames, package = "modeldata")
ames$Sale_Price <- log10(ames$Sale_Price)

rec <-
  recipe(Sale_Price ~ ., data = ames) |>
  step_collapse_cart(
    Sale_Type, Garage_Type, Neighborhood,
    outcome = vars(Sale_Price)
  ) |>
  prep()
tidy(rec, number = 1)
#> # A tibble: 45 × 4
#>    terms     old     new         id                      
#>    <chr>     <chr>   <chr>       <chr>                   
#>  1 Sale_Type "ConLD" Sale_Type_1 step_collapse_cart_EdLie
#>  2 Sale_Type "ConLw" Sale_Type_1 step_collapse_cart_EdLie
#>  3 Sale_Type "Oth"   Sale_Type_1 step_collapse_cart_EdLie
#>  4 Sale_Type "COD"   Sale_Type_2 step_collapse_cart_EdLie
#>  5 Sale_Type "VWD"   Sale_Type_2 step_collapse_cart_EdLie
#>  6 Sale_Type "ConLI" Sale_Type_3 step_collapse_cart_EdLie
#>  7 Sale_Type "WD "   Sale_Type_4 step_collapse_cart_EdLie
#>  8 Sale_Type "CWD"   Sale_Type_5 step_collapse_cart_EdLie
#>  9 Sale_Type "Con"   Sale_Type_6 step_collapse_cart_EdLie
#> 10 Sale_Type "New"   Sale_Type_7 step_collapse_cart_EdLie
#> # ℹ 35 more rows
```
