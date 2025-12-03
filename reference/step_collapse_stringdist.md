# collapse factor levels using stringdist

`step_collapse_stringdist()` creates a *specification* of a recipe step
that will collapse factor levels that have a low stringdist between
them.

## Usage

``` r
step_collapse_stringdist(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  distance = NULL,
  method = "osa",
  options = list(),
  results = NULL,
  columns = NULL,
  skip = FALSE,
  id = rand_id("collapse_stringdist")
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

- distance:

  Integer, value to determine which strings should be collapsed with
  which. The value is being used inclusive, so `2` will collapse levels
  that have a string distance between them of 2 or lower.

- method:

  Character, method for distance calculation. The default is `"osa"`,
  see
  [stringdist::stringdist-metrics](https://rdrr.io/pkg/stringdist/man/stringdist-metrics.html).

- options:

  List, other arguments passed to
  [`stringdist::stringdistmatrix()`](https://rdrr.io/pkg/stringdist/man/stringdist.html)
  such as `weight`, `q`, `p`, and `bt`, that are used for different
  values of `method`.

- results:

  A list denoting the way the labels should be collapses is stored here
  once this preprocessing step has be trained by
  [recipes::prep](https://recipes.tidymodels.org/reference/prep.html).

- columns:

  A character string of variable names that will be populated
  (eventually) by the `terms` argument.

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
of existing steps (if any). For the `tidy` method, a tibble with columns
`terms` (the columns that will be affected) and `base`.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms`, `from`, `to`, and
`id`:

- terms:

  character, the selectors or variables selected

- from:

  character, the old levels

- too:

  character, the new levels

- id:

  character, id of this step

## Case weights

The underlying operation does not allow for case weights.

## See also

[`step_collapse_cart()`](https://embed.tidymodels.org/reference/step_collapse_cart.md)

## Examples

``` r
library(recipes)
library(tibble)
data0 <- tibble(
  x1 = c("a", "b", "d", "e", "sfgsfgsd", "hjhgfgjgr"),
  x2 = c("ak", "b", "djj", "e", "hjhgfgjgr", "hjhgfgjgr")
)

rec <- recipe(~., data = data0) |>
  step_collapse_stringdist(all_predictors(), distance = 1) |>
  prep()

rec |>
  bake(new_data = NULL)
#> # A tibble: 6 × 2
#>   x1        x2       
#>   <fct>     <fct>    
#> 1 a         ak       
#> 2 a         b        
#> 3 a         djj      
#> 4 a         b        
#> 5 sfgsfgsd  hjhgfgjgr
#> 6 hjhgfgjgr hjhgfgjgr

tidy(rec, 1)
#> # A tibble: 11 × 4
#>    terms from      to        id                       
#>    <chr> <chr>     <chr>     <chr>                    
#>  1 x1    a         a         collapse_stringdist_PVfLS
#>  2 x1    b         a         collapse_stringdist_PVfLS
#>  3 x1    d         a         collapse_stringdist_PVfLS
#>  4 x1    e         a         collapse_stringdist_PVfLS
#>  5 x1    hjhgfgjgr hjhgfgjgr collapse_stringdist_PVfLS
#>  6 x1    sfgsfgsd  sfgsfgsd  collapse_stringdist_PVfLS
#>  7 x2    ak        ak        collapse_stringdist_PVfLS
#>  8 x2    b         b         collapse_stringdist_PVfLS
#>  9 x2    e         b         collapse_stringdist_PVfLS
#> 10 x2    djj       djj       collapse_stringdist_PVfLS
#> 11 x2    hjhgfgjgr hjhgfgjgr collapse_stringdist_PVfLS

rec <- recipe(~., data = data0) |>
  step_collapse_stringdist(all_predictors(), distance = 2) |>
  prep()

rec |>
  bake(new_data = NULL)
#> # A tibble: 6 × 2
#>   x1        x2       
#>   <fct>     <fct>    
#> 1 a         ak       
#> 2 a         ak       
#> 3 a         djj      
#> 4 a         ak       
#> 5 sfgsfgsd  hjhgfgjgr
#> 6 hjhgfgjgr hjhgfgjgr

tidy(rec, 1)
#> # A tibble: 11 × 4
#>    terms from      to        id                       
#>    <chr> <chr>     <chr>     <chr>                    
#>  1 x1    a         a         collapse_stringdist_SY1gQ
#>  2 x1    b         a         collapse_stringdist_SY1gQ
#>  3 x1    d         a         collapse_stringdist_SY1gQ
#>  4 x1    e         a         collapse_stringdist_SY1gQ
#>  5 x1    hjhgfgjgr hjhgfgjgr collapse_stringdist_SY1gQ
#>  6 x1    sfgsfgsd  sfgsfgsd  collapse_stringdist_SY1gQ
#>  7 x2    ak        ak        collapse_stringdist_SY1gQ
#>  8 x2    b         ak        collapse_stringdist_SY1gQ
#>  9 x2    e         ak        collapse_stringdist_SY1gQ
#> 10 x2    djj       djj       collapse_stringdist_SY1gQ
#> 11 x2    hjhgfgjgr hjhgfgjgr collapse_stringdist_SY1gQ
```
