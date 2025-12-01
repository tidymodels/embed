# Likelihood encoding using analytical formula

`step_lencode()` creates a *specification* of a recipe step that will
convert a nominal (i.e. factor) predictor into a single set of scores
derived analytically.

## Usage

``` r
step_lencode(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  outcome = NULL,
  smooth = TRUE,
  mapping = NULL,
  skip = FALSE,
  id = rand_id("lencode")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables. For
  `step_lencode()`, this indicates the variables to be encoded into a
  numeric format. See
  [`recipes::selections()`](https://recipes.tidymodels.org/reference/selections.html)
  for more details. For the `tidy` method, these are not currently used.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- outcome:

  A call to `vars` to specify which variable is used as the outcome.
  Only numeric and two-level factors are currently supported.

- smooth:

  A logical, default to `TRUE`, should the estimates of groups with low
  counts be pulled towards the gobal estimate? Defaults to `TRUE`. See
  Details for how this is done. This is also known as partial pooling or
  shrinkage. Only works for numeric outcomes.

- mapping:

  A list of tibble results that define the encoding. This is `NULL`
  until the step is trained by
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html).

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
of existing steps (if any). For the `tidy` method, a tibble with columns
`terms` (the selectors or variables for encoding), `level` (the factor
levels), and `value` (the encodings).

## Details

Each selected nominal predictor will be replaced by a numeric predictor.
Each unique value of the nominal predictor is replaced by a numeric
value. Thse values are calculated differently depending on the type of
the outcome.

For **numeric** outcomes each value is the average value of the outcome
inside each of the levels of the predictor. Unseen levels of the
predictor will be using the global mean of the predictor. If case
weights are used then a weighted mean is calculated instead.

For **nominal** outcomes each value is the log odds of the of the first
level of the outcome variable being present, within each level of the
levels of the predictor. Unseen levels will be replaced by the global
log odds without stratification. If case weights are used then a
weighted log odds is calculated.

If no or all occurances happens then the log odds is calculated using
`p = (2 * nrow(data) - 1) / (2 * nrow(data))` to avoid infinity that
would happen by taking the log of `0`.

For numeric outcomes where `smooth = TRUE`, the following adjustment is
done.

\$\$ estimate = (n / global\_{var}) / (n / global\_{var} + 1 /
outcome\_{var}) \* estimate + (1 / outcome\_{var}) / (n /
global\_{var} + 1 / outcome\_{var}) \* global\_{mean} \$\$

Where \\n\\ is the number of observations in the group.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `level`, `value`, `terms`,
and `id`:

- level:

  character, the factor levels

- value:

  numeric, the encoding

- terms:

  character, the selectors or variables selected

- id:

  character, id of this step

## Case weights

This step performs an supervised operation that can utilize case
weights. To use them, see the documentation in
[recipes::case_weights](https://recipes.tidymodels.org/reference/case_weights.html)
and the examples on `tidymodels.org`.

## References

Micci-Barreca D (2001) "A preprocessing scheme for high-cardinality
categorical attributes in classification and prediction problems," ACM
SIGKDD Explorations Newsletter, 3(1), 27-32.

Zumel N and Mount J (2017) "vtreat: a data.frame Processor for
Predictive Modeling," arXiv:1611.09477

## See also

[`step_lencode_bayes()`](https://embed.tidymodels.org/dev/reference/step_lencode_bayes.md),
[`step_lencode_glm()`](https://embed.tidymodels.org/dev/reference/step_lencode_glm.md),
[`step_lencode_mixed()`](https://embed.tidymodels.org/dev/reference/step_lencode_mixed.md)

## Examples

``` r
library(recipes)
library(dplyr)
library(modeldata)

data(grants)

set.seed(1)
grants_other <- sample_n(grants_other, 500)
reencoded <- recipe(class ~ sponsor_code, data = grants_other) |>
  step_lencode(sponsor_code, outcome = vars(class), smooth = FALSE) |>
  prep()

bake(reencoded, grants_other)
#> # A tibble: 500 × 2
#>    sponsor_code class       
#>           <dbl> <fct>       
#>  1       -1.61  successful  
#>  2        0     unsuccessful
#>  3       -1.61  unsuccessful
#>  4        6.91  unsuccessful
#>  5        6.91  unsuccessful
#>  6       -0.320 successful  
#>  7        1.24  successful  
#>  8        6.91  successful  
#>  9       -0.320 successful  
#> 10        1.24  successful  
#> # ℹ 490 more rows

tidy(reencoded, 1)
#> # A tibble: 80 × 4
#>    level  value terms        id           
#>    <chr>  <dbl> <chr>        <chr>        
#>  1 40D   -1.61  sponsor_code lencode_Y7Kxn
#>  2 266B   0     sponsor_code lencode_Y7Kxn
#>  3 205A   6.91  sponsor_code lencode_Y7Kxn
#>  4 4D    -0.320 sponsor_code lencode_Y7Kxn
#>  5 Unk    1.24  sponsor_code lencode_Y7Kxn
#>  6 204D   6.91  sponsor_code lencode_Y7Kxn
#>  7 2B    -0.492 sponsor_code lencode_Y7Kxn
#>  8 75C    6.91  sponsor_code lencode_Y7Kxn
#>  9 34B    1.20  sponsor_code lencode_Y7Kxn
#> 10 113A   0.693 sponsor_code lencode_Y7Kxn
#> # ℹ 70 more rows
```
