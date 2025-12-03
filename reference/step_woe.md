# Weight of evidence transformation

`step_woe()` creates a *specification* of a recipe step that will
transform nominal data into its numerical transformation based on
weights of evidence against a binary outcome.

## Usage

``` r
step_woe(
  recipe,
  ...,
  role = "predictor",
  outcome,
  trained = FALSE,
  dictionary = NULL,
  Laplace = 1e-06,
  prefix = "woe",
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("woe")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose which variables will be used
  to compute the components. See
  [recipes::selections](https://recipes.tidymodels.org/reference/selections.html)
  for more details. For the `tidy` method, these are not currently used.

- role:

  For model terms created by this step, what analysis role should they
  be assigned?. By default, the function assumes that the new woe
  components columns created by the original variables will be used as
  predictors in a model.

- outcome:

  The bare name of the binary outcome encased in
  [`vars()`](https://ggplot2.tidyverse.org/reference/vars.html).

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- dictionary:

  A tbl. A map of levels and woe values. It must have the same layout
  than the output returned from
  [`dictionary()`](https://embed.tidymodels.org/reference/dictionary.md).
  If `NULL` the function will build a dictionary with those variables
  passed to `...`. See
  [`dictionary()`](https://embed.tidymodels.org/reference/dictionary.md)
  for details.

- Laplace:

  The Laplace smoothing parameter. A value usually applied to avoid
  -Inf/Inf from predictor category with only one outcome class. Set to 0
  to allow Inf/-Inf. The default is 1e-6. Also known as 'pseudocount'
  parameter of the Laplace smoothing technique.

- prefix:

  A character string that will be the prefix to the resulting new
  variables. See notes below.

- keep_original_cols:

  A logical to keep the original variables in the output. Defaults to
  `FALSE`.

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
of existing steps (if any). For the `tidy` method, a tibble with the woe
dictionary used to map categories with woe values.

## Details

WoE is a transformation of a group of variables that produces a new set
of features. The formula is

\$\$woe_c = log((P(X = c\|Y = 1))/(P(X = c\|Y = 0)))\$\$

where \\c\\ goes from 1 to \\C\\ levels of a given nominal predictor
variable \\X\\.

These components are designed to transform nominal variables into
numerical ones with the property that the order and magnitude reflects
the association with a binary outcome. To apply it on numerical
predictors, it is advisable to discretize the variables prior to running
WoE. Here, each variable will be binarized to have woe associated later.
This can achieved by using
[`recipes::step_discretize()`](https://recipes.tidymodels.org/reference/step_discretize.html).

The argument `Laplace` is an small quantity added to the proportions of
1's and 0's with the goal to avoid log(p/0) or log(0/p) results. The
numerical woe versions will have names that begin with `woe_` followed
by the respective original name of the variables. See Good (1985).

One can pass a custom `dictionary` tibble to `step_woe()`. It must have
the same structure of the output from
[`dictionary()`](https://embed.tidymodels.org/reference/dictionary.md)
(see examples). If not provided it will be created automatically. The
role of this tibble is to store the map between the levels of nominal
predictor to its woe values. You may want to tweak this object with the
goal to fix the orders between the levels of one given predictor. One
easy way to do this is by tweaking an output returned from
[`dictionary()`](https://embed.tidymodels.org/reference/dictionary.md).

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble with columns `terms` (the selectors or variables
selected), `value`, `n_tot`, `n_bad`, `n_good`, `p_bad`, `p_good`, `woe`
and `outcome` is returned.. See
[`dictionary()`](https://embed.tidymodels.org/reference/dictionary.md)
for more information.

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with columns `terms` `value`, `n_tot`,
`n_bad`, `n_good`, `p_bad`, `p_good`, `woe` and `outcome` and `id`:

- terms:

  character, the selectors or variables selected

- value:

  character, level of the outcome

- n_tot:

  integer, total number

- n_bad:

  integer, number of bad examples

- n_good:

  integer, number of good examples

- p_bad:

  numeric, p of bad examples

- p_good:

  numeric, p of good examples

- woe:

  numeric, weight of evidence

- outcome:

  character, name of outcome variable

- id:

  character, id of this step

## Tuning Parameters

This step has 1 tuning parameters:

- `Laplace`: Laplace Correction (type: double, default: 1e-06)

## Case weights

The underlying operation does not allow for case weights.

## References

Kullback, S. (1959). *Information Theory and Statistics.* Wiley, New
York.

Hastie, T., Tibshirani, R. and Friedman, J. (1986). *Elements of
Statistical Learning*, Second Edition, Springer, 2009.

Good, I. J. (1985), "Weight of evidence: A brief survey", *Bayesian
Statistics*, 2, pp.249-270.

## Examples

``` r
library(modeldata)
data("credit_data")

set.seed(111)
in_training <- sample(1:nrow(credit_data), 2000)

credit_tr <- credit_data[in_training, ]
credit_te <- credit_data[-in_training, ]

rec <- recipe(Status ~ ., data = credit_tr) |>
  step_woe(Job, Home, outcome = vars(Status))

woe_models <- prep(rec, training = credit_tr)
#> Warning: Some columns used by `step_woe()` have categories with fewer than 10
#> values: "Home" and "Job"

# the encoding:
bake(woe_models, new_data = credit_te |> slice(1:5), starts_with("woe"))
#> # A tibble: 5 × 2
#>   woe_Job woe_Home
#>     <dbl>    <dbl>
#> 1  -0.451   0.519 
#> 2   0.187  -0.512 
#> 3  -0.451  -0.512 
#> 4   0.187  -0.512 
#> 5   1.51   -0.0519
# the original data
credit_te |>
  slice(1:5) |>
  dplyr::select(Job, Home)
#>         Job    Home
#> 1     fixed    rent
#> 2 freelance   owner
#> 3     fixed   owner
#> 4 freelance   owner
#> 5   partime parents
# the details:
tidy(woe_models, number = 1)
#> # A tibble: 12 × 10
#>    terms value n_tot n_bad n_good   p_bad  p_good     woe outcome id   
#>    <chr> <chr> <int> <dbl>  <dbl>   <dbl>   <dbl>   <dbl> <chr>   <chr>
#>  1 Job   fixed  1261   273    988 0.451   0.708   -0.451  Status  woe_…
#>  2 Job   free…   463   159    304 0.263   0.218    0.187  Status  woe_…
#>  3 Job   othe…    74    39     35 0.0645  0.0251   0.944  Status  woe_…
#>  4 Job   part…   201   133     68 0.220   0.0487   1.51   Status  woe_…
#>  5 Job   NA        1     1      0 0.00165 0       14.7    Status  woe_…
#>  6 Home  igno…     8     4      4 0.00661 0.00287  0.835  Status  woe_…
#>  7 Home  other   161    78     83 0.129   0.0595   0.773  Status  woe_…
#>  8 Home  owner   931   192    739 0.317   0.530   -0.512  Status  woe_…
#>  9 Home  pare…   336    98    238 0.162   0.171   -0.0519 Status  woe_…
#> 10 Home  priv    113    42     71 0.0694  0.0509   0.310  Status  woe_…
#> 11 Home  rent    446   188    258 0.311   0.185    0.519  Status  woe_…
#> 12 Home  NA        5     3      2 0.00496 0.00143  1.24   Status  woe_…

# Example of custom dictionary + tweaking
# custom dictionary
woe_dict_custom <- credit_tr |> dictionary(Job, Home, outcome = "Status")
woe_dict_custom[4, "woe"] <- 1.23 # tweak

# passing custom dict to step_woe()
rec_custom <- recipe(Status ~ ., data = credit_tr) |>
  step_woe(
    Job, Home,
    outcome = vars(Status), dictionary = woe_dict_custom
  ) |>
  prep()
#> Warning: Some columns used by `step_woe()` have categories with fewer than 10
#> values: "Home" and "Job"

rec_custom_baked <- bake(rec_custom, new_data = credit_te)
rec_custom_baked |>
  dplyr::filter(woe_Job == 1.23) |>
  head()
#> # A tibble: 6 × 14
#>   Seniority  Time   Age Marital Records Expenses Income Assets  Debt
#>       <int> <int> <int> <fct>   <fct>      <int>  <int>  <int> <int>
#> 1         0    48    41 married no            90     80      0     0
#> 2         0    18    21 single  yes           35     50      0     0
#> 3         0    36    23 single  no            45    122   2500     0
#> 4        14    24    51 married no            75    198   1000     0
#> 5         1    60    26 single  no            35    120      0     0
#> 6         1    36    24 married no            76    164      0     0
#> # ℹ 5 more variables: Amount <int>, Price <int>, Status <fct>,
#> #   woe_Job <dbl>, woe_Home <dbl>
```
