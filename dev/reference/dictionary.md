# Weight of evidence dictionary

Builds the woe dictionary of a set of predictor variables upon a given
binary outcome. Convenient to make a woe version of the given set of
predictor variables and also to allow one to tweak some woe values by
hand.

## Usage

``` r
dictionary(.data, outcome, ..., Laplace = 1e-06)
```

## Arguments

- .data:

  A tbl. The data.frame where the variables come from.

- outcome:

  The bare name of the outcome variable with exactly 2 distinct values.

- ...:

  bare names of predictor variables or selectors accepted by
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html).

- Laplace:

  Default to 1e-6. The `pseudocount` parameter of the Laplace Smoothing
  estimator. Value to avoid -Inf/Inf from predictor category with only
  one outcome class. Set to 0 to allow Inf/-Inf.

## Value

a tibble with summaries and woe for every given predictor variable
stacked up.

## Details

You can pass a custom dictionary to
[`step_woe()`](https://embed.tidymodels.org/dev/reference/step_woe.md).
It must have the exactly the same structure of the output of
`dictionary()`. One easy way to do this is by tweaking an output
returned from it.

## References

Kullback, S. (1959). *Information Theory and Statistics.* Wiley, New
York.

Hastie, T., Tibshirani, R. and Friedman, J. (1986). *Elements of
Statistical Learning*, Second Edition, Springer, 2009.

Good, I. J. (1985), "Weight of evidence: A brief survey", *Bayesian
Statistics*, 2, pp.249-270.

## Examples

``` r
mtcars |> dictionary("am", cyl, gear:carb)
#> # A tibble: 12 × 9
#>    variable predictor n_tot   n_0   n_1   p_0    p_1      woe outcome
#>    <chr>    <chr>     <int> <dbl> <dbl> <dbl>  <dbl>    <dbl> <chr>  
#>  1 cyl      4            11     3     8 0.158 0.615    1.36   am     
#>  2 cyl      6             7     4     3 0.211 0.231    0.0918 am     
#>  3 cyl      8            14    12     2 0.632 0.154   -1.41   am     
#>  4 gear     3            15    15     0 0.789 0      -16.1    am     
#>  5 gear     4            12     4     8 0.211 0.615    1.07   am     
#>  6 gear     5             5     0     5 0     0.385   15.8    am     
#>  7 carb     1             7     3     4 0.158 0.308    0.667  am     
#>  8 carb     2            10     6     4 0.316 0.308   -0.0260 am     
#>  9 carb     3             3     3     0 0.158 0      -14.5    am     
#> 10 carb     4            10     7     3 0.368 0.231   -0.468  am     
#> 11 carb     6             1     0     1 0     0.0769  14.2    am     
#> 12 carb     8             1     0     1 0     0.0769  14.2    am     
```
