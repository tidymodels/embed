# Add WoE in a data frame

A tidyverse friendly way to plug WoE versions of a set of predictor
variables against a given binary outcome.

## Usage

``` r
add_woe(.data, outcome, ..., dictionary = NULL, prefix = "woe")
```

## Arguments

- .data:

  A tbl. The data.frame to plug the new woe version columns.

- outcome:

  The bare name of the outcome variable.

- ...:

  Bare names of predictor variables, passed as you would pass variables
  to
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html).
  This means that you can use all the helpers like `starts_with()` and
  `matches()`.

- dictionary:

  A tbl. If NULL the function will build a dictionary with those
  variables passed to `...`. You can pass a custom dictionary too, see
  [`dictionary()`](https://embed.tidymodels.org/dev/reference/dictionary.md)
  for details.

- prefix:

  A character string that will be the prefix to the resulting new
  variables.

## Value

A tibble with the original columns of .data plus the woe columns wanted.

## Details

You can pass a custom dictionary to `add_woe()`. It must have the
exactly the same structure of the output of
[`dictionary()`](https://embed.tidymodels.org/dev/reference/dictionary.md).
One easy way to do this is to tweak a output returned from it.

## Examples

``` r
mtcars |> add_woe("am", cyl, gear:carb)
#> # A tibble: 32 × 14
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
#>  4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
#>  5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
#>  7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> # ℹ 22 more rows
#> # ℹ 3 more variables: woe_cyl <dbl>, woe_gear <dbl>, woe_carb <dbl>
```
