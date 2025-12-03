# Crosstable with woe between a binary outcome and a predictor variable.

Calculates some summaries and the WoE (Weight of Evidence) between a
binary outcome and a given predictor variable. Used to biuld the
dictionary.

## Usage

``` r
woe_table(predictor, outcome, Laplace = 1e-06, call = rlang::caller_env(0))
```

## Arguments

- predictor:

  A atomic vector, usualy with few distinct values.

- outcome:

  The dependent variable. A atomic vector with exactly 2 distinct
  values.

- Laplace:

  The `pseudocount` parameter of the Laplace Smoothing estimator.
  Default to 1e-6. Value to avoid -Inf/Inf from predictor category with
  only one outcome class. Set to 0 to allow Inf/-Inf.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the call argument of
  [`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html) for
  more information.

## Value

a tibble with counts, proportions and woe. Warning: woe can possibly be
-Inf. Use 'Laplace' arg to avoid that.

## References

Kullback, S. (1959). *Information Theory and Statistics.* Wiley, New
York.

Hastie, T., Tibshirani, R. and Friedman, J. (1986). *Elements of
Statistical Learning*, Second Edition, Springer, 2009.

Good, I. J. (1985), "Weight of evidence: A brief survey", *Bayesian
Statistics*, 2, pp.249-270.
