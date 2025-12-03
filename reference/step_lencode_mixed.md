# Supervised Factor Conversions into Linear Functions using Bayesian Likelihood Encodings

`step_lencode_mixed()` creates a *specification* of a recipe step that
will convert a nominal (i.e. factor) predictor into a single set of
scores derived from a generalized linear mixed model.

## Usage

``` r
step_lencode_mixed(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  outcome = NULL,
  options = list(verbose = 0),
  mapping = NULL,
  skip = FALSE,
  id = rand_id("lencode_mixed")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables. For
  `step_lencode_mixed`, this indicates the variables to be encoded into
  a numeric format. See
  [`recipes::selections()`](https://recipes.tidymodels.org/reference/selections.html)
  for more details. For the `tidy` method, these are not currently used.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- outcome:

  A call to `vars` to specify which variable is used as the outcome in
  the generalized linear model. Only numeric and two-level factors are
  currently supported.

- options:

  A list of options to pass to
  [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) or
  [`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html).

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

For each factor predictor, a generalized linear model is fit to the
outcome and the coefficients are returned as the encoding. These
coefficients are on the linear predictor scale so, for factor outcomes,
they are in log-odds units. The coefficients are created using a no
intercept model and, when two factor outcomes are used, the log-odds
reflect the event of interest being the *first* level of the factor.

For novel levels, a slightly timmed average of the coefficients is
returned.

A hierarchical generalized linear model is fit using
[`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) or
[`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html), depending on
the nature of the outcome, and no intercept via

      lmer(outcome ~ 1 + (1 | predictor), data = data, ...)

where the `...` include the `family` argument (automatically set by the
step) as well as any arguments given to the `options` argument to the
step. Relevant options include `control` and others.

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

[`step_lencode()`](https://embed.tidymodels.org/reference/step_lencode.md),
[`step_lencode_bayes()`](https://embed.tidymodels.org/reference/step_lencode_bayes.md),
[`step_lencode_glm()`](https://embed.tidymodels.org/reference/step_lencode_glm.md)

## Examples

``` r
library(recipes)
library(dplyr)
library(modeldata)

data(grants)

set.seed(1)
grants_other <- sample_n(grants_other, 500)
# \donttest{
reencoded <- recipe(class ~ sponsor_code, data = grants_other) |>
  step_lencode_mixed(sponsor_code, outcome = vars(class))
# }
```
