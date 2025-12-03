# Encoding Factors into Multiple Columns

`step_embed()` creates a *specification* of a recipe step that will
convert a nominal (i.e. factor) predictor into a set of scores derived
from a tensorflow model via a word-embedding model. `embed_control` is a
simple wrapper for setting default options.

## Usage

``` r
step_embed(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  outcome = NULL,
  predictors = NULL,
  num_terms = 2,
  hidden_units = 0,
  options = embed_control(),
  mapping = NULL,
  history = NULL,
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("embed")
)

embed_control(
  loss = "mse",
  metrics = NULL,
  optimizer = "sgd",
  epochs = 20,
  validation_split = 0,
  batch_size = 32,
  verbose = 0,
  callbacks = NULL
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables. For `step_embed`,
  this indicates the variables to be encoded into a numeric format. See
  [`recipes::selections()`](https://recipes.tidymodels.org/reference/selections.html)
  for more details. For the `tidy` method, these are not currently used.

- role:

  For model terms created by this step, what analysis role should they
  be assigned?. By default, the function assumes that the embedding
  variables created will be used as predictors in a model.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- outcome:

  A call to `vars` to specify which variable is used as the outcome in
  the neural network.

- predictors:

  An optional call to `vars` to specify any variables to be added as
  additional predictors in the neural network. These variables should be
  numeric and perhaps centered and scaled.

- num_terms:

  An integer for the number of resulting variables.

- hidden_units:

  An integer for the number of hidden units in a dense ReLu layer
  between the embedding and output later. Use a value of zero for no
  intermediate layer (see Details below).

- options:

  A list of options for the model fitting process.

- mapping:

  A list of tibble results that define the encoding. This is `NULL`
  until the step is trained by
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html).

- history:

  A tibble with the convergence statistics for each term. This is `NULL`
  until the step is trained by
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html).

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
  operations.

- id:

  A character string that is unique to this step to identify it.

- optimizer, loss, metrics:

  Arguments to pass to keras3::compile()

- epochs, validation_split, batch_size, verbose, callbacks:

  Arguments to pass to keras3::fit()

## Value

An updated version of `recipe` with the new step added to the sequence
of existing steps (if any). For the `tidy` method, a tibble with columns
`terms` (the selectors or variables for encoding), `level` (the factor
levels), and several columns containing `embed` in the name.

## Details

Factor levels are initially assigned at random to the new variables and
these variables are used in a neural network to optimize both the
allocation of levels to new columns as well as estimating a model to
predict the outcome. See Section 6.1.2 of Francois and Allaire (2018)
for more details.

The new variables are mapped to the specific levels seen at the time of
model training and an extra instance of the variables are used for new
levels of the factor.

One model is created for each call to `step_embed`. All terms given to
the step are estimated and encoded in the same model which would also
contain predictors give in `predictors` (if any).

When the outcome is numeric, a linear activation function is used in the
last layer while softmax is used for factor outcomes (with any number of
levels).

For example, the `keras3` code for a numeric outcome, one categorical
predictor, and no hidden units used here would be

      keras_model_sequential() |>
      layer_embedding(
        input_dim = num_factor_levels_x + 1,
        output_dim = num_terms
      ) |>
      layer_flatten() |>
      layer_dense(units = 1, activation = 'linear')

If a factor outcome is used and hidden units were requested, the code
would be

      keras_model_sequential() |>
      layer_embedding(
        input_dim = num_factor_levels_x + 1,
        output_dim = num_terms
      ) |>
      layer_flatten() |>
      layer_dense(units = hidden_units, activation = "relu") |>
      layer_dense(units = num_factor_levels_y, activation = 'softmax')

Other variables specified by `predictors` are added as an additional
dense layer after `layer_flatten` and before the hidden layer.

Also note that it may be difficult to obtain reproducible results using
this step due to the nature of Tensorflow (see link in References).

tensorflow models cannot be run in parallel within the same session (via
`foreach` or `futures`) or the `parallel` package. If using a recipes
with this step with `caret`, avoid parallel processing.

## Tidying

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this step, a tibble is returned with a number of columns with embedding
information, and columns `terms`, `levels`, and `id`:

- terms:

  character, the selectors or variables selected

- levels:

  character, levels in variable

- id:

  character, id of this step

## Tuning Parameters

This step has 2 tuning parameters:

- `num_terms`: \# Model Terms (type: integer, default: 2)

- `hidden_units`: \# Hidden Units (type: integer, default: 0)

## Case weights

The underlying operation does not allow for case weights.

## References

Francois C and Allaire JJ (2018) *Deep Learning with R*, Manning

"Concatenate Embeddings for Categorical Variables with keras3"
<https://flovv.github.io/Embeddings_with_keras_part2/>

## Examples

``` r
data(grants, package = "modeldata")

set.seed(1)
grants_other <- sample_n(grants_other, 500)

rec <- recipe(class ~ num_ci + sponsor_code, data = grants_other) |>
  step_embed(sponsor_code,
    outcome = vars(class),
    options = embed_control(epochs = 10)
  )
```
