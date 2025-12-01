# Dummy Variables Creation via Feature Hashing

**\[deprecated\]**

Please use `textrecipes::step_dummy_hash()` instead.

## Usage

``` r
step_feature_hash(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  num_hash = 2^6,
  preserve = deprecated(),
  columns = NULL,
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("feature_hash")
)
```
