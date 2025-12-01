# Changelog

## embed (development version)

## embed 1.2.0

CRAN release: 2025-09-08

### Improvements

- Adds
  [`step_lencode()`](https://embed.tidymodels.org/dev/reference/step_lencode.md)
  to perform analytical likelihood encoding.
  ([\#258](https://github.com/tidymodels/embed/issues/258))

- Adds `smooth` argument to
  [`step_lencode()`](https://embed.tidymodels.org/dev/reference/step_lencode.md)
  to allow for partial pooling in numeric outcomes.
  ([\#261](https://github.com/tidymodels/embed/issues/261))

- [`step_feature_hash()`](https://embed.tidymodels.org/dev/reference/step_feature_hash.md)
  has been fully deprecated in favor of
  `textrecipes::step_dummy_hash()`.
  ([\#253](https://github.com/tidymodels/embed/issues/253))

## embed 1.1.5

CRAN release: 2025-01-22

### Improvements

- [`step_umap()`](https://embed.tidymodels.org/dev/reference/step_umap.md)
  has tunable `initial` and `target_weight` arguments.
  ([\#223](https://github.com/tidymodels/embed/issues/223),
  [\#222](https://github.com/tidymodels/embed/issues/222))

- All messages, warnings and errors has been translated to use {cli}
  package ([\#153](https://github.com/tidymodels/embed/issues/153),
  [\#155](https://github.com/tidymodels/embed/issues/155)).

## embed 1.1.4

CRAN release: 2024-03-20

### Improvements

- [`step_umap()`](https://embed.tidymodels.org/dev/reference/step_umap.md)
  has gained `initial` and `target_weight` arguments.
  ([\#213](https://github.com/tidymodels/embed/issues/213))

- Calling `?tidy.step_*()` now sends you to the documentation for
  `step_*()` where the outcome is documented.
  ([\#216](https://github.com/tidymodels/embed/issues/216))

- Documentation for tidy methods for all steps has been improved to
  describe the return value more accurately.
  ([\#217](https://github.com/tidymodels/embed/issues/217))

- {keras} and {tensorflow} have been moved to Suggests instead of
  Imports. ([\#218](https://github.com/tidymodels/embed/issues/218))

## embed 1.1.3

CRAN release: 2023-10-28

- [`step_collapse_stringdist()`](https://embed.tidymodels.org/dev/reference/step_collapse_stringdist.md)
  will now return predictors as factors.
  ([\#204](https://github.com/tidymodels/embed/issues/204))

- Fixed regression from 1.1.2 in
  [`step_lencode_glm()`](https://embed.tidymodels.org/dev/reference/step_lencode_glm.md)
  where it couldn’t be used on multiple columns.

## embed 1.1.2

CRAN release: 2023-08-17

### Improvements

- The `keep_original_cols` argument has been added to
  [`step_woe()`](https://embed.tidymodels.org/dev/reference/step_woe.md).
  This change should mean that every step that produces new columns has
  the `keep_original_cols` argument.
  ([\#194](https://github.com/tidymodels/embed/issues/194))

- Many internal changes to improve consistency and slight speed
  increases.

### Breaking Changes

- [`step_pca_sparse()`](https://embed.tidymodels.org/dev/reference/step_pca_sparse.md),
  [`step_pca_truncated()`](https://embed.tidymodels.org/dev/reference/step_pca_truncated.md)
  and
  [`step_pca_sparse_bayes()`](https://embed.tidymodels.org/dev/reference/step_pca_sparse_bayes.md)
  now returns data unaltered if `num_comp = 0`. This is done to be
  consistent with recipes steps of the same nature.
  ([\#190](https://github.com/tidymodels/embed/issues/190))

## embed 1.1.1

CRAN release: 2023-05-30

### Bug Fixes

- Fixed bug where
  [`step_pca_truncated()`](https://embed.tidymodels.org/dev/reference/step_pca_truncated.md)
  didn’t work with zero selection.
  ([\#181](https://github.com/tidymodels/embed/issues/181))

- The tidy() methods for
  [`step_discretize_cart()`](https://embed.tidymodels.org/dev/reference/step_discretize_cart.md),
  [`step_discretize_xgb()`](https://embed.tidymodels.org/dev/reference/step_discretize_xgb.md),
  [`step_embed()`](https://embed.tidymodels.org/dev/reference/step_embed.md),
  [`step_feature_hash()`](https://embed.tidymodels.org/dev/reference/step_feature_hash.md),
  [`step_lencode_bayes()`](https://embed.tidymodels.org/dev/reference/step_lencode_bayes.md),
  [`step_lencode_glm()`](https://embed.tidymodels.org/dev/reference/step_lencode_glm.md),
  [`step_lencode_mixed()`](https://embed.tidymodels.org/dev/reference/step_lencode_mixed.md),
  [`step_pca_sparse()`](https://embed.tidymodels.org/dev/reference/step_pca_sparse.md),
  [`step_pca_sparse_bayes()`](https://embed.tidymodels.org/dev/reference/step_pca_sparse_bayes.md),
  [`step_pca_truncated()`](https://embed.tidymodels.org/dev/reference/step_pca_truncated.md),
  [`step_umap()`](https://embed.tidymodels.org/dev/reference/step_umap.md),
  and
  [`step_woe()`](https://embed.tidymodels.org/dev/reference/step_woe.md)
  now correctly return zero-row tibbles when used with empty selections.
  ([\#181](https://github.com/tidymodels/embed/issues/181))

## embed 1.1.0

CRAN release: 2023-04-14

### New Steps

- [`step_pca_truncated()`](https://embed.tidymodels.org/dev/reference/step_pca_truncated.md)
  has been added. This step only calculates the components that are
  required, and will be a speedup in cases where it is used on many
  variables. ([\#82](https://github.com/tidymodels/embed/issues/82))

### Improvements

- [`step_collapse_stringdist()`](https://embed.tidymodels.org/dev/reference/step_collapse_stringdist.md)
  has gained `method` and `options` arguments to allow for different
  types of string distance calculations.
  ([\#152](https://github.com/tidymodels/embed/issues/152))

- [`step_umap()`](https://embed.tidymodels.org/dev/reference/step_umap.md)
  has gained the argument `metric`.
  ([\#154](https://github.com/tidymodels/embed/issues/154))

- [`step_embed()`](https://embed.tidymodels.org/dev/reference/step_embed.md)
  has gained the `keep_original_cols` argument.
  ([\#176](https://github.com/tidymodels/embed/issues/176))

- All steps now have
  [`required_pkgs()`](https://generics.r-lib.org/reference/required_pkgs.html)
  methods.

- Steps with tunable arguments now have those arguments listed in the
  documentation.

- All steps that add new columns will now informatively error if name
  collision occurs.

## embed 1.0.0

CRAN release: 2022-07-02

- [`step_collapse_cart()`](https://embed.tidymodels.org/dev/reference/step_collapse_cart.md)
  can pool a predictor’s factor levels using a tree-based method.

- [`step_collapse_stringdist()`](https://embed.tidymodels.org/dev/reference/step_collapse_stringdist.md)
  can pool a predictor’s factor levels using string distances.

- Case weights support have been added to
  [`step_discretize_cart()`](https://embed.tidymodels.org/dev/reference/step_discretize_cart.md),
  [`step_discretize_xgb()`](https://embed.tidymodels.org/dev/reference/step_discretize_xgb.md),
  [`step_lencode_bayes()`](https://embed.tidymodels.org/dev/reference/step_lencode_bayes.md),
  [`step_lencode_glm()`](https://embed.tidymodels.org/dev/reference/step_lencode_glm.md),
  and
  [`step_lencode_mixed()`](https://embed.tidymodels.org/dev/reference/step_lencode_mixed.md).

## embed 0.2.0

CRAN release: 2022-04-13

- [`step_embed()`](https://embed.tidymodels.org/dev/reference/step_embed.md)
  now correctly defaults to have a random id with the word “embed”.
  ([\#102](https://github.com/tidymodels/embed/issues/102))

- [`step_feature_hash()`](https://embed.tidymodels.org/dev/reference/step_feature_hash.md)
  is soft deprecated in embed in favor of `step_dummy_hash()` in
  textrecipes. ([\#95](https://github.com/tidymodels/embed/issues/95))

- Steps now have a dedicated subsection detailing what happens when
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) is applied.
  ([\#105](https://github.com/tidymodels/embed/issues/105))

- Reorganize documentation for all recipe step `tidy` methods
  ([\#115](https://github.com/tidymodels/embed/issues/115)).

- Fixed a bug where
  [`woe_table()`](https://embed.tidymodels.org/dev/reference/woe_table.md)
  and
  [`step_woe()`](https://embed.tidymodels.org/dev/reference/step_woe.md)
  didn’t respect the factor levels of the outcome. (109)

## embed 0.1.5

CRAN release: 2021-11-24

- Re-licensed package from GPL-2 to MIT. See [consent from copyright
  holders here](https://github.com/tidymodels/embed/issues/78).

- The tunable parameter ranges for
  [`step_umap()`](https://embed.tidymodels.org/dev/reference/step_umap.md)
  were changed for `neighbors`, `num_comp`, and `min_dist` to prevent
  `uwot` segmentation faults. The step also check to see if the data
  dimensions are consistent with the argument values.

- Two new PCA steps were added, each using sparse techniques for
  estimation:
  [`step_pca_sparse()`](https://embed.tidymodels.org/dev/reference/step_pca_sparse.md)
  and
  [`step_pca_sparse_bayes()`](https://embed.tidymodels.org/dev/reference/step_pca_sparse_bayes.md).

- Updated to use
  [`recipes_eval_select()`](https://recipes.tidymodels.org/reference/recipes_eval_select.html)
  from recipes 0.1.17
  ([\#85](https://github.com/tidymodels/embed/issues/85)).

- Added `prefix` argument to
  [`step_umap()`](https://embed.tidymodels.org/dev/reference/step_umap.md)
  to harmonize with other recipes steps
  ([\#93](https://github.com/tidymodels/embed/issues/93)).

- All embed recipe steps now officially support empty selections to be
  more aligned with recipes, dplyr and other packages that use
  tidyselect.

- [`step_woe()`](https://embed.tidymodels.org/dev/reference/step_woe.md)
  no longer warns about high-cardinality predictors when the recipe is
  estimated. Instead it warns when categories have fewer than 10 data
  points in the training set.
  ([\#74](https://github.com/tidymodels/embed/issues/74))

## embed 0.1.4

CRAN release: 2021-01-16

- Minor release with changes to test for cases when CRAN cannot get
  `xgboost` to work on their Solaris configuration.

- `lme4` and `rstanarm` are now in the Suggests list so they are not
  automatically installed with `embed`. A message is written to the
  console if those packages are missing and their associated steps
  functions are invoked.

## embed 0.1.3

CRAN release: 2020-11-12

- More changes to enable better parallel processing on windows.

## embed 0.1.2

CRAN release: 2020-10-17

- Changes to enable better parallel processing on windows.

## embed 0.1.1

CRAN release: 2020-07-03

- Changes to tests to get out of archive jail.

- Updated the plumbing behind
  [`step_woe()`](https://embed.tidymodels.org/dev/reference/step_woe.md).

- Due to a bug in `tensorflow`, added a “warm start” to instigate a TF
  session if one does not currently exist.

## embed 0.1.0

CRAN release: 2020-05-25

- Changes for `dplyr` 1.0.0

### New Steps

- [`step_discretize_xgb()`](https://embed.tidymodels.org/dev/reference/step_discretize_xgb.md)
  and
  [`step_discretize_cart()`](https://embed.tidymodels.org/dev/reference/step_discretize_cart.md)
  can be used to convert numeric predictors to categorical using
  supervised binning methods based on tree models. Thanks to Konrad
  Semsch for the contribution.

- Added
  [`step_feature_hash()`](https://embed.tidymodels.org/dev/reference/step_feature_hash.md)
  for creating dummy variables using feature hashing.

### Breaking Changes

- [`tidy.step_woe()`](https://embed.tidymodels.org/dev/reference/step_woe.md)
  now has column names consistent with other recipe steps.

### Bug fixes

- Fixed a bug in detecting the TF version.

## embed 0.0.6

CRAN release: 2020-03-17

- Small changes for base R’s `stringsAsFactors` change.

## `embed` 0.0.5

CRAN release: 2020-01-07

- The example data are now in the `modeldata` package.

- Small TF updates to
  [`step_embed()`](https://embed.tidymodels.org/dev/reference/step_embed.md).

## `embed` 0.0.4

CRAN release: 2019-09-15

- Methods were added for a future generic called
  [`tunable()`](https://generics.r-lib.org/reference/tunable.html). This
  outlines which parameters in a step can/could be tuned.

- Small updates to work with different versions of `tidyr`.

## `embed` 0.0.3

CRAN release: 2019-07-12

### New Steps

- [`step_umap()`](https://embed.tidymodels.org/dev/reference/step_umap.md)
  was added for both supervised and unsupervised encodings.
- [`step_woe()`](https://embed.tidymodels.org/dev/reference/step_woe.md)
  created weight of evidence encodings.

## `embed` 0.0.2

CRAN release: 2018-11-19

A mostly maintainence release to be compatible with version 0.1.3 of
`recipes`.

### Other Changes:

- The package now depends on the `generics` pacakge to get the `broom`
  `tidy` methods.

- Karim Lahrichi added the ability to use callbacks when fitting
  tensorflow models. [PR](https://github.com/tidymodels/embed/pull/9)

## `embed` 0.0.1

CRAN release: 2018-09-14

First CRAN version
