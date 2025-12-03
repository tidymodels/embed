# embed (development version)

# embed 1.2.1

## Improvements

* Make work with all versions of xgboost. (#267)

# embed 1.2.0

## Improvements

* Adds `step_lencode()` to perform analytical likelihood encoding. (#258)

* Adds `smooth` argument to `step_lencode()` to allow for partial pooling in numeric outcomes. (#261)

* `step_feature_hash()` has been fully deprecated in favor of `textrecipes::step_dummy_hash()`. (#253)

# embed 1.1.5

## Improvements

* `step_umap()` has tunable `initial` and `target_weight` arguments. (#223, #222)

* All messages, warnings and errors has been translated to use {cli} package (#153, #155).

# embed 1.1.4

## Improvements

* `step_umap()` has gained `initial` and `target_weight` arguments. (#213)

* Calling `?tidy.step_*()` now sends you to the documentation for `step_*()` where the outcome is documented. (#216)

* Documentation for tidy methods for all steps has been improved to describe the return value more accurately. (#217)

* {keras} and {tensorflow} have been moved to Suggests instead of Imports. (#218)

# embed 1.1.3

* `step_collapse_stringdist()` will now return predictors as factors. (#204)

* Fixed regression from 1.1.2 in `step_lencode_glm()` where it couldn't be used on multiple columns.

# embed 1.1.2

## Improvements

* The `keep_original_cols` argument has been added to `step_woe()`. This change should mean that every step that produces new columns has the `keep_original_cols` argument. (#194)

* Many internal changes to improve consistency and slight speed increases.

## Breaking Changes

* `step_pca_sparse()`, `step_pca_truncated()` and `step_pca_sparse_bayes()` now returns data unaltered if `num_comp = 0`. This is done to be consistent with recipes steps of the same nature. (#190)

# embed 1.1.1

## Bug Fixes

* Fixed bug where `step_pca_truncated()` didn't work with zero selection. (#181)

* The tidy() methods for `step_discretize_cart()`, `step_discretize_xgb()`, `step_embed()`, `step_feature_hash()`, `step_lencode_bayes()`, `step_lencode_glm()`, `step_lencode_mixed()`, `step_pca_sparse()`, `step_pca_sparse_bayes()`, `step_pca_truncated()`, `step_umap()`, and `step_woe()` now correctly return zero-row tibbles when used with empty selections. (#181)

# embed 1.1.0

## New Steps

* `step_pca_truncated()` has been added. This step only calculates the components that are required, and will be a speedup in cases where it is used on many variables. (#82)

## Improvements

* `step_collapse_stringdist()` has gained `method` and `options` arguments to allow for different types of string distance calculations. (#152)
* `step_umap()` has gained the argument `metric`. (#154)

* `step_embed()` has gained the `keep_original_cols` argument. (#176)

* All steps now have `required_pkgs()` methods.

* Steps with tunable arguments now have those arguments listed in the documentation.

* All steps that add new columns will now informatively error if name collision occurs.

# embed 1.0.0

* `step_collapse_cart()` can pool a predictor's factor levels using a tree-based method. 

* `step_collapse_stringdist()` can pool a predictor's factor levels using string distances.

* Case weights support have been added to `step_discretize_cart()`, `step_discretize_xgb()`, `step_lencode_bayes()`, `step_lencode_glm()`, and `step_lencode_mixed()`.

# embed 0.2.0

* `step_embed()` now correctly defaults to have a random id with the word "embed". (#102)

* `step_feature_hash()` is soft deprecated in embed in favor of `step_dummy_hash()` in textrecipes. (#95)

* Steps now have a dedicated subsection detailing what happens when `tidy()` is applied. (#105)

* Reorganize documentation for all recipe step `tidy` methods (#115).

* Fixed a bug where `woe_table()` and `step_woe()` didn't respect the factor levels of the outcome. (109)

# embed 0.1.5

* Re-licensed package from GPL-2 to MIT. See [consent from copyright holders here](https://github.com/tidymodels/embed/issues/78).

* The tunable parameter ranges for `step_umap()` were changed for `neighbors`, `num_comp`, and `min_dist` to prevent `uwot` segmentation faults. The step also check to see if the data dimensions are consistent with the argument values. 

* Two new PCA steps were added, each using sparse techniques for estimation: `step_pca_sparse()` and `step_pca_sparse_bayes()`.

* Updated to use `recipes_eval_select()` from recipes 0.1.17 (#85).

* Added `prefix` argument to `step_umap()` to harmonize with other recipes steps (#93).

* All embed recipe steps now officially support empty selections to be more aligned with recipes, dplyr and other packages that use tidyselect.

* `step_woe()` no longer warns about high-cardinality predictors when the recipe is estimated. Instead it warns when categories have fewer than 10 data points in the training set.  (#74)


# embed 0.1.4

 * Minor release with changes to test for cases when CRAN cannot get `xgboost` to work on their Solaris configuration. 
 
 * `lme4` and `rstanarm` are now in the Suggests list so they are not automatically installed with `embed`. A message is written to the console if those packages are missing and their associated steps functions are invoked. 

# embed 0.1.3

 * More changes to enable better parallel processing on windows. 

# embed 0.1.2

 * Changes to enable better parallel processing on windows. 

# embed 0.1.1

 * Changes to tests to get out of archive jail.
 
 * Updated the plumbing behind `step_woe()`. 
 
 * Due to a bug in `tensorflow`, added a "warm start" to instigate a TF session if one does not currently exist. 
 
# embed 0.1.0

 * Changes for `dplyr` 1.0.0
 
## New Steps
 
  * `step_discretize_xgb()` and `step_discretize_cart()` can be used to convert numeric predictors to categorical using supervised binning methods based on tree models. Thanks to Konrad Semsch for the contribution. 

 * Added `step_feature_hash()` for creating dummy variables using feature hashing. 

## Breaking Changes

 * `tidy.step_woe()` now has column names consistent with other recipe steps. 

## Bug fixes

 * Fixed a bug in detecting the TF version. 


# embed 0.0.6

* Small changes for base R's `stringsAsFactors` change. 

# `embed` 0.0.5

 * The example data are now in the `modeldata` package. 
 
 * Small TF updates to `step_embed()`. 


# `embed` 0.0.4

 * Methods were added for a future generic called `tunable()`. This outlines which parameters in a step can/could be tuned.

 * Small updates to work with different versions of `tidyr`.  
 

# `embed` 0.0.3

## New Steps

 * `step_umap()` was added for both supervised and unsupervised encodings. 
 * `step_woe()` created weight of evidence encodings.


# `embed` 0.0.2

A mostly maintainence release to be compatible with version 0.1.3 of `recipes`. 

## Other Changes:

 * The package now depends on the `generics` pacakge to get the `broom` `tidy` methods. 

 * Karim Lahrichi added the ability to use callbacks when fitting tensorflow models. [PR](https://github.com/tidymodels/embed/pull/9)


# `embed` 0.0.1

First CRAN version
