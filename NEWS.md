# embed (development version)

* Re-licensed package from GPL-2 to MIT. See [consent from copyright holders here](https://github.com/tidymodels/embed/issues/78).

* The tunable parameter ranges for `step_umap()` were changed for `neighbors`, `num_comp`, and `min_dist` to prevent `uwot` segmentation faults. The step also check to see if the data dimensions are consistent with the argument values. 

* Two new PCA steps were added, each using sparse techniques for estimation: `step_pca_sparse()` and `step_pca_sparse_bayes()`.

* Updated to use `recipes_eval_select()` from recipes 0.1.17 (#85).

* Added `prefix` argument to `step_umap()` to harmonize with other recipes steps.

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
