# embed 0.0.7

 * Changes for `dplyr` 1.0.0
 
 ## New Steps
 
  * `step_discretize_xgb()` and `step_discretize_cart()` can be used to convert numeric predictors to categorical using supervised binning methods based on tree models. Thanks to Konrad Semsch for the contribution. 

 * Added `step_feature_hash()` for creating dummy variables using feature hashing. 


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
