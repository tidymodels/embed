# `embed` 0.0.4.9000

 * The example data are now in the `modeldata` package. 


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
