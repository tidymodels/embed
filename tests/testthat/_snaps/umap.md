# can prep recipes with no keep_original_cols

    Code
      umap_pred <- prep(unsupervised, training = tr[, -5], verbose = FALSE)
    Condition
      Warning:
      'keep_original_cols' was added to `step_umap()` after this recipe was created.
      Regenerate your recipe to avoid this warning.

