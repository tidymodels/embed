# can prep recipes with no keep_original_cols

    Code
      umap_pred <- prep(unsupervised, training = tr[, -5], verbose = FALSE)
    Condition
      Warning:
      'keep_original_cols' was added to `step_umap()` after this recipe was created.
      Regenerate your recipe to avoid this warning.

# printing

    Code
      print_test
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Operations 
      * UMAP embedding for: all_predictors()

---

    Code
      prep(print_test)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Training information 
      Training data contained 133 data points and no incomplete rows.
      
      -- Operations 
      * UMAP embedding for: Sepal.Length, Sepal.Width, Petal.Length, ... | Trained

