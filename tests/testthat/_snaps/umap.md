# can prep recipes with no keep_original_cols

    Code
      umap_pred <- prep(unsupervised, training = tr[, -5], verbose = FALSE)
    Condition
      Warning:
      'keep_original_cols' was added to `step_umap()` after this recipe was created.
      Regenerate your recipe to avoid this warning.

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_umap()`:
      Caused by error in `bake()`:
      ! Name collision occured. The following variable names already exists:
      i  UMAP1

# empty printing

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * UMAP embedding for: <none>

---

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * UMAP embedding for: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Operations 
      * UMAP embedding for: all_predictors()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Training information 
      Training data contained 133 data points and no incomplete rows.
      
      -- Operations 
      * UMAP embedding for: Sepal.Length, Sepal.Width, Petal.Length, ... | Trained

