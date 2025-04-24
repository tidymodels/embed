# step_pca_sparse

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 56
      
      -- Training information 
      Training data contained 1817 data points and no incomplete rows.
      
      -- Operations 
      * Sparse PCA extraction with: angle_ch_1 area_ch_1, ... | Trained

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_pca_sparse()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `PC1`

# bad args

    Code
      prep(step_pca_sparse(recipe(~., data = mtcars), num_comp = -4))
    Condition
      Error in `step_pca_sparse()`:
      Caused by error in `prep()`:
      ! `num_comp` must be a whole number larger than or equal to 0, not the number -4.

---

    Code
      prep(step_pca_sparse(recipe(~., data = mtcars), predictor_prop = -4))
    Condition
      Error in `step_pca_sparse()`:
      Caused by error in `prep()`:
      ! `predictor_prop` must be a number between 0 and 1, not the number -4.

---

    Code
      step_pca_sparse(recipe(~., data = mtcars), prefix = NULL)
    Condition
      Error in `step_pca_sparse()`:
      ! `prefix` must be a single string, not `NULL`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = tr[, -3])
    Condition
      Error in `step_pca_sparse()`:
      ! The following required column is missing from `new_data`: avg_inten_ch_1.

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
      * No Sparse PCA components were extracted from: <none>

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
      * No Sparse PCA components were extracted from: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_pca_sparse()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * No Sparse PCA components were extracted from: all_predictors()

---

    Code
      prep(rec)
    Condition
      Warning in `irlba::ssvd()`:
      no sparsity constraints specified
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Sparse PCA extraction with: cyl, disp, hp, drat, wt, qsec, ... | Trained

