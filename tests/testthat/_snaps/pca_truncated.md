# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_pca_truncated()`:
      Caused by error in `check_name()`:
      ! Name collision occured in `step_pca_truncated`. The following variable names already exists: PC1.

# printing

    Code
      cart_rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
      * Truncated PCA extraction with: all_predictors()

---

    Code
      prep(cart_rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
      * Truncated PCA extraction with: cyl, disp, hp, drat, wt, qsec, ... | Trained

