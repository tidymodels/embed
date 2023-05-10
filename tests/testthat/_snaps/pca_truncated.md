# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_pca_truncated()`:
      Caused by error in `bake()`:
      ! Name collision occured. The following variable names already exists:
      i  PC1

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
      * Truncated PCA extraction with: all_predictors()

---

    Code
      prep(rec)
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

