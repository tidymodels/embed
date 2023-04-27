# step_pca_sparse_bayes

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
      * Sparse PCA extraction with: angle_ch_1, area_ch_1, ... | Trained

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_pca_sparse_bayes()`:
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
      predictor: 55
      
      -- Operations 
      * No Sparse PCA components were extracted from: all_predictors()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 55
      
      -- Training information 
      Training data contained 1817 data points and no incomplete rows.
      
      -- Operations 
      * Sparse PCA extraction with: angle_ch_1, area_ch_1, ... | Trained

