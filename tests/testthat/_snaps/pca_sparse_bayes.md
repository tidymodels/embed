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
      Caused by error in `check_name()`:
      ! Name collision occured in `step_pca_sparse_bayes`. The following variable names already exists: PC1.

# printing

    Code
      print_test
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 55
      
      -- Operations 
      * No Sparse PCA components were extracted from: all_predictors()

---

    Code
      prep(print_test)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 55
      
      -- Training information 
      Training data contained 1817 data points and no incomplete rows.
      
      -- Operations 
      * Sparse PCA extraction with: angle_ch_1, area_ch_1, ... | Trained

