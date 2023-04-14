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
      * Sparse PCA extraction with: angle_ch_1, area_ch_1, ... | Trained

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_pca_sparse()`:
      Caused by error in `check_name()`:
      ! Name collision occured in `step_pca_sparse`. The following variable names already exists: PC1.

