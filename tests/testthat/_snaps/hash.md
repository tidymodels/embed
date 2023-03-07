# basic usage

    Code
      res_te <- bake(rec_tr, te_dat, dplyr::starts_with("x3"))
    Condition
      Warning:
       There was 1 column that was a factor when the recipe was prepped:
       'x3'.
       This may cause errors when processing new data.

# can prep recipes with no keep_original_cols

    Code
      rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)
    Condition
      Warning:
      'keep_original_cols' was added to `step_feature_hash()` after this recipe was created.
      Regenerate your recipe to avoid this warning.
      Warning in `dplyr::left_join()`:
      Each row in `x` is expected to match at most 1 row in `y`.
      i Row 1 of `x` matches multiple rows.
      i If multiple matches are expected, set `multiple = "all"` to silence this warning.

# printing

    Code
      print_test
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 1
      
      -- Operations 
      * Feature hashed dummy variables for: x3

---

    Code
      prep(print_test)
    Condition
      Warning in `dplyr::left_join()`:
      Each row in `x` is expected to match at most 1 row in `y`.
      i Row 1 of `x` matches multiple rows.
      i If multiple matches are expected, set `multiple = "all"` to silence this warning.
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 1
      
      -- Training information 
      Training data contained 500 data points and no incomplete rows.
      
      -- Operations 
      * Feature hashed dummy variables for: <none> | Trained

