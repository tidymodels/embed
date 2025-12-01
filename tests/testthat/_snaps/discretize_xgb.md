# xgb_binning for multi-classification

    Code
      embed:::xgb_binning(attrition_data_small, "EducationField", "Age", sample_val = 0.3,
        learn_rate = 0.3, num_breaks = 10, tree_depth = 1, min_n = 5)
    Output
      numeric(0)

# xgb_binning for regression

    Code
      embed:::xgb_binning(ames_data_small, "Sale_Price", "Latitude", sample_val = 0.3,
        learn_rate = 0.3, num_breaks = 10, tree_depth = 1, min_n = 5)
    Output
      numeric(0)

# step_discretize_xgb for multi-classification

    Code
      prep(step_discretize_xgb(recipe(class ~ ., data = sim_tr_mcls[1:9, ]),
      all_predictors(), outcome = "class"))
    Condition
      Error in `step_discretize_xgb()`:
      Caused by error in `prep()`:
      ! Too few observations in the early stopping validation set.
      i Consider increasing the `sample_val` parameter.

# xgb_binning() errors if only one class in outcome

    Code
      embed:::xgb_binning(const_outcome, "outcome", "predictor", sample_val = 0.2,
        learn_rate = 0.3, num_breaks = 10, tree_depth = 1, min_n = 5)
    Condition
      Error:
      ! Outcome variable only has less than 2 levels.
      i Doesn't conform to regresion or classification task.

# case weights step_discretize_xgb

    Code
      xgb_rec_cw
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:      1
      predictor:    2
      case_weights: 1
      
      -- Training information 
      Training data contained 1000 data points and no incomplete rows.
      
      -- Operations 
      * Discretizing variables using xgboost: x z | Trained, weighted

# bad args

    Code
      prep(step_discretize_xgb(recipe(~., data = mtcars), outcome = "class",
      sample_val = -4))
    Condition
      Error in `step_discretize_xgb()`:
      Caused by error in `prep()`:
      ! `sample_val` must be a number between 0 and 1, not the number -4.

---

    Code
      prep(step_discretize_xgb(recipe(~., data = mtcars), outcome = "class",
      learn_rate = -4))
    Condition
      Error in `step_discretize_xgb()`:
      Caused by error in `prep()`:
      ! `learn_rate` must be a number larger than or equal to 0, not the number -4.

---

    Code
      prep(step_discretize_xgb(recipe(~., data = mtcars), outcome = "class",
      num_breaks = -4))
    Condition
      Error in `step_discretize_xgb()`:
      Caused by error in `prep()`:
      ! `num_breaks` must be a whole number larger than or equal to 0, not the number -4.

---

    Code
      prep(step_discretize_xgb(recipe(~., data = mtcars), outcome = "class",
      tree_depth = -4))
    Condition
      Error in `step_discretize_xgb()`:
      Caused by error in `prep()`:
      ! `tree_depth` must be a whole number larger than or equal to 0, not the number -4.

---

    Code
      prep(step_discretize_xgb(recipe(~., data = mtcars), outcome = "class", min_n = -
      4))
    Condition
      Error in `step_discretize_xgb()`:
      Caused by error in `prep()`:
      ! `min_n` must be a whole number larger than or equal to 0, not the number -4.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = sim_tr_cls[, -1])
    Condition
      Error in `step_discretize_xgb()`:
      ! The following required column is missing from `new_data`: x.

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
      * Discretizing variables using xgboost: <none>

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
      * Discretizing variables using xgboost: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 2
      
      -- Operations 
      * Discretizing variables using xgboost: all_predictors()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 2
      
      -- Training information 
      Training data contained 1000 data points and no incomplete rows.
      
      -- Operations 
      * Discretizing variables using xgboost: x z | Trained

