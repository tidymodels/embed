# factor encoded predictor

    Code
      new_values_ch <- bake(class_test, new_data = new_dat_ch)
    Condition
      Warning:
       There was 1 column that was a factor when the recipe was prepped:
       'x3'.
       This may cause errors when processing new data.

---

    Code
      new_values_ch <- bake(reg_test, new_data = new_dat_ch)
    Condition
      Warning:
       There was 1 column that was a factor when the recipe was prepped:
       'x3'.
       This may cause errors when processing new data.

# bad args

    Code
      recipe(Species ~ ., data = three_class) %>% step_lencode_mixed(Sepal.Length,
        outcome = vars(Species)) %>% prep(training = three_class, retain = TRUE)
    Condition
      Error in `step_lencode_mixed()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be string, factor, or ordered.

# case weights

    Code
      class_test
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:      1
      predictor:    3
      case_weights: 1
      
      -- Training information 
      Training data contained 500 data points and no incomplete rows.
      
      -- Operations 
      * Linear embedding for factors via mixed effects for: x3 | Trained, weighted

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 3
      
      -- Operations 
      * Linear embedding for factors via mixed effects for: x3

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 3
      
      -- Training information 
      Training data contained 500 data points and no incomplete rows.
      
      -- Operations 
      * Linear embedding for factors via mixed effects for: x3 | Trained

