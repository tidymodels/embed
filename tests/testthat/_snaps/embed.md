# factor encoded predictor

    Code
      new_values_ch <- bake(class_test, new_data = new_dat_ch, contains("embed"))
    Condition
      Warning:
       There was 1 column that was a factor when the recipe was prepped:
       'x3'.
       This may cause errors when processing new data.

---

    Code
      new_values_ch <- bake(class_test, new_data = new_dat_ch, contains("embed"))
    Condition
      Warning:
       There was 1 column that was a factor when the recipe was prepped:
       'x3'.
       This may cause errors when processing new data.

# bad args

    Code
      recipe(Species ~ ., data = three_class) %>% step_embed(Sepal.Length, outcome = vars(
        Species)) %>% prep(training = three_class, retain = TRUE)
    Condition
      Error in `step_embed()`:
      Caused by error in `prep()`:
      ! All columns selected for the step should be string, factor, or ordered.

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_embed()`:
      Caused by error in `check_name()`:
      ! Name collision occured in `step_embed`. The following variable names already exists: x3_embed_1.

# printing

    Code
      print_test
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 3
      
      -- Operations 
      * Embedding of factors via tensorflow for: x3

---

    Code
      prep(print_test)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 3
      
      -- Training information 
      Training data contained 500 data points and no incomplete rows.
      
      -- Operations 
      * Embedding of factors via tensorflow for: x3 | Trained

