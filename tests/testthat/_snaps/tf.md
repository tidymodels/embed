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
      Error in `check_type()`:
      ! All columns selected for the step should be factor or character

# printing

    Code
      print_test
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          3
      
      Operations:
      
      Embedding of factors via tensorflow for x3

---

    Code
      prep(print_test, training = ex_dat_ch, verbose = TRUE)
    Output
      oper 1 step embed [training] 
      The retained training set is ~ 0.02 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          3
      
      Training data contained 500 data points and no missing data.
      
      Operations:
      
      Embedding of factors via tensorflow for x3 [trained]

