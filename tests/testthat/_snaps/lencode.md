# factor outcome - factor predictor

    Code
      new_values_ch <- bake(class_test, new_data = new_dat_ch)
    Condition
      Warning in `bake()`:
      ! There was 1 column that was a factor when the recipe was prepped:
      * `x3`
      i This may cause errors when processing new data.

# factor outcome - character predictor

    Code
      new_values <- bake(class_test, new_data = new_dat_ch)

# numeric outcome - factor predictor

    Code
      new_values_ch <- bake(reg_test, new_data = new_dat_ch)
    Condition
      Warning in `bake()`:
      ! There was 1 column that was a factor when the recipe was prepped:
      * `x3`
      i This may cause errors when processing new data.

# bad args

    Code
      prep(step_lencode(recipe(Species ~ ., data = three_class), Sepal.Length,
      outcome = vars(Species)), training = three_class, retain = TRUE)
    Condition
      Error in `step_lencode()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be string, factor, or ordered.
      * 1 double variable found: `Sepal.Length`

---

    Code
      prep(step_lencode(recipe(Species ~ ., data = three_class), Species, outcome = vars(
        logical)), training = three_class, retain = TRUE)
    Condition
      Error in `step_lencode()`:
      Caused by error in `purrr::map()`:
      i In index: 1.
      i With name: Species.
      Caused by error in `.f()`:
      ! Only works nominal or numeric `outcome`, not a logical vector.

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
      * Linear embedding for factors via GLM for: x3 | Trained, weighted

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = ex_dat[, -3])
    Condition
      Error in `step_lencode()`:
      ! The following required column is missing from `new_data`: x3.

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
      * Linear embedding for factors via GLM for: <none>

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
      * Linear embedding for factors via GLM for: <none> | Trained

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
      * Linear embedding for factors via GLM for: x3

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
      * Linear embedding for factors via GLM for: x3 | Trained

