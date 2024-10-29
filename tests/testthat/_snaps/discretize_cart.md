# low-level binning for classification

    Code
      splits <- embed:::cart_binning(sample(sim_tr_cls$x), "x", sim_tr_cls$class,
      cost_complexity = 0.01, tree_depth = 5, min_n = 10)
    Condition
      Warning:
      `step_discretize_cart()` failed to find any meaningful splits for predictor "x", which will not be binned.

# low-level binning for regression

    Code
      splits <- embed:::cart_binning(sample(sim_tr_reg$x), "potato", sim_tr_reg$y,
      cost_complexity = 0.01, tree_depth = 5, min_n = 10)
    Condition
      Warning:
      `step_discretize_cart()` failed to find any meaningful splits for predictor "potato", which will not be binned.

# step function for classification

    Code
      cart_rec <- recipe(class ~ ., data = sim_tr_cls) %>% step_discretize_cart(
        all_predictors(), outcome = "class") %>% prep()
    Condition
      Warning:
      `step_discretize_cart()` failed to find any meaningful splits for predictor "z", which will not be binned.

# step function for regression

    Code
      cart_rec <- recipe(y ~ ., data = sim_tr_reg) %>% step_discretize_cart(
        all_predictors(), outcome = "y") %>% prep()
    Condition
      Warning:
      `step_discretize_cart()` failed to find any meaningful splits for predictor "z", which will not be binned.

# bad args

    Code
      cart_rec <- recipe(y ~ ., data = tmp) %>% step_discretize_cart(all_predictors(),
      outcome = "y") %>% prep()
    Condition
      Error in `step_discretize_cart()`:
      Caused by error in `prep()`:
      x All columns selected for the step should be double or integer.
      * 1 factor variable found: `w`

# tidy method

    Code
      cart_rec <- prep(cart_rec)
    Condition
      Warning:
      `step_discretize_cart()` failed to find any meaningful splits for predictor "z", which will not be binned.

# case weights step functions

    Code
      cart_rec <- recipe(class ~ ., data = sim_tr_cls_cw) %>% step_discretize_cart(
        all_predictors(), outcome = "class") %>% prep()
    Condition
      Warning:
      `step_discretize_cart()` failed to find any meaningful splits for predictor "z", which will not be binned.

---

    Code
      cart_rec <- recipe(y ~ ., data = sim_tr_reg_cw) %>% step_discretize_cart(
        all_predictors(), outcome = "y") %>% prep()

---

    Code
      cart_rec
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
      * Discretizing variables using CART: x and z | Trained, weighted

# bake method errors when needed non-standard role columns are missing

    Code
      rec_trained <- prep(rec, training = sim_tr_cls, verbose = FALSE)
    Condition
      Warning:
      `step_discretize_cart()` failed to find any meaningful splits for predictor "z", which will not be binned.

---

    Code
      bake(rec_trained, new_data = sim_tr_cls[, -1])
    Condition
      Error in `step_discretize_cart()`:
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
      * Discretizing variables using CART: <none>

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
      * Discretizing variables using CART: <none> | Trained

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
      * Discretizing variables using CART: all_predictors()

---

    Code
      prep(rec)
    Condition
      Warning:
      `step_discretize_cart()` failed to find any meaningful splits for predictor "z", which will not be binned.
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 2
      
      -- Training information 
      Training data contained 1000 data points and no incomplete rows.
      
      -- Operations 
      * Discretizing variables using CART: x | Trained

