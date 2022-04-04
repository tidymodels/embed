# low-level binning for classification

    Code
      splits <- embed:::cart_binning(sample(sim_tr_cls$x), "x", sim_tr_cls$class,
      cost_complexity = 0.01, tree_depth = 5, min_n = 10)
    Condition
      Warning:
      `step_discretize_cart()` failed to find any meaningful splits for predictor 'x', which will not be binned.

# low-level binning for regression

    Code
      splits <- embed:::cart_binning(sample(sim_tr_reg$x), "potato", sim_tr_reg$y,
      cost_complexity = 0.01, tree_depth = 5, min_n = 10)
    Condition
      Warning:
      `step_discretize_cart()` failed to find any meaningful splits for predictor 'potato', which will not be binned.

# step function for classification

    Code
      cart_rec <- recipe(class ~ ., data = sim_tr_cls) %>% step_discretize_cart(
        all_predictors(), outcome = "class") %>% prep()
    Condition
      Warning:
      `step_discretize_cart()` failed to find any meaningful splits for predictor 'z', which will not be binned.

# step function for regression

    Code
      cart_rec <- recipe(y ~ ., data = sim_tr_reg) %>% step_discretize_cart(
        all_predictors(), outcome = "y") %>% prep()
    Condition
      Warning:
      `step_discretize_cart()` failed to find any meaningful splits for predictor 'z', which will not be binned.

# bad args

    Code
      cart_rec <- recipe(y ~ ., data = tmp) %>% step_discretize_cart(all_predictors(),
      outcome = "y") %>% prep()
    Condition
      Error in `check_type()`:
      ! All columns selected for the step should be numeric

# tidy method

    Code
      cart_rec <- prep(cart_rec)
    Condition
      Warning:
      `step_discretize_cart()` failed to find any meaningful splits for predictor 'z', which will not be binned.

# printing

    Code
      cart_rec
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          2
      
      Operations:
      
      Discretizing variables using CART all_predictors()

---

    Code
      prep(cart_rec, verbose = TRUE)
    Output
      oper 1 step discretize cart [training] 
    Condition
      Warning:
      `step_discretize_cart()` failed to find any meaningful splits for predictor 'z', which will not be binned.
    Output
      The retained training set is ~ 0.02 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          2
      
      Training data contained 1000 data points and no missing data.
      
      Operations:
      
      Discretizing variables using CART x [trained]

# case weights step functions

    Code
      cart_rec <- recipe(class ~ ., data = sim_tr_cls_cw) %>% step_discretize_cart(
        all_predictors(), outcome = "class") %>% prep()
    Condition
      Warning:
      `step_discretize_cart()` failed to find any meaningful splits for predictor 'z', which will not be binned.

---

    Code
      cart_rec <- recipe(y ~ ., data = sim_tr_reg_cw) %>% step_discretize_cart(
        all_predictors(), outcome = "y") %>% prep()

---

    Code
      cart_rec
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
            outcome          1
          predictor          2
      
      Training data contained 1000 data points and no missing data.
      
      Operations:
      
      Discretizing variables using CART x, z [weighted, trained]

