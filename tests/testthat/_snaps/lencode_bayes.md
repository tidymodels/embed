# factor outcome - factor predictor

    Code
      class_test <- prep(step_lencode_bayes(recipe(x2 ~ ., data = ex_dat), x3,
      outcome = vars(x2), verbose = FALSE, options = opts), training = ex_dat,
      retain = TRUE)
    Condition

---

    Code
      new_values_ch <- bake(class_test, new_data = new_dat_ch)
    Condition
      Warning in `bake()`:
      ! There was 1 column that was a factor when the recipe was prepped:
      * `x3`
      i This may cause errors when processing new data.

# factor outcome - character predictor

    Code
      class_test <- prep(step_lencode_bayes(recipe(x2 ~ ., data = ex_dat_ch), x3,
      outcome = vars(x2), verbose = FALSE, options = opts, id = "id"), training = ex_dat_ch,
      retain = TRUE, options = opts)
    Condition

# numeric outcome - factor predictor

    Code
      set.seed(8283)
      reg_test <- prep(step_lencode_bayes(recipe(x1 ~ ., data = ex_dat), x3, outcome = vars(
        x1), verbose = FALSE, options = opts), training = ex_dat, retain = TRUE)
    Condition

---

    Code
      new_values_ch <- bake(reg_test, new_data = new_dat_ch)
    Condition
      Warning in `bake()`:
      ! There was 1 column that was a factor when the recipe was prepped:
      * `x3`
      i This may cause errors when processing new data.

# numeric outcome - character predictor

    Code
      set.seed(8283)
      reg_test <- prep(step_lencode_bayes(recipe(x1 ~ ., data = ex_dat_ch), x3,
      outcome = vars(x1), verbose = FALSE, options = opts), training = ex_dat_ch,
      retain = TRUE)
    Condition

# Works with passing family 

    Code
      class_test <- prep(step_lencode_bayes(recipe(outcome ~ ., data = ex_dat_poisson),
      x3, outcome = vars(outcome), verbose = FALSE, options = c(opts, family = stats::poisson)),
      training = ex_dat_poisson, retain = TRUE)
    Condition

---

    Code
      new_values_ch <- bake(class_test, new_data = new_dat_ch)
    Condition
      Warning in `bake()`:
      ! There was 1 column that was a factor when the recipe was prepped:
      * `x3`
      i This may cause errors when processing new data.

# case weights

    Code
      class_test <- prep(step_lencode_bayes(recipe(x2 ~ ., data = ex_dat_cw), x3,
      outcome = vars(x2), verbose = FALSE, options = opts), training = ex_dat_cw,
      retain = TRUE)
    Condition
    Code
      junk <- capture.output(ref_mod <- rstanarm::stan_glmer(formula = x2 ~ (1 |
      value), data = transmute(ex_dat_cw, value = x3, x2), family = binomial(),
      na.action = na.omit, seed = 34677, chains = 2, iter = 500, weights = wts_int, ))
    Condition

---

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
      * Linear embedding for factors via Bayesian GLM for: x3 | Trained, weighted

# bad args

    Code
      step_lencode_bayes(recipe(~., data = mtcars), outcome = vars(mpg), verbose = "yes")
    Condition
      Error in `step_lencode_bayes()`:
      ! `verbose` must be `TRUE` or `FALSE`, not the string "yes".

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = ex_dat[, -3])
    Condition
      Error in `step_lencode_bayes()`:
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
      * Linear embedding for factors via Bayesian GLM for: <none>

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
      * Linear embedding for factors via Bayesian GLM for: <none> | Trained

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
      * Linear embedding for factors via Bayesian GLM for: x3

---

    Code
      prep(rec)
    Condition
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 3
      
      -- Training information 
      Training data contained 500 data points and no incomplete rows.
      
      -- Operations 
      * Linear embedding for factors via Bayesian GLM for: x3 | Trained

