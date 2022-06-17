# factor encoded predictor

    Code
      class_test <- recipe(x2 ~ ., data = ex_dat) %>% step_lencode_bayes(x3, outcome = vars(
        x2), verbose = FALSE, options = opts) %>% prep(training = ex_dat, retain = TRUE)
    Condition
      Warning:
      Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
      Running the chains for more iterations may help. See
      https://mc-stan.org/misc/warnings.html#bulk-ess
      Warning:
      Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
      Running the chains for more iterations may help. See
      https://mc-stan.org/misc/warnings.html#tail-ess

---

    Code
      new_values_ch <- bake(class_test, new_data = new_dat_ch)
    Condition
      Warning:
       There was 1 column that was a factor when the recipe was prepped:
       'x3'.
       This may cause errors when processing new data.

---

    Code
      set.seed(8283)
      reg_test <- recipe(x1 ~ ., data = ex_dat) %>% step_lencode_bayes(x3, outcome = vars(
        x1), verbose = FALSE, options = opts) %>% prep(training = ex_dat, retain = TRUE)
    Condition

---

    Code
      new_values_ch <- bake(reg_test, new_data = new_dat_ch)
    Condition
      Warning:
       There was 1 column that was a factor when the recipe was prepped:
       'x3'.
       This may cause errors when processing new data.

# character encoded predictor

    Code
      class_test <- recipe(x2 ~ ., data = ex_dat_ch) %>% step_lencode_bayes(x3,
        outcome = vars(x2), verbose = FALSE, options = opts, id = "id") %>% prep(
        training = ex_dat_ch, retain = TRUE, options = opts)
    Condition
      Warning:
      Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
      Running the chains for more iterations may help. See
      https://mc-stan.org/misc/warnings.html#bulk-ess
      Warning:
      Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
      Running the chains for more iterations may help. See
      https://mc-stan.org/misc/warnings.html#tail-ess

---

    Code
      set.seed(8283)
      reg_test <- recipe(x1 ~ ., data = ex_dat_ch) %>% step_lencode_bayes(x3,
        outcome = vars(x1), verbose = FALSE, options = opts) %>% prep(training = ex_dat_ch,
        retain = TRUE)
    Condition

# Works with passing family 

    Code
      class_test <- recipe(outcome ~ ., data = ex_dat_poisson) %>% step_lencode_bayes(
        x3, outcome = vars(outcome), verbose = FALSE, options = c(opts, family = stats::poisson)) %>%
        prep(training = ex_dat_poisson, retain = TRUE)
    Condition

---

    Code
      new_values_ch <- bake(class_test, new_data = new_dat_ch)
    Condition
      Warning:
       There was 1 column that was a factor when the recipe was prepped:
       'x3'.
       This may cause errors when processing new data.

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
      
      Linear embedding for factors via Bayesian GLM for x3

---

    Code
      prep(print_test)
    Condition
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          3
      
      Training data contained 500 data points and no missing data.
      
      Operations:
      
      Linear embedding for factors via Bayesian GLM for x3 [trained]

# case weights

    Code
      class_test <- recipe(x2 ~ ., data = ex_dat_cw) %>% step_lencode_bayes(x3,
        outcome = vars(x2), verbose = FALSE, options = opts) %>% prep(training = ex_dat_cw,
        retain = TRUE)
    Condition
    Code
      junk <- capture.output(ref_mod <- rstanarm::stan_glmer(formula = x2 ~ (1 |
      value), data = ex_dat_cw %>% transmute(value = x3, x2), family = binomial(),
      na.action = na.omit, seed = 34677, chains = 2, iter = 500, weights = wts_int, ))
    Condition

---

    Code
      class_test
    Output
      Recipe
      
      Inputs:
      
               role #variables
       case_weights          1
            outcome          1
          predictor          3
      
      Training data contained 500 data points and no missing data.
      
      Operations:
      
      Linear embedding for factors via Bayesian GLM for x3 [weighted, trained]

