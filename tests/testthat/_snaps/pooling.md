# factor encoded predictor

    Code
      class_test <- recipe(x2 ~ ., data = ex_dat) %>% step_lencode_bayes(x3, outcome = vars(
        x2), verbose = FALSE, options = opts) %>% prep(training = ex_dat, retain = TRUE)
    Condition
      Warning:
      The largest R-hat is 1.05, indicating chains have not mixed.
      Running the chains for more iterations may help. See
      https://mc-stan.org/misc/warnings.html#r-hat
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

