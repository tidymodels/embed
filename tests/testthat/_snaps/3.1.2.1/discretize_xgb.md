# run_xgboost for classification

    Code
      xgboost
    Output
      ##### xgb.Booster
      call:
      xgboost::xgb.train(params = .params, data = .train, nrounds = 100,
      evals = list(train = .train, test = .test), verbose = 0,
      early_stopping_rounds = 10)
      # of features: 13
      # of rounds:  80
      xgb.attributes:
      best_iteration, best_score
      callbacks:
      early_stop, evaluation_log
      evaluation_log:
      iter train_logloss test_logloss
      <int>         <num>        <num>
      1     0.5739956    0.5763535
      2     0.5546428    0.5563444
      ---           ---          ---
      79     0.4189363    0.4445886
      80     0.4186880    0.4440128

# run_xgboost for multi-classification

    Code
      xgboost
    Output
      ##### xgb.Booster
      call:
      xgboost::xgb.train(params = .params, data = .train, nrounds = 100,
      evals = list(train = .train, test = .test), verbose = 0,
      early_stopping_rounds = 10)
      # of features: 30
      # of rounds:  33
      xgb.attributes:
      best_iteration, best_score
      callbacks:
      early_stop, evaluation_log
      evaluation_log:
      iter train_mlogloss test_mlogloss
      <int>          <num>         <num>
      1       1.550003      1.558689
      2       1.465324      1.481185
      ---            ---           ---
      32       1.159343      1.250772
      33       1.157686      1.250718

# run_xgboost for regression

    Code
      xgboost
    Output
      ##### xgb.Booster
      call:
      xgboost::xgb.train(params = .params, data = .train, nrounds = 100,
      evals = list(train = .train, test = .test), verbose = 0,
      early_stopping_rounds = 10)
      # of features: 73
      # of rounds:  25
      xgb.attributes:
      best_iteration, best_score
      callbacks:
      early_stop, evaluation_log
      evaluation_log:
      iter train_rmse test_rmse
      <int>      <num>     <num>
      1 0.15708398 0.1633305
      2 0.14190657 0.1588983
      ---        ---       ---
      24 0.07039493 0.1211529
      25 0.06975907 0.1206107

# xgb_binning for classification

    Code
      xgb_binning
    Output
      [1]  1  2  3  5  6  9 12 15 20

# xgb_binning for multi-classification

    Code
      xgb_binning
    Output
      [1] 26

# xgb_binning for regression

    Code
      xgb_binning
    Output
      [1] 42.01972 42.02510 42.03122 42.03462 42.03840 42.04638 42.05236 42.05917

# step_discretize_xgb for classification

    Code
      xgb_train_bins[1:10, ]
    Output
      # A tibble: 10 x 3
         x               z             class
         <fct>           <fct>         <fct>
       1 [0.6808, Inf]   [0.3327, Inf] a    
       2 [0.5749,0.6808) [0.3327, Inf] b    
       3 [0.3687,0.5749) [-Inf,0.3327) b    
       4 [0.5749,0.6808) [0.3327, Inf] b    
       5 [0.6808, Inf]   [0.3327, Inf] a    
       6 [0.5749,0.6808) [0.3327, Inf] a    
       7 [0.5749,0.6808) [-Inf,0.3327) b    
       8 [0.5749,0.6808) [-Inf,0.3327) a    
       9 [0.2779,0.3687) [0.3327, Inf] a    
      10 [0.3687,0.5749) [0.3327, Inf] b    

---

    Code
      xgb_test_bins[1:10, ]
    Output
      # A tibble: 10 x 3
         x               z             class
         <fct>           <fct>         <fct>
       1 [0.5749,0.6808) [-Inf,0.3327) b    
       2 [0.5749,0.6808) [0.3327, Inf] b    
       3 [0.3687,0.5749) [-Inf,0.3327) b    
       4 [0.2779,0.3687) [0.3327, Inf] b    
       5 [0.6808, Inf]   [0.3327, Inf] a    
       6 [0.3687,0.5749) [-Inf,0.3327) b    
       7 [0.2779,0.3687) [0.3327, Inf] a    
       8 [0.6808, Inf]   [0.3327, Inf] a    
       9 [0.3687,0.5749) [0.3327, Inf] b    
      10 [0.3687,0.5749) [0.3327, Inf] b    

---

    Code
      prep(step_discretize_xgb(recipe(class ~ ., data = sim_tr_cls[1:9, ]),
      all_predictors(), outcome = "class"))
    Condition
      Error in `step_discretize_xgb()`:
      Caused by error in `prep()`:
      ! Too few observations in the early stopping validation set.
      i Consider increasing the `sample_val` parameter.

---

    Code
      set.seed(1)
      prep(step_discretize_xgb(recipe(Status ~ ., data = credit_data_train), Time,
      outcome = "Status"), retain = TRUE)
    Condition
      Warning:
      More than 20 unique training set values are required.
      i Predictors 'Time' were not processed; their original values will be used.
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 13
      
      -- Training information 
      Training data contained 3340 data points and 301 incomplete rows.
      
      -- Operations 
      * Discretizing variables using xgboost: <none> | Trained

# step_discretize_xgb for multi-classification

    Code
      xgb_train_bins[1:10, ]
    Output
      # A tibble: 10 x 3
         x               z              class
         <fct>           <fct>          <fct>
       1 [0.6879, Inf]   [0.09924, Inf] c    
       2 [0.5863,0.6879) [0.09924, Inf] b    
       3 [0.3821,0.5863) [-Inf,0.09924) b    
       4 [0.5863,0.6879) [0.09924, Inf] b    
       5 [0.6879, Inf]   [0.09924, Inf] c    
       6 [0.5863,0.6879) [0.09924, Inf] c    
       7 [0.5863,0.6879) [-Inf,0.09924) b    
       8 [0.5863,0.6879) [0.09924, Inf] c    
       9 [-Inf,0.2887)   [0.09924, Inf] a    
      10 [0.3821,0.5863) [0.09924, Inf] b    

---

    Code
      xgb_test_bins[1:10, ]
    Output
      # A tibble: 10 x 3
         x               z              class
         <fct>           <fct>          <fct>
       1 [0.5863,0.6879) [0.09924, Inf] b    
       2 [0.5863,0.6879) [0.09924, Inf] b    
       3 [0.3821,0.5863) [0.09924, Inf] b    
       4 [0.2887,0.3821) [0.09924, Inf] b    
       5 [0.6879, Inf]   [0.09924, Inf] c    
       6 [0.3821,0.5863) [-Inf,0.09924) b    
       7 [0.2887,0.3821) [0.09924, Inf] a    
       8 [0.6879, Inf]   [0.09924, Inf] c    
       9 [0.3821,0.5863) [0.09924, Inf] b    
      10 [0.3821,0.5863) [0.09924, Inf] b    

