# run_xgboost for classification

    Code
      xgboost
    Output
      ##### xgb.Booster
      raw: 74.2 Kb
      call:
      xgboost::xgb.train(params = .params, data = .train, nrounds = 100,
      watchlist = list(train = .train, test = .test), verbose = 0,
      early_stopping_rounds = 10, tree_method = "hist", objective = .objective,
      nthread = 1)
      params (as set within xgb.train):
      eta = "0.3", max_bin = "10", max_depth = "1", min_child_weight = "5", tree_method = "hist", objective = "binary:logistic", nthread = "1", validate_parameters = "TRUE"
      xgb.attributes:
      best_iteration, best_msg, best_ntreelimit, best_score, niter
      callbacks:
      cb.evaluation.log()
      cb.early.stop(stopping_rounds = early_stopping_rounds, maximize = maximize,
      verbose = verbose)
      # of features: 13
      niter: 96
      best_iteration : 86
      best_ntreelimit : 86
      best_score : 0.4421503
      best_msg : [86]	train-logloss:0.417583	test-logloss:0.442150
      nfeatures : 13
      evaluation_log:
      iter train_logloss test_logloss
      <num>         <num>        <num>
      1     0.6279229    0.6303495
      2     0.5869984    0.5894989
      ---           ---          ---
      95     0.4157892    0.4425857
      96     0.4156102    0.4432699

# run_xgboost for multi-classification

    Code
      xgboost
    Output
      ##### xgb.Booster
      raw: 149.7 Kb
      call:
      xgboost::xgb.train(params = .params, data = .train, nrounds = 100,
      watchlist = list(train = .train, test = .test), verbose = 0,
      early_stopping_rounds = 10, tree_method = "hist", objective = .objective,
      nthread = 1)
      params (as set within xgb.train):
      eta = "0.3", max_bin = "10", max_depth = "1", min_child_weight = "5", num_class = "6", tree_method = "hist", objective = "multi:softprob", nthread = "1", validate_parameters = "TRUE"
      xgb.attributes:
      best_iteration, best_msg, best_ntreelimit, best_score, niter
      callbacks:
      cb.evaluation.log()
      cb.early.stop(stopping_rounds = early_stopping_rounds, maximize = maximize,
      verbose = verbose)
      # of features: 30
      niter: 33
      best_iteration : 23
      best_ntreelimit : 23
      best_score : 1.246428
      best_msg : [23]	train-mlogloss:1.178121	test-mlogloss:1.246428
      nfeatures : 30
      evaluation_log:
      iter train_mlogloss test_mlogloss
      <num>          <num>         <num>
      1       1.623174      1.631783
      2       1.515108      1.531188
      ---            ---           ---
      32       1.159813      1.249701
      33       1.158088      1.250462

# run_xgboost for regression

    Code
      xgboost
    Output
      ##### xgb.Booster
      raw: 40.2 Kb
      call:
      xgboost::xgb.train(params = .params, data = .train, nrounds = 100,
      watchlist = list(train = .train, test = .test), verbose = 0,
      early_stopping_rounds = 10, tree_method = "hist", objective = .objective,
      nthread = 1)
      params (as set within xgb.train):
      eta = "0.3", max_bin = "10", max_depth = "1", min_child_weight = "5", tree_method = "hist", objective = "reg:squarederror", nthread = "1", validate_parameters = "TRUE"
      xgb.attributes:
      best_iteration, best_msg, best_ntreelimit, best_score, niter
      callbacks:
      cb.evaluation.log()
      cb.early.stop(stopping_rounds = early_stopping_rounds, maximize = maximize,
      verbose = verbose)
      # of features: 73
      niter: 50
      best_iteration : 40
      best_ntreelimit : 40
      best_score : 0.1165337
      best_msg : [40]	train-rmse:0.064010	test-rmse:0.116534
      nfeatures : 73
      evaluation_log:
      iter train_rmse test_rmse
      <num>      <num>     <num>
      1 3.31007782 3.3068878
      2 2.31969213 2.3262197
      ---        ---       ---
      49 0.06207940 0.1175223
      50 0.06191289 0.1188113

# xgb_binning for classification

    Code
      xgb_binning
    Output
      [1]  1  2  3  5  6  9 12 15 20

# xgb_binning for multi-classification

    Code
      xgb_binning
    Output
      [1] 26 31 35 38

---

    Code
      embed:::xgb_binning(attrition_data_small, "EducationField", "Age", sample_val = 0.3,
        learn_rate = 0.3, num_breaks = 10, tree_depth = 1, min_n = 5)
    Output
      numeric(0)

# xgb_binning for regression

    Code
      xgb_binning
    Output
      [1] 42.01972 42.02510 42.03122 42.03462 42.03840 42.04638 42.05236 42.05917

---

    Code
      embed:::xgb_binning(ames_data_small, "Sale_Price", "Latitude", sample_val = 0.3,
        learn_rate = 0.3, num_breaks = 10, tree_depth = 1, min_n = 5)
    Output
      numeric(0)

# step_discretize_xgb for classification

    Code
      xgb_train_bins[1:10, ]
    Output
      # A tibble: 10 x 3
         x               z               class
         <fct>           <fct>           <fct>
       1 [0.6808, Inf]   [0.4208,0.7999) a    
       2 [0.5749,0.6808) [0.4208,0.7999) b    
       3 [0.3687,0.5749) [-Inf,0.3327)   b    
       4 [0.5749,0.6808) [0.4208,0.7999) b    
       5 [0.6808, Inf]   [0.4208,0.7999) a    
       6 [0.5749,0.6808) [0.7999, Inf]   a    
       7 [0.5749,0.6808) [-Inf,0.3327)   b    
       8 [0.5749,0.6808) [-Inf,0.3327)   a    
       9 [0.2779,0.3687) [0.4208,0.7999) a    
      10 [0.3687,0.5749) [0.4208,0.7999) b    

---

    Code
      xgb_test_bins[1:10, ]
    Output
      # A tibble: 10 x 3
         x               z               class
         <fct>           <fct>           <fct>
       1 [0.5749,0.6808) [-Inf,0.3327)   b    
       2 [0.5749,0.6808) [0.4208,0.7999) b    
       3 [0.3687,0.5749) [-Inf,0.3327)   b    
       4 [0.2779,0.3687) [0.4208,0.7999) b    
       5 [0.6808, Inf]   [0.7999, Inf]   a    
       6 [0.3687,0.5749) [-Inf,0.3327)   b    
       7 [0.2779,0.3687) [0.4208,0.7999) a    
       8 [0.6808, Inf]   [0.7999, Inf]   a    
       9 [0.3687,0.5749) [0.7999, Inf]   b    
      10 [0.3687,0.5749) [0.4208,0.7999) b    

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
         x               z             class
         <fct>           <fct>         <fct>
       1 [0.6879, Inf]   [0.3274, Inf] c    
       2 [0.5863,0.6879) [0.3274, Inf] b    
       3 [0.3821,0.5863) [-Inf,0.3274) b    
       4 [0.5863,0.6879) [0.3274, Inf] b    
       5 [0.6879, Inf]   [0.3274, Inf] c    
       6 [0.5863,0.6879) [0.3274, Inf] c    
       7 [0.5863,0.6879) [-Inf,0.3274) b    
       8 [0.5863,0.6879) [-Inf,0.3274) c    
       9 [-Inf,0.2887)   [0.3274, Inf] a    
      10 [0.3821,0.5863) [0.3274, Inf] b    

---

    Code
      xgb_test_bins[1:10, ]
    Output
      # A tibble: 10 x 3
         x               z             class
         <fct>           <fct>         <fct>
       1 [0.5863,0.6879) [-Inf,0.3274) b    
       2 [0.5863,0.6879) [0.3274, Inf] b    
       3 [0.3821,0.5863) [-Inf,0.3274) b    
       4 [0.2887,0.3821) [0.3274, Inf] b    
       5 [0.6879, Inf]   [0.3274, Inf] c    
       6 [0.3821,0.5863) [-Inf,0.3274) b    
       7 [0.2887,0.3821) [0.3274, Inf] a    
       8 [0.6879, Inf]   [0.3274, Inf] c    
       9 [0.3821,0.5863) [0.3274, Inf] b    
      10 [0.3821,0.5863) [0.3274, Inf] b    

---

    Code
      prep(step_discretize_xgb(recipe(class ~ ., data = sim_tr_mcls[1:9, ]),
      all_predictors(), outcome = "class"))
    Condition
      Error in `step_discretize_xgb()`:
      Caused by error in `prep()`:
      ! Too few observations in the early stopping validation set.
      i Consider increasing the `sample_val` parameter.

# xgb_binning() errors if only one class in outcome

    Code
      embed:::xgb_binning(const_outcome, "outcome", "predictor", sample_val = 0.2,
        learn_rate = 0.3, num_breaks = 10, tree_depth = 1, min_n = 5)
    Condition
      Error:
      ! Outcome variable only has less than 2 levels.
      i Doesn't conform to regresion or classification task.

# case weights step_discretize_xgb

    Code
      xgb_rec_cw
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
      * Discretizing variables using xgboost: x z | Trained, weighted

# bad args

    Code
      prep(step_discretize_xgb(recipe(~., data = mtcars), outcome = "class",
      sample_val = -4))
    Condition
      Error in `step_discretize_xgb()`:
      Caused by error in `prep()`:
      ! `sample_val` must be a number between 0 and 1, not the number -4.

---

    Code
      prep(step_discretize_xgb(recipe(~., data = mtcars), outcome = "class",
      learn_rate = -4))
    Condition
      Error in `step_discretize_xgb()`:
      Caused by error in `prep()`:
      ! `learn_rate` must be a number larger than or equal to 0, not the number -4.

---

    Code
      prep(step_discretize_xgb(recipe(~., data = mtcars), outcome = "class",
      num_breaks = -4))
    Condition
      Error in `step_discretize_xgb()`:
      Caused by error in `prep()`:
      ! `num_breaks` must be a whole number larger than or equal to 0, not the number -4.

---

    Code
      prep(step_discretize_xgb(recipe(~., data = mtcars), outcome = "class",
      tree_depth = -4))
    Condition
      Error in `step_discretize_xgb()`:
      Caused by error in `prep()`:
      ! `tree_depth` must be a whole number larger than or equal to 0, not the number -4.

---

    Code
      prep(step_discretize_xgb(recipe(~., data = mtcars), outcome = "class", min_n = -
      4))
    Condition
      Error in `step_discretize_xgb()`:
      Caused by error in `prep()`:
      ! `min_n` must be a whole number larger than or equal to 0, not the number -4.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = sim_tr_cls[, -1])
    Condition
      Error in `step_discretize_xgb()`:
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
      * Discretizing variables using xgboost: <none>

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
      * Discretizing variables using xgboost: <none> | Trained

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
      * Discretizing variables using xgboost: all_predictors()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:   1
      predictor: 2
      
      -- Training information 
      Training data contained 1000 data points and no incomplete rows.
      
      -- Operations 
      * Discretizing variables using xgboost: x z | Trained

