# run_xgboost for classification

    Code
      print(xgboost)
    Output
      ##### xgb.Booster
      raw: 54.6 Kb 
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
      best_score : 0.44215 
      best_msg : [86]	train-logloss:0.417584	test-logloss:0.442150 
      nfeatures : 13 
      evaluation_log:
          iter train_logloss test_logloss
             1      0.627907     0.630349
             2      0.587016     0.589497
      ---                                
            95      0.415791     0.442585
            96      0.415610     0.443270

# run_xgboost for multi-classification

    Code
      print(xgboost)
    Output
      ##### xgb.Booster
      raw: 110.1 Kb 
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
      best_msg : [23]	train-mlogloss:1.178122	test-mlogloss:1.246428 
      nfeatures : 30 
      evaluation_log:
          iter train_mlogloss test_mlogloss
             1       1.623173      1.631783
             2       1.515109      1.531188
      ---                                  
            32       1.159813      1.249700
            33       1.158088      1.250462

# run_xgboost for regression

    Code
      print(xgboost)
    Output
      ##### xgb.Booster
      raw: 55.7 Kb 
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
      # of features: 5 
      niter: 100
      best_iteration : 100 
      best_ntreelimit : 100 
      best_score : 9.353357 
      best_msg : [100]	train-rmse:9.360451	test-rmse:9.353357 
      nfeatures : 5 
      evaluation_log:
          iter train_rmse test_rmse
             1  24.206776 24.204262
             2  18.229397 18.225639
      ---                          
            99   9.360474  9.353493
           100   9.360451  9.353357

# xgb_binning for classification

    Code
      print(xgb_binning)
    Output
      [1]  1  2  3  5  6  9 12 15 20

# xgb_binning for multi-classification

    Code
      print(xgb_binning)
    Output
      [1] 26 31 35 38

# xgb_binning for regression

    Code
      print(xgb_binning)
    Output
      [1] 63 65 67 68 70 71

# step_discretize_xgb for classification

    Code
      print(xgb_train_bins)
    Output
      # A tibble: 1,000 x 3
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
      # ... with 990 more rows

---

    Code
      print(xgb_test_bins)
    Output
      # A tibble: 100 x 3
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
      # ... with 90 more rows

# step_discretize_xgb for multi-classification

    Code
      print(xgb_train_bins)
    Output
      # A tibble: 1,000 x 3
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
      # ... with 990 more rows

---

    Code
      print(xgb_test_bins)
    Output
      # A tibble: 100 x 3
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
      # ... with 90 more rows

# printing

    Code
      print(xgb_rec)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor          2
      
      Operations:
      
      Discretizing variables using XgBoost all_predictors()

