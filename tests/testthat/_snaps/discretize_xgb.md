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

