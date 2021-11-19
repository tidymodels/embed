# printing

    Code
      print(cart_rec)
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
      expect_warning(prep(cart_rec, verbose = TRUE),
      "failed to find any meaningful splits for predictor 'z'")
    Output
      oper 1 step discretize cart [training] 
      The retained training set is ~ 0.02 Mb  in memory.
      

