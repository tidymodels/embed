# step_woe

    Code
      prep(rec_all_nominal, training = credit_tr, verbose = TRUE)
    Output
      oper 1 step woe [training] 
    Warning <warning>
      Some columns used by `step_woe()` have categories with less than 10 values: 'Home', 'Job'
    Output
      The retained training set is ~ 0.14 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         13
      
      Training data contained 2000 data points and 186 incomplete rows. 
      
      Operations:
      
      WoE version against outcome Status for Home, Marital, Records, Job [trained]

# printing

    Code
      print(woe_extract)
    Output
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         13
      
      Operations:
      
      WoE version against outcome Status for Job, Home

---

    Code
      prep(woe_extract, training = credit_tr, verbose = TRUE)
    Output
      oper 1 step woe [training] 
    Warning <warning>
      Some columns used by `step_woe()` have categories with less than 10 values: 'Home', 'Job'
    Output
      The retained training set is ~ 0.13 Mb  in memory.
      
      Recipe
      
      Inputs:
      
            role #variables
         outcome          1
       predictor         13
      
      Training data contained 2000 data points and 186 incomplete rows. 
      
      Operations:
      
      WoE version against outcome Status for Job, Home [trained]

