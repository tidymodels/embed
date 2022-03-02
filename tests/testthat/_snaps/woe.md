# woe_table do not accept different length inputs

    Code
      embed:::woe_table(rep(c(0, 1), 20), rep(letters[1:4], 5))
    Error <rlang_error>
      'outcome' must have exactly 2 categories (has 4)

# woe_table accepts only outcome with 2 distinct categories

    Code
      embed:::woe_table(rep(letters[1:3], 10), rep(c(0, 1, 2), 10))
    Error <rlang_error>
      'outcome' must have exactly 2 categories (has 3)

---

    Code
      embed:::woe_table(rep(letters[1:3], 10), rep(c(0), 30))
    Error <rlang_error>
      'outcome' must have exactly 2 categories (has 1)

---

    Code
      embed:::woe_table(df$x2, df$x1)
    Error <rlang_error>
      'outcome' must have exactly 2 categories (has 3)

# add_woe do not accept dictionary with unexpected layout

    Code
      add_woe(df, outcome = "y", x1, dictionary = iris)
    Error <rlang_error>
      column "variable" is missing in dictionary.

---

    Code
      add_woe(df, outcome = "y", x1, dictionary = iris %>% mutate(variable = 1))
    Error <rlang_error>
      column "predictor" is missing in dictionary.

# step_woe

    Code
      woe_models <- prep(rec, training = credit_tr)
    Warning <rlang_warning>
      Some columns used by `step_woe()` have categories with less than 10 values: 'Home', 'Job'

---

    Code
      prep(rec_all_nominal, training = credit_tr, verbose = TRUE)
    Output
      oper 1 step woe [training] 
    Warning <rlang_warning>
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

---

    Code
      prep(rec_all_numeric, training = credit_tr)
    Error <rlang_error>
      All columns selected for the step should be factor or character

# printing

    Code
      woe_extract
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
    Warning <rlang_warning>
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

# 2-level factors

    Code
      recipe(Species ~ ., data = iris3) %>% step_woe(group, outcome = vars(Species)) %>%
        prep()
    Error <rlang_error>
      'outcome' must have exactly 2 categories (has 3)

