# bad args

    Code
      step_collapse_stringdist(recipe(~., data = mtcars), cost_complexity = -4)
    Condition
      Error in `step_collapse_stringdist()`:
      ! `distance` must be a number, not `NULL`.

---

    Code
      step_collapse_stringdist(recipe(~., data = mtcars), min_n = -4)
    Condition
      Error in `step_collapse_stringdist()`:
      ! `distance` must be a number, not `NULL`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = ames[, -1])
    Condition
      Error in `step_collapse_stringdist()`:
      ! The following required column is missing from `new_data`: MS_SubClass.

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
      * Collapsing factor levels using stringdist: <none>

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
      * Collapsing factor levels using stringdist: <none> | Trained

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 73
      
      -- Operations 
      * Collapsing factor levels using stringdist: MS_SubClass

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 73
      
      -- Training information 
      Training data contained 2930 data points and no incomplete rows.
      
      -- Operations 
      * Collapsing factor levels using stringdist: MS_SubClass | Trained

