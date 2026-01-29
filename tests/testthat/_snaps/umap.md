# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_umap()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `UMAP1`

# bad args

    Code
      prep(step_umap(recipe(~., data = mtcars), num_comp = -4))
    Condition
      Error in `step_umap()`:
      Caused by error in `prep()`:
      ! `num_comp` must be a whole number larger than or equal to 0, not the number -4.

---

    Code
      prep(step_umap(recipe(~., data = mtcars), neighbors = -4))
    Condition
      Error in `step_umap()`:
      Caused by error in `prep()`:
      ! `neighbors` must be a whole number larger than or equal to 0, not the number -4.

---

    Code
      prep(step_umap(recipe(~., data = mtcars), min_dist = TRUE))
    Condition
      Error in `step_umap()`:
      Caused by error in `prep()`:
      ! `min_dist` must be a number, not `TRUE`.

---

    Code
      prep(step_umap(recipe(~., data = mtcars), learn_rate = -4))
    Condition
      Error in `step_umap()`:
      Caused by error in `prep()`:
      ! `learn_rate` must be a number larger than or equal to 0, not the number -4.

---

    Code
      prep(step_umap(recipe(~., data = mtcars), epochs = -4))
    Condition
      Error in `step_umap()`:
      Caused by error in `prep()`:
      ! `epochs` must be a whole number larger than or equal to 0 or `NULL`, not the number -4.

---

    Code
      prep(step_umap(recipe(~., data = mtcars), initial = "wrong"))
    Condition
      Error in `step_umap()`:
      Caused by error in `prep()`:
      ! `initial` must be one of "spectral", "normlaplacian", "random", "lvrandom", "laplacian", "pca", "spca", or "agspectral", not "wrong".

---

    Code
      prep(step_umap(recipe(~., data = mtcars), target_weight = -4))
    Condition
      Error in `step_umap()`:
      Caused by error in `prep()`:
      ! `target_weight` must be a number between 0 and 1 or `NULL`, not the number -4.

---

    Code
      step_umap(recipe(~., data = mtcars), prefix = NULL)
    Condition
      Error in `step_umap()`:
      ! `prefix` must be a single string, not `NULL`.

# bake method errors when needed non-standard role columns are missing

    Code
      bake(rec_trained, new_data = tr[, -4])
    Condition
      Error in `step_umap()`:
      ! The following required column is missing from `new_data`: Petal.Width.

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
      * UMAP embedding for: <none>

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
      * UMAP embedding for: <none> | Trained

# keep_original_cols - can prep recipes with it missing

    Code
      rec <- prep(rec)
    Condition
      Warning:
      `keep_original_cols` was added to `step_umap()` after this recipe was created.
      i Regenerate your recipe to avoid this warning.

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Operations 
      * UMAP embedding for: all_predictors()

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 4
      
      -- Training information 
      Training data contained 133 data points and no incomplete rows.
      
      -- Operations 
      * UMAP embedding for: Sepal.Length, Sepal.Width, Petal.Length, ... | Trained

