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
      recipe(~., data = mtcars) %>% step_umap(num_comp = -4) %>% prep()
    Condition
      Error in `step_umap()`:
      Caused by error in `prep()`:
      ! `num_comp` must be a whole number larger than or equal to 0, not the number -4.

---

    Code
      recipe(~., data = mtcars) %>% step_umap(neighbors = -4) %>% prep()
    Condition
      Error in `step_umap()`:
      Caused by error in `prep()`:
      ! `neighbors` must be a whole number larger than or equal to 0, not the number -4.

---

    Code
      recipe(~., data = mtcars) %>% step_umap(min_dist = TRUE) %>% prep()
    Condition
      Error in `step_umap()`:
      Caused by error in `prep()`:
      ! `min_dist` must be a number, not `TRUE`.

---

    Code
      recipe(~., data = mtcars) %>% step_umap(learn_rate = -4) %>% prep()
    Condition
      Error in `step_umap()`:
      Caused by error in `prep()`:
      ! `learn_rate` must be a number larger than or equal to 0, not the number -4.

---

    Code
      recipe(~., data = mtcars) %>% step_umap(epochs = -4) %>% prep()
    Condition
      Error in `step_umap()`:
      Caused by error in `prep()`:
      ! `epochs` must be a whole number larger than or equal to 0 or `NULL`, not the number -4.

---

    Code
      recipe(~., data = mtcars) %>% step_umap(initial = "wrong") %>% prep()
    Condition
      Error in `step_umap()`:
      Caused by error in `prep()`:
      ! `initial` must be one of "spectral", "normlaplacian", "random", "lvrandom", "laplacian", "pca", "spca", or "agspectral", not "wrong".

---

    Code
      recipe(~., data = mtcars) %>% step_umap(target_weight = -4) %>% prep()
    Condition
      Error in `step_umap()`:
      Caused by error in `prep()`:
      ! `target_weight` must be a number between 0 and 1, not the number -4.

---

    Code
      recipe(~., data = mtcars) %>% step_umap(prefix = NULL)
    Condition
      Error in `step_umap()`:
      ! `prefix` must be a single string, not `NULL`.

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

