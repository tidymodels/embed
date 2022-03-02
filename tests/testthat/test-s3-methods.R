r1 <- recipe(mpg ~ ., data = mtcars)
r2 <- r1 %>% step_lencode_bayes(wt, outcome = vars(mpg))
r3 <- r1 %>% step_discretize_cart(disp, outcome = vars(mpg))
r4 <- r1 %>% step_discretize_xgb(disp, outcome = vars(mpg))
r5 <- r1 %>% step_umap(disp, outcome = vars(mpg))
r6 <- r1 %>% step_feature_hash(disp)

nothing <-
  tibble::tibble(
    name = character(0),
    call_info = list(),
    source = character(0),
    component = character(0),
    component_id = character(0)
  )

# ------------------------------------------------------------------------------

test_that("required packages", {
  expect_equal(required_pkgs(r1), "recipes")
  expect_equal(required_pkgs(r2), c("recipes", "rstanarm", "embed"))
  expect_equal(required_pkgs(r3), c("recipes", "rpart", "embed"))
  expect_equal(required_pkgs(r4), c("recipes", "xgboost", "embed"))
  expect_equal(required_pkgs(r5), c("recipes", "uwot", "embed"))
  expect_equal(required_pkgs(r6), c("recipes", "keras", "embed"))
})

test_that("tunable arguments", {
  expect_equal(tunable(r1), nothing)
  expect_equal(tunable(r2), nothing)
  expect_equal(
    tunable(r3)$name,
    c("cost_complexity", "tree_depth", "min_n")
  )
  expect_equal(
    tunable(r4)$name,
    c("sample_val", "learn_rate", "num_breaks", "tree_depth", "min_n")
  )
  expect_equal(
    tunable(r5)$name,
    c("num_comp", "neighbors", "min_dist", "learn_rate", "epochs")
  )
  expect_equal(tunable(r6), nothing)
})
