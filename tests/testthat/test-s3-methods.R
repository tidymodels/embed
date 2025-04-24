rlang::local_options(lifecycle_verbosity = "quiet")

r1 <- recipe(mpg ~ ., data = mtcars)
r2 <- r1 |> step_lencode_bayes(wt, outcome = vars(mpg))
r3 <- r1 |> step_discretize_cart(disp, outcome = vars(mpg))
r4 <- r1 |> step_discretize_xgb(disp, outcome = vars(mpg))
r5 <- r1 |> step_umap(disp, outcome = vars(mpg))

test_that("required packages", {
  expect_equal(required_pkgs(r1), "recipes")
  expect_equal(required_pkgs(r2), c("recipes", "rstanarm", "embed"))
  expect_equal(required_pkgs(r3), c("recipes", "rpart", "embed"))
  expect_equal(required_pkgs(r4), c("recipes", "xgboost", "embed"))
  expect_equal(required_pkgs(r5), c("recipes", "uwot", "embed"))
})
