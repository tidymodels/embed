set.seed(1)
df <- data.frame(
  x1 = sample(c("A", "B", "C"), size = 20, replace = TRUE) |> factor(),
  x2 = sample(c("A", "B", "C"), size = 20, replace = TRUE),
  stringsAsFactors = TRUE
) |>
  mutate(
    y = rbinom(
      20,
      1,
      prob = 1 / (1 + exp(-1 * (-4 + as.numeric(x1) + as.numeric(x2))))
    )
  ) |>
  mutate(y = if_else(y == 1, "A", "B"))

# woe_table ---------------------------------------------------------------

test_that("woe_table do not accept different length inputs", {
  expect_snapshot(
    error = TRUE,
    embed:::woe_table(rep(c(0, 1), 20), rep(letters[1:4], 5))
  )
})

test_that("woe_table accepts only outcome with 2 distinct categories", {
  expect_snapshot(
    error = TRUE,
    embed:::woe_table(rep(letters[1:3], 10), rep(c(0, 1, 2), 10))
  )
  expect_snapshot(
    error = TRUE,
    embed:::woe_table(rep(letters[1:3], 10), rep(c(0), 30))
  )
  expect_snapshot(
    error = TRUE,
    embed:::woe_table(df$x2, df$x1)
  )
})

test_that("woe_table returns a proper tibble", {
  expect_equal(dim(embed:::woe_table(df$x1, df$y)), c(3, 7))
  expect_identical(
    names(embed:::woe_table(df$x1, df$y)),
    c("predictor", "n_tot", "n_A", "n_B", "p_A", "p_B", "woe")
  )
})

test_that("logical outcome variables are treated properly", {
  expect_equal(
    dim(
      embed:::woe_table(c("A", "A", "A", "B"), c(TRUE, FALSE, TRUE, FALSE))
    ),
    c(2, 7)
  )
})

test_that("logical predictor variable are treated properly", {
  expect_equal(
    class(
      embed:::woe_table(
        c(TRUE, FALSE, TRUE, FALSE),
        c("A", "A", "A", "B")
      )$predictor
    ),
    "character"
  )
})

test_that("woe_table ruturns no messages nor warnings", {
  expect_silent(embed:::woe_table(
    c(TRUE, FALSE, TRUE, FALSE),
    c("A", "A", "A", "B")
  ))
  expect_silent(embed:::woe_table(
    c(TRUE, FALSE, TRUE, FALSE, NA),
    c("A", "A", "A", "B", "B")
  ))
  expect_silent(embed:::woe_table(
    as.factor(c(TRUE, FALSE, TRUE, FALSE, NA)),
    c("A", "A", "A", "B", "B")
  ))
  expect_silent(embed:::woe_table(df$x1, df$y))
})

test_that("Laplace works", {
  expect_true(all(is.finite(
    embed:::woe_table(c("A", "A", "B", "B"), c(0, 0, 0, 1), Laplace = 1e-6)$woe
  )))
  expect_false(all(is.finite(
    embed:::woe_table(c("A", "A", "B", "B"), c(0, 0, 0, 1), Laplace = 0)$woe
  )))
})

# dictionary --------------------------------------------------------------

test_that("dictionary returns a proper tibble", {
  expect_equal(
    dictionary(df, "y") |> class(),
    c("tbl_df", "tbl", "data.frame")
  )
  expect_equal(dictionary(df, "y") |> dim(), c(6, 9))
  expect_identical(
    dictionary(df, "y") |> names(),
    c(
      "variable",
      "predictor",
      "n_tot",
      "n_A",
      "n_B",
      "p_A",
      "p_B",
      "woe",
      "outcome"
    )
  )
})

test_that("dictionary accepts numeric, logical and character predictor variables", {
  tmp <- mutate(df, x3 = rep(c(TRUE, FALSE), 10), x4 = rep(c(20, 30), 10))
  expect_equal(
    dim(dictionary(tmp, "y")),
    c(10, 9)
  )
})

test_that("dictionary returns no messages nor warnings nor errors", {
  expect_silent(dictionary(df, "y", x1))
  expect_silent(dictionary(
    df |> mutate(x3 = rep(c(TRUE, FALSE), 10)),
    "y",
    x3
  ))
})

# add_woe -----------------------------------------------------------------

test_that("add_woe returns a proper tibble", {
  expect_equal(add_woe(df, "y") |> class(), c("tbl_df", "tbl", "data.frame"))
  expect_equal(add_woe(df, "y") |> dim(), c(20, 5))
  expect_identical(
    add_woe(df, "y") |> names(),
    c("x1", "x2", "y", "woe_x1", "woe_x2")
  )
})

test_that("add_woe accepts only outcome with 2 distinct categories", {
  expect_snapshot(error = TRUE, dictionary(df |> filter(y %in% "B"), "y"))
})

test_that("add_woe ruturns no messages nor warnings nor errors", {
  expect_silent(add_woe(df, "y", x1))
  expect_silent(add_woe(df |> mutate(x3 = rep(c(TRUE, FALSE), 10)), "y", x3))
})

test_that("add_woe accepts numeric, logical and character predictor variables", {
  expect_equal(
    add_woe(
      df |>
        mutate(
          x3 = rep(c(TRUE, FALSE), 10),
          x4 = rep(c(20, 30), 10)
        ),
      "y"
    ) |>
      dim(),
    c(20, 9)
  )
})

test_that("add_woe returns woe only for those variables that exists in both data and dictionary", {
  expect_equal(
    names(add_woe(df, "y", x2, dictionary = dictionary(df, "y", x1))),
    c("x1", "x2", "y")
  )
  expect_equal(
    names(add_woe(df, "y", x1, dictionary = dictionary(df, "y", x1))),
    c("x1", "x2", "y", "woe_x1")
  )
  expect_equal(
    names(add_woe(df, "y", dictionary = dictionary(df, "y", x1))),
    c("x1", "x2", "y", "woe_x1")
  )
  expect_equal(
    names(add_woe(df, "y", x1, x2, dictionary = dictionary(df, "y", x1))),
    c("x1", "x2", "y", "woe_x1")
  )
})

test_that("add_woe do not accept dictionary with unexpected layout", {
  expect_snapshot(
    error = TRUE,
    add_woe(df, outcome = "y", x1, dictionary = iris)
  )
  expect_snapshot(
    error = TRUE,
    add_woe(df, outcome = "y", x1, dictionary = iris |> mutate(variable = 1))
  )
})

# test_that("add_woe warns user if the variable has too many levels", {
#   expect_snapshot(credit_data |> add_woe("Status", Expenses))
# })

# step_woe ----------------------------------------------------------------

test_that("step_woe", {
  skip_if_not_installed("modeldata")
  data("credit_data", package = "modeldata")

  set.seed(342)
  in_training <- sample(seq_len(nrow(credit_data)), 2000)

  credit_tr <- credit_data[in_training, ]
  credit_te <- credit_data[-in_training, ]

  rec <-
    recipe(Status ~ ., data = credit_tr) |>
    step_woe(Job, Home, outcome = vars(Status))

  expect_snapshot(
    woe_models <- prep(rec, training = credit_tr)
  )

  woe_dict <- credit_tr |> dictionary("Status", Job, Home)
  expect_equal(woe_dict, woe_models$steps[[1]]$dictionary, ignore_attr = TRUE)

  bake_woe_output <- bake(woe_models, new_data = credit_te)
  add_woe_output <-
    credit_te |>
    add_woe("Status", Job, Home, dictionary = woe_dict) |>
    dplyr::select(one_of(names(bake_woe_output)))

  #
  expect_equal(bake_woe_output, add_woe_output)

  tidy_output <- tidy(woe_models, number = 1)
  woe_dict_output <-
    dictionary(credit_tr, Job, Home, outcome = "Status") |>
    dplyr::rename(terms = variable, value = predictor)

  #
  expect_equal(
    tidy_output |> dplyr::select(-id),
    woe_dict_output,
    ignore_attr = TRUE
  )

  rec_all_nominal <- recipe(Status ~ ., data = credit_tr) |>
    step_woe(all_nominal(), outcome = vars(Status))

  #
  expect_snapshot(prep(rec_all_nominal, training = credit_tr, verbose = TRUE))

  rec_all_numeric <- recipe(Status ~ ., data = credit_tr) |>
    step_woe(all_predictors(), outcome = vars(Status))

  #
  expect_snapshot(
    error = TRUE,
    prep(rec_all_numeric, training = credit_tr)
  )

  rec_discretize <- recipe(Status ~ ., data = credit_tr) |>
    step_discretize(Price)
  rec_discretize_woe <- rec_discretize |>
    step_woe(Price, outcome = vars(Status))

  prep_discretize <- prep(rec_discretize, training = credit_tr)
  prep_discretize_woe <- prep(rec_discretize_woe, training = credit_tr)

  bake_discretize <- bake(prep_discretize, new_data = credit_te)
  bake_discretize_woe <- bake(prep_discretize_woe, new_data = credit_te)

  expect_equal(
    sort(as.character(unique(bake_discretize$Price))),
    sort(prep_discretize_woe$steps[[2]]$dictionary$predictor)
  )
})

test_that("2-level factors", {
  iris3 <- iris
  iris3$group <- factor(rep(letters[1:5], each = 30))

  expect_snapshot(
    error = TRUE,
    recipe(Species ~ ., data = iris3) |>
      step_woe(group, outcome = vars(Species)) |>
      prep()
  )
})

test_that("woe_table respects factor levels", {
  dat <- tibble(
    predictor = sample(0:1, 100, TRUE),
    target = factor(predictor == 0, levels = c(TRUE, FALSE), labels = 0:1),
    target0 = relevel(target, ref = "0"),
    target1 = relevel(target, ref = "1")
  )

  expect_equal(
    woe_table(dat$predictor, dat$target0)$woe,
    -woe_table(dat$predictor, dat$target1)$woe
  )

  expect_identical(
    woe_table(dat$predictor, dat$target0) |> select(-woe),
    woe_table(dat$predictor, dat$target1) |> select(-woe)
  )
})

test_that("tunable", {
  rec <-
    recipe(~., data = mtcars) |>
    step_woe(all_predictors(), outcome = "mpg")
  rec_param <- tunable.step_woe(rec$steps[[1]])
  expect_equal(rec_param$name, "Laplace")
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 1)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_woe(outcome = vars(mpg), Laplace = NULL) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_woe(outcome = vars(mpg), prefix = NULL)
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_if_not_installed("modeldata")
  data("credit_data", package = "modeldata")

  rec <- recipe(credit_data) |>
    step_woe(Job, Home, outcome = vars(Status)) |>
    update_role(Job, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  suppressWarnings(
    rec_trained <- prep(rec, training = credit_data, verbose = FALSE)
  )

  expect_snapshot(
    error = TRUE,
    bake(rec_trained, new_data = credit_data[, -8])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_woe(rec, outcome = vars(mpg))

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_woe(rec1, outcome = vars(mpg))

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_woe(rec, outcome = vars(mpg))

  expect <- res <- tibble(
    terms = character(),
    value = character(),
    n_tot = integer(),
    n_bad = integer(),
    n_good = integer(),
    p_bad = double(),
    p_good = double(),
    woe = double(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  skip_if_not_installed("modeldata")
  data("ames", package = "modeldata")

  new_names <- c("Street", "woe_Alley")

  rec <- recipe(Street ~ Alley, data = ames) |>
    step_woe(Alley, outcome = vars(Street), keep_original_cols = FALSE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(Street ~ Alley, data = ames) |>
    step_woe(Alley, outcome = vars(Street), keep_original_cols = TRUE)

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("Alley", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  skip_if_not_installed("modeldata")
  data("ames", package = "modeldata")

  rec <- recipe(Street ~ Alley, data = ames) |>
    step_woe(Alley, outcome = vars(Street), keep_original_cols = FALSE)

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = ames)
  )
})

test_that("printing", {
  skip_if_not_installed("modeldata")
  data("credit_data", package = "modeldata")

  rec <- recipe(Status ~ ., data = credit_data) |>
    step_woe(Job, Home, outcome = vars(Status))

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to works with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) |>
    step_woe(
      all_predictors(),
      outcome = "mpg",
      Laplace = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 1L)
})
