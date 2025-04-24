# Stops noisy tensorflow messages
withr::local_envvar(TF_CPP_MIN_LOG_LEVEL = "2")

test_that("factor encoded predictor", {
  skip_on_cran()
  skip_if_not_installed("keras3")
  skip_if(!embed:::is_tf_available())

  class_test <- recipe(x2 ~ ., data = ex_dat) |>
    step_embed(
      x3,
      outcome = vars(x2),
      options = embed_control(verbose = 0),
      id = "id"
    ) |>
    prep(training = ex_dat, retain = TRUE)
  tr_values <- bake(class_test, new_data = NULL, contains("embed"))
  new_values <- bake(class_test, new_data = new_dat, contains("embed"))
  expect_snapshot(
    new_values_ch <- bake(class_test, new_data = new_dat_ch, contains("embed"))
  )

  key <- class_test$steps[[1]]$mapping
  td_obj <- tidy(class_test, number = 1)

  expect_equal("x3", names(key))

  expect_equal(
    length(unique(ex_dat$x3)) + 1,
    nrow(key$x3)
  )
  expect_equal(3, ncol(key$x3))

  expect_true(sum(key$x3$..level == "..new") == 1)

  expect_true(all(vapply(tr_values, is.numeric, logical(1))))

  expect_equal(
    new_values[1, ] |> setNames(letters[1:2]),
    key$x3[key$x3$..level == "..new", -3] |> setNames(letters[1:2]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values[2, ] |> setNames(letters[1:2]),
    key$x3[key$x3$..level == levels(ex_dat$x3)[1], -3] |>
      setNames(letters[1:2]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values[3, ] |> setNames(letters[1:2]),
    key$x3[key$x3$..level == "..new", -3] |> setNames(letters[1:2]),
    ignore_attr = TRUE
  )

  expect_equal(
    new_values_ch[1, ] |> setNames(letters[1:2]),
    key$x3[key$x3$..level == "..new", -3] |> setNames(letters[1:2]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values_ch[2, ] |> setNames(letters[1:2]),
    key$x3[key$x3$..level == levels(ex_dat$x3)[1], -3] |>
      setNames(letters[1:2]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values_ch[3, ] |> setNames(letters[1:2]),
    key$x3[key$x3$..level == "..new", -3] |> setNames(letters[1:2]),
    ignore_attr = TRUE
  )

  expect_equal(
    td_obj$level,
    key$x3$..level
  )
  expect_equal(
    td_obj |> select(contains("emb")) |> setNames(letters[1:2]),
    key$x3 |> select(contains("emb")) |> setNames(letters[1:2])
  )
})

test_that("character encoded predictor", {
  skip_on_cran()
  skip_if_not_installed("keras3")
  skip_if(!embed:::is_tf_available())

  class_test <- recipe(x2 ~ ., data = ex_dat_ch) |>
    step_embed(x3, outcome = vars(x2), options = embed_control(verbose = 0)) |>
    prep(training = ex_dat_ch, retain = TRUE)
  tr_values <- bake(class_test, new_data = NULL, contains("embed"))
  new_values <- bake(class_test, new_data = new_dat, contains("embed"))
  new_values_fc <- bake(class_test, new_data = new_dat, contains("embed"))

  key <- class_test$steps[[1]]$mapping
  td_obj <- tidy(class_test, number = 1)

  expect_equal("x3", names(key))

  expect_equal(
    length(unique(ex_dat$x3)) + 1,
    nrow(key$x3)
  )
  expect_equal(3, ncol(key$x3))

  expect_true(sum(key$x3$..level == "..new") == 1)

  expect_true(all(vapply(tr_values, is.numeric, logical(1))))

  expect_equal(
    new_values[1, ] |> setNames(letters[1:2]),
    key$x3[key$x3$..level == "..new", -3] |> setNames(letters[1:2]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values[2, ] |> setNames(letters[1:2]),
    key$x3[key$x3$..level == levels(ex_dat$x3)[1], -3] |>
      setNames(letters[1:2]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values[3, ] |> setNames(letters[1:2]),
    key$x3[key$x3$..level == "..new", -3] |> setNames(letters[1:2]),
    ignore_attr = TRUE
  )

  expect_equal(
    new_values_fc[1, ] |> setNames(letters[1:2]),
    key$x3[key$x3$..level == "..new", -3] |> setNames(letters[1:2]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values_fc[2, ] |> setNames(letters[1:2]),
    key$x3[key$x3$..level == levels(ex_dat$x3)[1], -3] |>
      setNames(letters[1:2]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values_fc[3, ] |> setNames(letters[1:2]),
    key$x3[key$x3$..level == "..new", -3] |> setNames(letters[1:2]),
    ignore_attr = TRUE
  )

  expect_equal(
    td_obj$level,
    key$x3$..level
  )
  expect_equal(
    td_obj |> select(contains("emb")) |> setNames(letters[1:2]),
    key$x3 |> select(contains("emb")) |> setNames(letters[1:2])
  )
})

test_that("factor encoded predictor", {
  skip_on_cran()
  skip_if_not_installed("keras3")
  skip_if(!embed:::is_tf_available())

  class_test <- recipe(x1 ~ ., data = ex_dat) |>
    step_embed(x3, outcome = vars(x1), options = embed_control(verbose = 0)) |>
    prep(training = ex_dat, retain = TRUE)
  tr_values <- bake(class_test, new_data = NULL, contains("embed"))
  new_values <- bake(class_test, new_data = new_dat, contains("embed"))
  expect_snapshot(
    new_values_ch <- bake(class_test, new_data = new_dat_ch, contains("embed"))
  )
  key <- class_test$steps[[1]]$mapping
  td_obj <- tidy(class_test, number = 1)

  expect_equal("x3", names(key))

  expect_equal(
    length(unique(ex_dat$x3)) + 1,
    nrow(key$x3)
  )
  expect_equal(3, ncol(key$x3))

  expect_true(sum(key$x3$..level == "..new") == 1)

  expect_true(all(vapply(tr_values, is.numeric, logical(1))))

  expect_equal(
    new_values[1, ] |> setNames(letters[1:2]),
    key$x3[key$x3$..level == "..new", -3] |> setNames(letters[1:2]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values[2, ] |> setNames(letters[1:2]),
    key$x3[key$x3$..level == levels(ex_dat$x3)[1], -3] |>
      setNames(letters[1:2]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values[3, ] |> setNames(letters[1:2]),
    key$x3[key$x3$..level == "..new", -3] |> setNames(letters[1:2]),
    ignore_attr = TRUE
  )

  expect_equal(
    new_values_ch[1, ] |> setNames(letters[1:2]),
    key$x3[key$x3$..level == "..new", -3] |> setNames(letters[1:2]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values_ch[2, ] |> setNames(letters[1:2]),
    key$x3[key$x3$..level == levels(ex_dat$x3)[1], -3] |>
      setNames(letters[1:2]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values_ch[3, ] |> setNames(letters[1:2]),
    key$x3[key$x3$..level == "..new", -3] |> setNames(letters[1:2]),
    ignore_attr = TRUE
  )

  expect_equal(
    td_obj$level,
    key$x3$..level
  )
  expect_equal(
    td_obj |> select(contains("emb")) |> setNames(letters[1:2]),
    key$x3 |> select(contains("emb")) |> setNames(letters[1:2])
  )
})

test_that("character encoded predictor", {
  skip_on_cran()
  skip_if_not_installed("keras3")
  skip_if(!embed:::is_tf_available())

  class_test <- recipe(x1 ~ ., data = ex_dat_ch) |>
    step_embed(
      x3,
      outcome = vars(x1),
      num_terms = 5,
      options = embed_control(verbose = 0)
    ) |>
    prep(training = ex_dat_ch, retain = TRUE)
  tr_values <- bake(class_test, new_data = NULL, contains("embed"))
  new_values <- bake(class_test, new_data = new_dat, contains("embed"))
  new_values_fc <- bake(class_test, new_data = new_dat, contains("embed"))

  key <- class_test$steps[[1]]$mapping
  td_obj <- tidy(class_test, number = 1)

  expect_equal("x3", names(key))

  expect_equal(
    length(unique(ex_dat$x3)) + 1,
    nrow(key$x3)
  )
  expect_equal(6, ncol(key$x3))

  expect_true(sum(key$x3$..level == "..new") == 1)

  expect_true(all(vapply(tr_values, is.numeric, logical(1))))

  expect_equal(
    new_values[1, ] |> setNames(letters[1:5]),
    key$x3[key$x3$..level == "..new", -6] |> setNames(letters[1:5]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values[2, ] |> setNames(letters[1:5]),
    key$x3[key$x3$..level == levels(ex_dat$x3)[1], -6] |>
      setNames(letters[1:5]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values[3, ] |> setNames(letters[1:5]),
    key$x3[key$x3$..level == "..new", -6] |> setNames(letters[1:5]),
    ignore_attr = TRUE
  )

  expect_equal(
    new_values_fc[1, ] |> setNames(letters[1:5]),
    key$x3[key$x3$..level == "..new", -6] |> setNames(letters[1:5]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values_fc[2, ] |> setNames(letters[1:5]),
    key$x3[key$x3$..level == levels(ex_dat$x3)[1], -6] |>
      setNames(letters[1:5]),
    ignore_attr = TRUE
  )
  expect_equal(
    new_values_fc[3, ] |> setNames(letters[1:5]),
    key$x3[key$x3$..level == "..new", -6] |> setNames(letters[1:5]),
    ignore_attr = TRUE
  )

  expect_equal(
    td_obj$level,
    key$x3$..level
  )
  expect_equal(
    td_obj |> select(contains("emb")) |> setNames(letters[1:5]),
    key$x3 |> select(contains("emb")) |> setNames(letters[1:5])
  )
})

test_that("check_name() is used", {
  skip_on_cran()
  skip_if_not_installed("keras3")
  skip_if(!embed:::is_tf_available())

  dat <- ex_dat
  dat$x3_embed_1 <- dat$x3

  rec <- recipe(~., data = dat) |>
    step_embed(x3, outcome = vars(x2), options = embed_control(verbose = 0))

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("tunable", {
  rec <-
    recipe(~., data = mtcars) |>
    step_embed(all_predictors(), outcome = "mpg")
  rec_param <- tunable.step_embed(rec$steps[[1]])
  expect_equal(rec_param$name, c("num_terms", "hidden_units"))
  expect_true(all(rec_param$source == "recipe"))
  expect_true(is.list(rec_param$call_info))
  expect_equal(nrow(rec_param), 2)
  expect_equal(
    names(rec_param),
    c("name", "call_info", "source", "component", "component_id")
  )
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_embed(outcome = vars(mpg), num_terms = -4) |>
      prep()
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_embed(outcome = vars(mpg), hidden_units = -4) |>
      prep()
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_on_cran()
  skip_if_not_installed("keras3")
  skip_if(!embed:::is_tf_available())
  rec <- recipe(x2 ~ ., data = ex_dat) |>
    step_embed(
      x3,
      outcome = vars(x2),
      options = embed_control(verbose = 0),
      id = "id"
    ) |>
    update_role(x3, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(rec_trained, new_data = ex_dat[, -3])
  )
})

test_that("empty printing", {
  skip_on_cran()
  skip_if_not_installed("keras3")
  skip_if(!embed:::is_tf_available())

  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_embed(rec, outcome = vars(mpg))

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_embed(rec1, outcome = vars(mpg))

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_embed(rec, outcome = vars(mpg))

  expect <- tibble(
    terms = character(),
    level = character(),
    value = double(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("keep_original_cols works", {
  skip_on_cran()
  skip_if_not_installed("keras3")
  skip_if(!embed:::is_tf_available())

  new_names <- c("x2", "x3_embed_1", "x3_embed_2")

  rec <- recipe(x2 ~ x3, data = ex_dat) |>
    step_embed(
      x3,
      outcome = vars(x2),
      options = embed_control(verbose = 0),
      keep_original_cols = FALSE
    )

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    new_names
  )

  rec <- recipe(x2 ~ x3, data = ex_dat) |>
    step_embed(
      x3,
      outcome = vars(x2),
      options = embed_control(verbose = 0),
      keep_original_cols = TRUE
    )

  rec <- prep(rec)
  res <- bake(rec, new_data = NULL)

  expect_equal(
    colnames(res),
    c("x3", new_names)
  )
})

test_that("keep_original_cols - can prep recipes with it missing", {
  skip_on_cran()
  skip_if_not_installed("keras3")
  skip_if(!embed:::is_tf_available())

  rec <- recipe(x2 ~ x3, data = ex_dat) |>
    step_embed(x3, outcome = vars(x2), options = embed_control(verbose = 0))

  rec$steps[[1]]$keep_original_cols <- NULL

  expect_snapshot(
    rec <- prep(rec)
  )

  expect_no_error(
    bake(rec, new_data = ex_dat)
  )
})

test_that("printing", {
  skip_on_cran()
  skip_if_not_installed("keras3")
  skip_if(!embed:::is_tf_available())

  rec <- recipe(x2 ~ ., data = ex_dat_ch) |>
    step_embed(x3, outcome = vars(x2))

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})

test_that("tunable is setup to works with extract_parameter_set_dials", {
  skip_if_not_installed("dials")
  rec <- recipe(~., data = mtcars) |>
    step_embed(
      all_predictors(),
      outcome = "mpg",
      num_terms = hardhat::tune(),
      hidden_units = hardhat::tune()
    )

  params <- extract_parameter_set_dials(rec)

  expect_s3_class(params, "parameters")
  expect_identical(nrow(params), 2L)
})
