test_that("factor outcome - factor predictor", {
  class_test <- recipe(x2 ~ ., data = ex_dat) |>
    step_lencode(x3, outcome = vars(x2), smooth = FALSE, id = "id") |>
    prep(training = ex_dat, retain = TRUE)
  tr_values <- bake(class_test, new_data = NULL)$x3
  new_values <- bake(class_test, new_data = new_dat)
  expect_snapshot(
    new_values_ch <- bake(class_test, new_data = new_dat_ch)
  )
  key <- class_test$steps[[1]]$mapping
  td_obj <- tidy(class_test, number = 1)

  expect_equal("x3", names(key))

  expect_equal(
    length(unique(ex_dat$x3)) + 1,
    nrow(key$x3)
  )
  expect_true(sum(key$x3$..level == "..new") == 1)

  expect_true(is.numeric(tr_values))

  expect_equal(
    new_values$x3[1],
    key$x3$..value[key$x3$..level == "..new"]
  )
  expect_equal(
    new_values$x3[2],
    key$x3$..value[key$x3$..level == levels(ex_dat$x3)[1]]
  )
  expect_equal(
    new_values$x3[3],
    key$x3$..value[key$x3$..level == "..new"]
  )
  expect_equal(
    new_values_ch$x3[1],
    key$x3$..value[key$x3$..level == "..new"]
  )
  expect_equal(
    new_values_ch$x3[2],
    key$x3$..value[key$x3$..level == levels(ex_dat$x3)[1]]
  )
  expect_equal(
    new_values_ch$x3[3],
    key$x3$..value[key$x3$..level == "..new"]
  )

  expect_equal(
    td_obj$level,
    key$x3$..level
  )
  expect_equal(
    td_obj$value,
    key$x3$..value
  )

  new_values
})

test_that("factor outcome - character predictor", {
  class_test <- recipe(x2 ~ ., data = ex_dat_ch) |>
    step_lencode(x3, outcome = vars(x2), smooth = FALSE) |>
    prep(training = ex_dat_ch, retain = TRUE)
  tr_values <- bake(class_test, new_data = NULL)$x3
  expect_snapshot(
    new_values <- bake(class_test, new_data = new_dat_ch)
  )
  new_values_fc <- bake(class_test, new_data = new_dat)
  key <- class_test$steps[[1]]$mapping
  td_obj <- tidy(class_test, number = 1)

  expect_equal("x3", names(key))

  expect_equal(
    length(unique(ex_dat$x3)) + 1,
    nrow(key$x3)
  )
  expect_true(sum(key$x3$..level == "..new") == 1)

  expect_true(is.numeric(tr_values))

  expect_equal(
    new_values$x3[1],
    key$x3$..value[key$x3$..level == "..new"]
  )
  expect_equal(
    new_values$x3[2],
    key$x3$..value[key$x3$..level == levels(ex_dat$x3)[1]]
  )
  expect_equal(
    new_values$x3[3],
    key$x3$..value[key$x3$..level == "..new"]
  )
  expect_equal(
    new_values_fc$x3[1],
    key$x3$..value[key$x3$..level == "..new"]
  )
  expect_equal(
    new_values_fc$x3[2],
    key$x3$..value[key$x3$..level == levels(ex_dat$x3)[1]]
  )
  expect_equal(
    new_values_fc$x3[3],
    key$x3$..value[key$x3$..level == "..new"]
  )

  expect_equal(
    td_obj$level,
    key$x3$..level
  )
  expect_equal(
    td_obj$value,
    key$x3$..value
  )

  unseen_level <- data.frame(
    x1 = 0,
    x2 = factor("a", levels = c("a", "b")),
    x3 = "unseen-level",
    x4 = factor("A", levels = c("A", "B", "C", "D", "E"))
  )

  expect_equal(
    bake(class_test, unseen_level)$x3,
    0
  )
})

test_that("numeric outcome - factor predictor", {
  reg_test <- recipe(x1 ~ ., data = ex_dat) |>
    step_lencode(x3, outcome = vars(x1)) |>
    prep(training = ex_dat, retain = TRUE)
  tr_values <- bake(reg_test, new_data = NULL)$x3
  new_values <- bake(reg_test, new_data = new_dat)
  expect_snapshot(
    new_values_ch <- bake(reg_test, new_data = new_dat_ch)
  )
  td_obj <- tidy(reg_test, number = 1)

  key <- reg_test$steps[[1]]$mapping

  expect_equal("x3", names(key))

  expect_equal(
    length(unique(ex_dat$x3)) + 1,
    nrow(key$x3)
  )
  expect_true(sum(key$x3$..level == "..new") == 1)

  expect_true(is.numeric(tr_values))

  expect_equal(
    new_values$x3[1],
    key$x3$..value[key$x3$..level == "..new"]
  )
  expect_equal(
    new_values$x3[2],
    key$x3$..value[key$x3$..level == levels(ex_dat$x3)[1]]
  )
  expect_equal(
    new_values$x3[3],
    key$x3$..value[key$x3$..level == "..new"]
  )
  expect_equal(
    new_values_ch$x3[1],
    key$x3$..value[key$x3$..level == "..new"]
  )
  expect_equal(
    new_values_ch$x3[2],
    key$x3$..value[key$x3$..level == levels(ex_dat$x3)[1]]
  )
  expect_equal(
    new_values_ch$x3[3],
    key$x3$..value[key$x3$..level == "..new"]
  )

  expect_equal(
    td_obj$level,
    key$x3$..level
  )
  expect_equal(
    td_obj$value,
    key$x3$..value
  )
})

test_that("numeric outcome - character predictor", {
  reg_test <- recipe(x1 ~ ., data = ex_dat_ch) |>
    step_lencode(x3, outcome = vars(x1)) |>
    prep(training = ex_dat_ch, retain = TRUE)
  tr_values <- bake(reg_test, new_data = NULL)$x3
  new_values <- bake(reg_test, new_data = new_dat_ch)
  new_values_fc <- bake(reg_test, new_data = new_dat)
  key <- reg_test$steps[[1]]$mapping
  td_obj <- tidy(reg_test, number = 1)

  expect_equal("x3", names(key))

  expect_equal(
    length(unique(ex_dat$x3)) + 1,
    nrow(key$x3)
  )
  expect_true(sum(key$x3$..level == "..new") == 1)

  expect_true(is.numeric(tr_values))

  expect_equal(
    new_values$x3[1],
    key$x3$..value[key$x3$..level == "..new"]
  )
  expect_equal(
    new_values$x3[2],
    key$x3$..value[key$x3$..level == levels(ex_dat$x3)[1]]
  )
  expect_equal(
    new_values$x3[3],
    key$x3$..value[key$x3$..level == "..new"]
  )
  expect_equal(
    new_values_fc$x3[1],
    key$x3$..value[key$x3$..level == "..new"]
  )
  expect_equal(
    new_values_fc$x3[2],
    key$x3$..value[key$x3$..level == levels(ex_dat$x3)[1]]
  )
  expect_equal(
    new_values_fc$x3[3],
    key$x3$..value[key$x3$..level == "..new"]
  )

  expect_equal(
    td_obj$level,
    key$x3$..level
  )
  expect_equal(
    td_obj$value,
    key$x3$..value
  )

  unseen_level <- data.frame(
    x1 = 0,
    x2 = factor("a", levels = c("a", "b")),
    x3 = "unseen-level",
    x4 = factor("A", levels = c("A", "B", "C", "D", "E"))
  )

  expect_equal(
    bake(reg_test, unseen_level)$x3,
    mean(ex_dat_ch$x1)
  )
})

test_that("non occurring events doesn't result in infinities", {
  data <- data.frame(
    outcome = c("a", "a", "b", "b"),
    predictor = c("a", "a", "a", "b")
  )

  res <- recipe(outcome ~ ., data = data) |>
    step_lencode(predictor, outcome = vars(outcome), smooth = FALSE) |>
    prep() |>
    tidy(1)

  exp <- c(
    log(2 / 3 / (1 - 2 / 3)),
    log(
      (2 * nrow(data) - 1) /
        (2 * nrow(data)) /
        (1 - (2 * nrow(data) - 1) / (2 * nrow(data)))
    ),
    log(0.5 / (1 - 0.5))
  )

  expect_identical(res$value, exp)
  expect_identical(res$level, c("a", "b", "..new"))
})

test_that("non occurring events doesn't result in infinities - case weights", {
  data <- data.frame(
    outcome = c("a", "a", "b", "b"),
    predictor = c("a", "a", "a", "b"),
    wts = importance_weights(rep(1, 4))
  )

  res <- recipe(outcome ~ ., data = data) |>
    step_lencode(predictor, outcome = vars(outcome), smooth = FALSE) |>
    prep() |>
    tidy(1)

  exp <- c(
    log(2 / 3 / (1 - 2 / 3)),
    log(
      (2 * nrow(data) - 1) /
        (2 * nrow(data)) /
        (1 - (2 * nrow(data) - 1) / (2 * nrow(data)))
    ),
    log(0.5 / (1 - 0.5))
  )

  expect_identical(res$value, exp)
  expect_identical(res$level, c("a", "b", "..new"))
})


test_that("bad args", {
  three_class <- iris
  three_class$fac <- rep(letters[1:3], 50)
  three_class$logical <- rep(c(TRUE, FALSE), 75)

  expect_snapshot(
    error = TRUE,
    recipe(Species ~ ., data = three_class) |>
      step_lencode(Sepal.Length, outcome = vars(Species)) |>
      prep(training = three_class, retain = TRUE)
  )

  expect_snapshot(
    error = TRUE,
    recipe(Species ~ ., data = three_class) |>
      step_lencode(Species, outcome = vars(logical)) |>
      prep(training = three_class, retain = TRUE)
  )
})

test_that("case weights", {
  wts_int <- rep(c(0.9, 1), times = c(100, 400))

  ex_dat_cw <- ex_dat |>
    mutate(wts = importance_weights(wts_int))

  class_test <- recipe(x2 ~ ., data = ex_dat_cw) |>
    step_lencode(x3, outcome = vars(x2), smooth = FALSE, id = "id") |>
    prep(training = ex_dat_cw, retain = TRUE)

  ref_mod <- glm(
    x2 ~ 0 + x3,
    data = ex_dat_cw,
    family = binomial,
    na.action = na.omit,
    weights = ex_dat_cw$wts
  )

  inf_estimate_p <- (2 * nrow(ex_dat_cw) - 1) / (2 * nrow(ex_dat_cw))
  inf_estimate_log_odds <- log(inf_estimate_p / (1 - inf_estimate_p))

  exp <- tibble(
    ..level = names(coef(ref_mod)),
    ..value = unname(coef(ref_mod))
  ) |>
    mutate(
      ..level = gsub("^x3", "", ..level),
      ..value = -..value,
      ..value = if_else(abs(..value) < 0.0001, 0, ..value),
      ..value = if_else(
        abs(round(..value, 0.4)) == max(abs(round(..value, 0.4))),
        inf_estimate_log_odds,
        ..value
      )
    ) |>
    arrange(..level)

  res <- slice_head(class_test$steps[[1]]$mapping$x3, n = -1) |>
    arrange(..level)

  expect_equal(
    res,
    exp,
    tolerance = 0.00001
  )

  expect_snapshot(class_test)
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(x2 ~ ., data = ex_dat) |>
    step_lencode(x3, outcome = vars(x2), smooth = FALSE) |>
    update_role(x3, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = ex_dat, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(rec_trained, new_data = ex_dat[, -3])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_lencode(rec, outcome = vars(mpg))

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_lencode(rec1, outcome = vars(mpg))

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_lencode(rec, outcome = vars(mpg))

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

test_that("printing", {
  rec <- recipe(x1 ~ ., data = ex_dat_ch) |>
    step_lencode(x3, outcome = vars(x1))

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
