test_that("collapsing factors", {
  skip_if_not_installed("stringdist")
  skip_if_not_installed("modeldata")

  data(ames, package = "modeldata")

  expect_no_error(
    {
      rec_1 <-
        recipe(Sale_Price ~ ., data = ames) |>
        step_collapse_stringdist(MS_SubClass, distance = 5) |>
        prep()
    }
  )

  expect_true(length(rec_1$steps[[1]]$results) == 1)
  expect_equal(names(rec_1$steps[[1]]$results), "MS_SubClass")

  expect_true(
    length(unique(rec_1$steps[[1]]$results$Neighborhood$.group)) <
      length(levels(ames$Neighborhood))
  )

  expect_equal(
    ames |> select(-MS_SubClass, -Sale_Price),
    bake(rec_1, new_data = NULL) |> select(-MS_SubClass, -Sale_Price)
  )

  expect_false(
    isTRUE(
      all.equal(bake(rec_1, new_data = NULL)$MS_SubClass, ames$MS_SubClass)
    )
  )

  expect_no_error(
    {
      rec_2 <-
        recipe(Sale_Price ~ ., data = ames) |>
        step_collapse_stringdist(MS_SubClass, Overall_Cond, distance = 10) |>
        prep()
    }
  )

  expect_true(length(rec_2$steps[[1]]$results) == 2)
  expect_equal(
    names(rec_2$steps[[1]]$results),
    c("MS_SubClass", "Overall_Cond")
  )

  expect_true(
    length(rec_2$steps[[1]]$results$MS_SubClass) <
      length(rec_1$steps[[1]]$results$MS_SubClass)
  )
})

test_that("collapsing factors manual test", {
  skip_if_not_installed("stringdist")

  data0 <- tibble(
    x1 = c("a", "b", "d", "e", "aaaaaa", "bbbbbb"),
    x2 = c("ak", "b", "djj", "e", "aaaaaa", "aaaaaa")
  )

  rec <- recipe(~., data = data0) |>
    step_collapse_stringdist(all_predictors(), distance = 1) |>
    prep()

  exp_result <- tibble(
    x1 = factor(c("a", "a", "a", "a", "aaaaaa", "bbbbbb")),
    x2 = factor(c("ak", "b", "djj", "b", "aaaaaa", "aaaaaa"))
  )
  expect_equal(
    bake(rec, new_data = NULL),
    exp_result
  )

  rec <- recipe(~., data = data0) |>
    step_collapse_stringdist(all_predictors(), distance = 2) |>
    prep()

  exp_result <- tibble(
    x1 = factor(c("a", "a", "a", "a", "aaaaaa", "bbbbbb")),
    x2 = factor(c("ak", "ak", "djj", "ak", "aaaaaa", "aaaaaa"))
  )
  expect_equal(
    bake(rec, new_data = NULL),
    exp_result
  )
})

test_that("method argument", {
  skip_if_not_installed("stringdist")

  data0 <- tibble(
    x1 = c("a", "b", "d", "e", "aaaaaa", "bbbbbb"),
    x2 = c("ak", "b", "djj", "e", "aaaaaa", "aaaaaa")
  )

  rec <- recipe(~., data = data0) |>
    step_collapse_stringdist(
      all_predictors(),
      distance = 0.5,
      method = "cosine"
    ) |>
    prep()

  exp_result <- tibble(
    x1 = factor(c("a", "b", "d", "e", "a", "b")),
    x2 = factor(c("aaaaaa", "b", "djj", "e", "aaaaaa", "aaaaaa"))
  )
  expect_equal(
    bake(rec, new_data = NULL),
    exp_result
  )

  rec <- recipe(~., data = data0) |>
    step_collapse_stringdist(
      all_predictors(),
      distance = 1,
      method = "cosine"
    ) |>
    prep()

  exp_result <- tibble(
    x1 = factor(c("a", "a", "a", "a", "a", "a")),
    x2 = factor(c("aaaaaa", "aaaaaa", "aaaaaa", "aaaaaa", "aaaaaa", "aaaaaa"))
  )
  expect_equal(
    bake(rec, new_data = NULL),
    exp_result
  )
})

test_that("options argument", {
  skip_if_not_installed("stringdist")

  data0 <- tibble(
    x1 = c("a", "b", "d", "e", "aaaaaa", "bbbbbb"),
    x2 = c("ak", "b", "djj", "e", "aaaaaa", "aaaaaa")
  )

  rec <- recipe(~., data = data0) |>
    step_collapse_stringdist(
      all_predictors(),
      distance = 1,
      options = list(weight = c(d = 0.1, i = 1, s = 1, t = 1))
    ) |>
    prep()

  exp_result <- tibble(
    x1 = factor(c("a", "a", "a", "a", "a", "b")),
    x2 = factor(c("ak", "b", "djj", "b", "aaaaaa", "aaaaaa"))
  )
  expect_equal(
    bake(rec, new_data = NULL),
    exp_result
  )
})


test_that("failed collapsing", {
  skip_if_not_installed("stringdist")
  skip_if_not_installed("modeldata")

  data(ames, package = "modeldata")

  # too many splits
  expect_no_error(
    {
      rec_4 <-
        recipe(Sale_Price ~ ., data = ames) |>
        step_collapse_stringdist(MS_SubClass, distance = 0) |>
        prep()
    }
  )

  expect_equal(
    length(rec_4$steps[[1]]$results$MS_SubClass),
    length(levels(ames$MS_SubClass))
  )

  # too few splits
  expect_no_error(
    {
      rec_5 <-
        recipe(Sale_Price ~ ., data = ames) |>
        step_collapse_stringdist(MS_SubClass, distance = 10000) |>
        prep()
    }
  )

  expect_equal(
    length(rec_5$steps[[1]]$results$MS_SubClass),
    1
  )
})

test_that("bad args", {
  skip_if_not_installed("stringdist")

  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_collapse_stringdist(cost_complexity = -4)
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_collapse_stringdist(min_n = -4)
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_if_not_installed("stringdist")
  skip_if_not_installed("modeldata")

  data(ames, package = "modeldata")

  rec <- recipe(Sale_Price ~ ., data = ames) |>
    step_collapse_stringdist(MS_SubClass, distance = 2) |>
    update_role(MS_SubClass, new_role = "potato") |>
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = ames, verbose = FALSE)

  expect_snapshot(
    error = TRUE,
    bake(rec_trained, new_data = ames[, -1])
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_collapse_stringdist(rec, distance = 1)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_collapse_stringdist(rec1, distance = 1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_collapse_stringdist(rec, distance = 1)

  expect <- tibble(terms = character(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  skip_if_not_installed("stringdist")
  skip_if_not_installed("modeldata")

  data(ames, package = "modeldata")

  rec <- recipe(Sale_Price ~ ., data = ames) |>
    step_collapse_stringdist(MS_SubClass, distance = 5)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
