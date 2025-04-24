test_that("collapsing factors", {
  skip_if_not_installed("modeldata")
  data(ames, package = "modeldata")
  ames$Sale_Price <- log10(ames$Sale_Price)

  expect_no_error(
    {
      rec_1 <-
        recipe(Sale_Price ~ ., data = ames) |>
        step_collapse_cart(
          Neighborhood,
          Central_Air,
          outcome = vars(Sale_Price)
        ) |>
        prep()
    }
  )

  expect_true(length(rec_1$steps[[1]]$results) == 1)
  expect_equal(names(rec_1$steps[[1]]$results), "Neighborhood")

  expect_true(
    length(unique(rec_1$steps[[1]]$results$Neighborhood$.group)) <
      length(levels(ames$Neighborhood))
  )

  expect_equal(
    ames |> select(-Neighborhood, -Sale_Price),
    bake(rec_1, new_data = NULL) |> select(-Neighborhood, -Sale_Price)
  )

  expect_false(
    isTRUE(
      all.equal(bake(rec_1, new_data = NULL)$Neighborhood, ames$Neighborhood)
    )
  )

  expect_no_error(
    {
      rec_2 <-
        recipe(Sale_Price ~ ., data = ames) |>
        step_collapse_cart(
          Neighborhood,
          Central_Air,
          outcome = vars(Sale_Price),
          min_n = 100,
          cost_complexity = 0.1
        ) |>
        prep()
    }
  )

  expect_true(
    length(levels(rec_2$steps[[1]]$results$Neighborhood$.group)) <
      length(levels(rec_1$steps[[1]]$results$Neighborhood$.group))
  )
})

test_that("failed collapsing", {
  skip_if_not_installed("modeldata")
  data(ames, package = "modeldata")

  # model fails
  ames$Sale_Price2 <- Inf
  expect_no_error(
    {
      rec_3 <-
        recipe(Sale_Price2 ~ ., data = ames) |>
        step_collapse_cart(
          Neighborhood,
          Central_Air,
          outcome = vars(Sale_Price2)
        ) |>
        prep()
    }
  )

  expect_true(length(rec_3$steps[[1]]$results) == 0)

  # too many splits
  expect_no_error(
    {
      rec_4 <-
        recipe(Sale_Price ~ ., data = ames) |>
        step_collapse_cart(
          Neighborhood,
          outcome = vars(Sale_Price),
          cost_complexity = 0,
          min_n = 1
        ) |>
        prep()
    }
  )

  expect_true(length(rec_4$steps[[1]]$results) == 0)

  # too many splits
  expect_no_error(
    {
      rec_5 <-
        recipe(Sale_Price ~ ., data = ames) |>
        step_collapse_cart(Central_Air, outcome = vars(Sale_Price)) |>
        prep()
    }
  )

  expect_true(length(rec_5$steps[[1]]$results) == 0)
})

test_that("bad args", {
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_collapse_cart(cost_complexity = -4)
  )
  expect_snapshot(
    error = TRUE,
    recipe(~., data = mtcars) |>
      step_collapse_cart(min_n = -4)
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("bake method errors when needed non-standard role columns are missing", {
  skip_if_not_installed("modeldata")
  data(ames, package = "modeldata")

  rec <- recipe(Sale_Price ~ ., data = ames) |>
    step_collapse_cart(MS_SubClass, outcome = vars(Sale_Price)) |>
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
  rec <- step_collapse_cart(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_collapse_cart(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_collapse_cart(rec)

  expect <- tibble(terms = character(), value = double(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  skip_if_not_installed("modeldata")
  data(ames, package = "modeldata")

  rec <- recipe(Sale_Price ~ ., data = ames) |>
    step_collapse_cart(Neighborhood, Central_Air, outcome = vars(Sale_Price))

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
