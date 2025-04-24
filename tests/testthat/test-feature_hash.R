test_that("step is deprecated", {
  expect_snapshot(
    error = TRUE,
    recipe |>
      step_feature_hash()
  )
})
