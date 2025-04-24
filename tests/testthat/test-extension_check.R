test_that("recipes_extension_check", {
  expect_snapshot(
    recipes::recipes_extension_check(
      exclude_steps = "step_feature_hash",
      pkg = "embed"
    )
  )
})
