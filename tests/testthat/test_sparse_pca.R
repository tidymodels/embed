source(testthat::test_path("test_helpers.R"))

# ------------------------------------------------------------------------------

data(cells, package = "modeldata")
cells$case <- cells$class <- NULL
cells <- as.data.frame(scale(cells))

split <- seq.int(1, 2019, by = 10)
tr <- cells[-split, ]
te <- cells[split, ]

# ------------------------------------------------------------------------------

test_that("step_pca_sparse_bayes", {
  skip_if_not_installed("VBsparsePCA")

  rec <-
    recipe(~., data = tr) %>%
    step_pca_sparse_bayes(
      all_predictors(),
      num_comp = 4,
      prior_slab_dispersion = 1 / 2,
      prior_mixture_threshold = 1 / 15
    ) %>%
    prep()

  direct_mod <- VBsparsePCA::VBsparsePCA(as.matrix(tr), lambda = 1 / 2, r = 4, threshold = 1 / 15)
  direct_coef <- svd(direct_mod$loadings)$u
  embed_coef <- rec$steps[[1]]$res
  vars <- rownames(embed_coef)
  dimnames(embed_coef) <- NULL

  expect_equal(abs(direct_coef), abs(embed_coef), tolerance = 0.1)

  tidy_coef <- tidy(rec, number = 1)
  # test a few values
  expect_equal(
    tidy_coef$value[tidy_coef$terms == "angle_ch_1" & tidy_coef$component == "PC1"],
    embed_coef[which(vars == "angle_ch_1"), 1]
  )

  expect_equal(
    tidy_coef$value[tidy_coef$terms == "total_inten_ch_3" & tidy_coef$component == "PC3"],
    embed_coef[which(vars == "total_inten_ch_3"), 3]
  )

  expect_snapshot(rec)
})


# ------------------------------------------------------------------------------

test_that("step_pca_sparse", {
  skip_if_not_installed("irlba")

  rec <-
    recipe(~., data = tr) %>%
    step_pca_sparse(
      all_predictors(),
      num_comp = 4,
      predictor_prop = 1 / 2
    ) %>%
    prep()

  direct_mod <- irlba::ssvd(as.matrix(tr), k = 4, n = ncol(tr) / 2)
  direct_coef <- direct_mod$v
  embed_coef <- rec$steps[[1]]$res
  vars <- rownames(embed_coef)
  dimnames(embed_coef) <- NULL
  dimnames(direct_coef) <- NULL

  expect_equal(abs(direct_coef), abs(embed_coef), tolerance = 0.1)

  tidy_coef <- tidy(rec, number = 1)
  # test a few values
  expect_equal(
    tidy_coef$value[tidy_coef$terms == "angle_ch_1" & tidy_coef$component == "PC1"],
    embed_coef[which(vars == "angle_ch_1"), 1]
  )

  expect_equal(
    tidy_coef$value[tidy_coef$terms == "total_inten_ch_3" & tidy_coef$component == "PC3"],
    embed_coef[which(vars == "total_inten_ch_3"), 3]
  )

  expect_snapshot(rec)
})

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(~., data = tr) %>%
    step_pca_sparse_bayes(
      avg_inten_ch_1, avg_inten_ch_2, avg_inten_ch_3, avg_inten_ch_4,
      num_comp = 2,
      prior_slab_dispersion = 1 / 2,
      prior_mixture_threshold = 1 / 15
    ) %>%
    update_role(avg_inten_ch_1, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = tr, verbose = FALSE)

  expect_error(
    bake(rec_trained, new_data = tr[, -3]),
    class = "new_data_missing_column"
  )
})

test_that("printing", {
  print_test <- recipe(~., data = tr[, -5]) %>%
    step_pca_sparse_bayes(all_predictors())
  expect_snapshot(print_test)
  expect_snapshot(prep(print_test))
})

# ------------------------------------------------------------------------------

test_that("empty selections", {
  data(ad_data, package = "modeldata")
  expect_error(
    rec <-
      recipe(Class ~ Genotype + tau, data = ad_data) %>%
      step_pca_sparse(starts_with("potato")) %>%
      prep(),
    regexp = NA
  )
  expect_equal(
    bake(rec, new_data = NULL),
    ad_data %>% select(Genotype, tau, Class)
  )

  expect_error(
    rec <-
      recipe(Class ~ Genotype + tau, data = ad_data) %>%
      step_pca_sparse_bayes(starts_with("potato")) %>%
      prep(),
    regexp = NA
  )
  expect_equal(
    bake(rec, new_data = NULL),
    ad_data %>% select(Genotype, tau, Class)
  )
})
