library(testthat)
library(dplyr)
library(rsample)

skip_on_cran()
skip_if_not_installed("xgboost")

source(test_path("make_binned_data.R"))

data("credit_data", package = "modeldata")
data("ames", package = "modeldata")
data("attrition", package = "modeldata")

# Data for classification problem testing
set.seed(42)
credit_data_split <- initial_split(credit_data, strata = "Status")
credit_data_train <- training(credit_data_split)
credit_data_test <- testing(credit_data_split)

set.seed(2393)
credit_data_small <- dplyr::sample_n(credit_data_train, 30)

rec_credit <- credit_data_train %>%
  select(-Status) %>%
  recipe(~.) %>%
  step_integer(all_predictors()) %>%
  prep(retain = TRUE)

xgb_credit_train <- xgboost::xgb.DMatrix(
  data = as.matrix(bake(rec_credit, new_data = NULL)),
  label = ifelse(credit_data_train[["Status"]] == "bad", 0, 1)
)

xgb_credit_test <- xgboost::xgb.DMatrix(
  data = as.matrix(bake(rec_credit, new_data = credit_data_test)),
  label = ifelse(credit_data_test[["Status"]] == "bad", 0, 1)
)

# Data for multi-classification problem testing
set.seed(42)
attrition <- attrition %>%
  mutate(EducationField = as.integer(EducationField) - 1)
attrition_data_split <- initial_split(attrition, strata = "EducationField")
attrition_data_train <- training(attrition_data_split)
attrition_data_test <- testing(attrition_data_split)

set.seed(2393)
attrition_data_small <- dplyr::sample_n(attrition_data_train, 10)

rec_attrition <- attrition_data_train %>%
  select(-EducationField) %>%
  recipe(~.) %>%
  step_integer(all_predictors()) %>%
  prep(retain = TRUE)

xgb_attrition_train <- xgboost::xgb.DMatrix(
  data = as.matrix(bake(rec_attrition, new_data = NULL)),
  label = attrition_data_train$EducationField
)

xgb_attrition_test <- xgboost::xgb.DMatrix(
  data = as.matrix(bake(rec_attrition, new_data = attrition_data_test)),
  label = attrition_data_test$EducationField
)

ames$Sale_Price <- log10(ames$Sale_Price)
# Data for regression problem testing (naive)
set.seed(1773)
ames_data_split <- initial_split(ames, strata = "Sale_Price")
ames_data_train <- training(ames_data_split)
ames_data_test <- testing(ames_data_split)

set.seed(8134)
ames_data_small <- dplyr::sample_n(ames_data_train, 10)

ames_rec <- ames_data_train %>%
  select(-Sale_Price) %>%
  recipe(~.) %>%
  step_integer(all_predictors()) %>%
  prep(retain = TRUE)

xgb_ames_train <- xgboost::xgb.DMatrix(
  data = as.matrix(bake(ames_rec, new_data = NULL)),
  label = ames_data_train[["Sale_Price"]]
)

xgb_ames_test <- xgboost::xgb.DMatrix(
  data = as.matrix(bake(ames_rec, new_data = ames_data_test)),
  label = ames_data_test[["Sale_Price"]]
)

set.seed(8497)
sim_tr_cls <- sim_data_2class(1000)
sim_te_cls <- sim_data_2class(100)

set.seed(8497)
sim_tr_mcls <- sim_data_3class(1000)
sim_te_mcls <- sim_data_3class(100)

set.seed(8497)
sim_tr_reg <- sim_data_reg(1000)
sim_te_reg <- sim_data_reg(100)

test_that("run_xgboost for classification", {
  xgboost <- embed:::run_xgboost(
    xgb_credit_train,
    xgb_credit_test,
    .learn_rate = 0.3,
    .num_breaks = 10,
    .tree_depth = 1,
    .min_n = 5,
    .objective = "binary:logistic",
    .num_class = NA
  )

  expect_snapshot(xgboost)
  expect_equal(length(xgboost$params), 8)
  expect_equal(xgboost$nfeatures, 13)
  expect_equal(xgboost$params$tree_method, "hist")
  expect_equal(xgboost$params$objective, "binary:logistic")
})

test_that("run_xgboost for multi-classification", {
  xgboost <- embed:::run_xgboost(
    xgb_attrition_train,
    xgb_attrition_test,
    .learn_rate = 0.3,
    .num_breaks = 10,
    .tree_depth = 1,
    .min_n = 5,
    .num_class = 6, # label must be in [0, num_class)
    .objective = "multi:softprob"
  )

  expect_snapshot(xgboost)
  expect_equal(length(xgboost$params), 9)
  expect_equal(xgboost$nfeatures, 30)
  expect_equal(xgboost$params$tree_method, "hist")
  expect_equal(xgboost$params$objective, "multi:softprob")
})

test_that("run_xgboost for regression", {
  xgboost <- embed:::run_xgboost(
    xgb_ames_train,
    xgb_ames_test,
    .learn_rate = 0.3,
    .num_breaks = 10,
    .tree_depth = 1,
    .min_n = 5,
    .objective = "reg:squarederror",
    .num_class = NA
  )

  expect_snapshot(xgboost)
  expect_true(length(xgboost$params) > 1)
  expect_true(xgboost$nfeatures > 1)
  expect_equal(xgboost$params$tree_method, "hist")
  expect_equal(xgboost$params$objective, "reg:squarederror")
})

test_that("xgb_binning for classification", {
  less_than_3.6 <- function() {
    utils::compareVersion("3.5.3", as.character(getRversion())) >= 0
  }
  skip_if(less_than_3.6())

  # Usual case
  set.seed(8497)
  xgb_binning <- embed:::xgb_binning(
    credit_data_train,
    "Status",
    "Seniority",
    sample_val = 0.20,
    learn_rate = 0.3,
    num_breaks = 10,
    tree_depth = 1,
    min_n = 5
  )

  expect_snapshot(xgb_binning)
  expect_true(length(xgb_binning) > 1)
  expect_type(xgb_binning, "double")

  skip_if(packageVersion("xgboost") > "1.5.2.1")
  # Algorithm runs on a too small training set/ insufficient variation in data
  expect_snapshot(
    embed:::xgb_binning(
      credit_data_small,
      "Status",
      "Seniority",
      sample_val = 0.30,
      learn_rate = 0.3,
      num_breaks = 10,
      tree_depth = 1,
      min_n = 5
    )
  )
})

test_that("xgb_binning for multi-classification", {
  less_than_3.6 <- function() {
    utils::compareVersion("3.5.3", as.character(getRversion())) >= 0
  }
  skip_if(less_than_3.6())

  # Usual case
  set.seed(8497)
  xgb_binning <- embed:::xgb_binning(
    attrition_data_train,
    "EducationField",
    "Age",
    sample_val = 0.20,
    learn_rate = 0.3,
    num_breaks = 10,
    tree_depth = 1,
    min_n = 5
  )

  expect_snapshot(xgb_binning)
  expect_true(length(xgb_binning) > 1)
  expect_type(xgb_binning, "double")

  # Algorithm runs on a too small training set/ insufficient variation in data
  expect_snapshot(
    embed:::xgb_binning(
      attrition_data_small,
      "EducationField",
      "Age",
      sample_val = 0.30,
      learn_rate = 0.3,
      num_breaks = 10,
      tree_depth = 1,
      min_n = 5
    )
  )
})

test_that("xgb_binning for regression", {
  less_than_3.6 <- function() {
    utils::compareVersion("3.5.3", as.character(getRversion())) >= 0
  }
  skip_if(less_than_3.6())

  set.seed(4235)
  # Usual case
  xgb_binning <- embed:::xgb_binning(
    ames_data_train,
    "Sale_Price",
    "Latitude",
    sample_val = 0.20,
    learn_rate = 0.3,
    num_breaks = 10,
    tree_depth = 1,
    min_n = 5
  )

  expect_snapshot(xgb_binning)
  expect_true(length(xgb_binning) > 1)
  expect_type(xgb_binning, "double")

  # Algorithm runs on a too small training set/ insufficient variation in data

  expect_snapshot(
    embed:::xgb_binning(
      ames_data_small,
      "Sale_Price",
      "Latitude",
      sample_val = 0.30,
      learn_rate = 0.3,
      num_breaks = 10,
      tree_depth = 1,
      min_n = 5
    )
  )
})

test_that("step_discretize_xgb for classification", {
  set.seed(125)
  # General use
  xgb_rec <-
    recipe(class ~ ., data = sim_tr_cls) %>%
    step_discretize_xgb(all_predictors(), outcome = "class")

  set.seed(28)
  xgb_rec <- prep(xgb_rec, training = sim_tr_cls)

  xgb_train_bins <- bake(xgb_rec, sim_tr_cls)
  xgb_test_bins <- bake(xgb_rec, sim_te_cls)

  expect_snapshot(xgb_train_bins[1:10, ])
  expect_snapshot(xgb_test_bins[1:10, ])
  expect_true(length(levels(xgb_train_bins$x)) > 1)
  expect_true(length(levels(xgb_train_bins$z)) > 1)

  expect_equal(
    levels(xgb_train_bins$x),
    levels(xgb_test_bins$x)
  )
  expect_equal(
    levels(xgb_train_bins$z),
    levels(xgb_test_bins$z)
  )

  # Too few data
  expect_snapshot(error = TRUE, {
    recipe(class ~ ., data = sim_tr_cls[1:9, ]) %>%
      step_discretize_xgb(all_predictors(), outcome = "class") %>%
      prep()
  })

  # No numeric variables present
  predictors_non_numeric <- c(
    "Status", "Home", "Marital"
  )

  xgb_rec <- credit_data_train %>%
    select(one_of(predictors_non_numeric)) %>%
    recipe(Status ~ .) %>%
    step_impute_median(all_numeric()) %>%
    step_discretize_xgb(all_numeric(), outcome = "Status")

  # Information about insufficient datapoints for Time predictor
  expect_snapshot({
    set.seed(1)
    recipe(Status ~ ., data = credit_data_train) %>%
      step_discretize_xgb(Time, outcome = "Status") %>%
      prep(retain = TRUE)
  })
})

test_that("step_discretize_xgb for multi-classification", {
  set.seed(125)
  # General use
  xgb_rec <-
    recipe(class ~ ., data = sim_tr_mcls) %>%
    step_discretize_xgb(all_predictors(), outcome = "class")

  set.seed(28)
  xgb_rec <- prep(xgb_rec, training = sim_tr_mcls)

  xgb_train_bins <- bake(xgb_rec, sim_tr_mcls)
  xgb_test_bins <- bake(xgb_rec, sim_te_mcls)

  expect_snapshot(xgb_train_bins[1:10, ])
  expect_snapshot(xgb_test_bins[1:10, ])
  expect_true(length(levels(xgb_train_bins$x)) > 0)
  expect_true(length(levels(xgb_train_bins$z)) > 0)

  expect_equal(
    levels(xgb_train_bins$x),
    levels(xgb_test_bins$x)
  )
  expect_equal(
    levels(xgb_train_bins$z),
    levels(xgb_test_bins$z)
  )

  # Too few data
  expect_snapshot(
    error = TRUE,
    recipe(class ~ ., data = sim_tr_mcls[1:9, ]) %>%
      step_discretize_xgb(all_predictors(), outcome = "class") %>%
      prep()
  )

  # No numeric variables present
  predictors_non_numeric <- c(
    "Attrition", "BusinessTravel", "Department",
    "Education", "EnvironmentSatisfaction", "Gender",
    "JobInvolvement", "JobRole", "JobSatisfaction",
    "MaritalStatus", "OverTime", "PerformanceRating",
    "RelationshipSatisfaction", "WorkLifeBalance"
  )

  xgb_rec <- attrition_data_train %>%
    select(one_of(predictors_non_numeric)) %>%
    recipe(BusinessTravel ~ .) %>%
    step_impute_median(all_numeric()) %>%
    step_discretize_xgb(all_numeric(), outcome = "BusinessTravel")
})

test_that("step_discretize_xgb for regression", {
  # Skip on R < 3.6 since the rng is different.
  skip("Needs to determine why random numbers are different")

  less_than_3.6 <- function() {
    utils::compareVersion("3.5.3", as.character(getRversion())) >= 0
  }
  skip_if(less_than_3.6())

  # General use
  set.seed(83834)
  xgb_rec <-
    recipe(y ~ ., data = sim_tr_reg) %>%
    step_discretize_xgb(all_predictors(), outcome = "y")
  tidy_untrained <- tidy(xgb_rec, 1)

  xgb_rec <- prep(xgb_rec, training = sim_tr_reg)
  tidy_trained <- tidy(xgb_rec, 1)

  xgb_train_bins <- bake(xgb_rec, sim_tr_reg)
  xgb_test_bins <- bake(xgb_rec, sim_te_reg)

  expect_snapshot(xgb_train_bins)
  expect_snapshot(xgb_test_bins)

  expect_true(length(levels(xgb_train_bins$x)) > 0)
  expect_true(length(levels(xgb_train_bins$z)) > 0)

  expect_equal(
    levels(xgb_train_bins$x),
    levels(xgb_test_bins$x)
  )
  expect_equal(
    levels(xgb_train_bins$z),
    levels(xgb_test_bins$z)
  )

  expect_true(tibble::is_tibble(tidy_untrained))
  expect_equal(
    tidy_untrained$variable,
    "all_predictors()"
  )
  expect_equal(
    tidy_untrained$values,
    NA_character_
  )
  expect_true(tibble::is_tibble(tidy_trained))
  expect_equal(
    tidy_trained$terms,
    rep(c("x", "z"), c(4, 7))
  )
  expect_equal(
    tidy_trained$values,
    unlist(xgb_rec$steps[[1]]$rules, use.names = FALSE)
  )

  # one bad predictor
  sim_tr_reg$x <- round(sim_tr_reg$x, 1)
  expect_snapshot({
    xgb_rec <-
      recipe(y ~ ., data = sim_tr_reg[1:100, ]) %>%
      step_discretize_xgb(all_predictors(), outcome = "y") %>%
      prep()
  })

  # No numeric variables present
  predictors_non_numeric <- c(
    "Neighborhood"
  )

  xgb_rec <- ames_data_train %>%
    select(Sale_Price, one_of(predictors_non_numeric)) %>%
    recipe(Sale_Price ~ .) %>%
    step_medianimpute(all_numeric()) %>%
    step_discretize_xgb(all_predictors(), outcome = "Sale_Price")
})

test_that("xgb_binning() errors if only one class in outcome", {
  const_outcome <- data.frame(
    outcome = factor(rep("a", 1000)),
    predictor = rep(1, 1000)
  )
  expect_snapshot(
    error = TRUE,
    embed:::xgb_binning(
      const_outcome,
      "outcome",
      "predictor",
      sample_val = 0.20,
      learn_rate = 0.3,
      num_breaks = 10,
      tree_depth = 1,
      min_n = 5
    )
  )
})

test_that("bake method errors when needed non-standard role columns are missing", {
  rec <- recipe(class ~ ., data = sim_tr_cls) %>%
    step_discretize_xgb(x, z, outcome = "class") %>%
    update_role(x, new_role = "potato") %>%
    update_role_requirements(role = "potato", bake = FALSE)

  rec_trained <- prep(rec, training = sim_tr_cls, verbose = FALSE)

  expect_error(
    bake(rec_trained, new_data = sim_tr_cls[, -1]),
    class = "new_data_missing_column"
  )
})

test_that("printing", {
  xgb_rec <-
    recipe(class ~ ., data = sim_tr_cls) %>%
    step_discretize_xgb(all_predictors(), outcome = "class")

  expect_snapshot(xgb_rec)
  expect_snapshot(prep(xgb_rec))
})

test_that("empty selections", {
  data(ad_data, package = "modeldata")
  expect_error(
    rec <-
      recipe(Class ~ Genotype + tau, data = ad_data) %>%
      step_discretize_xgb(starts_with("potato"), outcome = "Class") %>%
      prep(),
    regexp = NA
  )
  expect_equal(
    bake(rec, new_data = NULL),
    ad_data %>% select(Genotype, tau, Class)
  )
})

test_that("case weights step_discretize_xgb", {
  sim_tr_cls_cw <- sim_tr_cls %>%
    mutate(weight = importance_weights(rep(1:0, each = 500)))

  sim_tr_mcls_cw <- sim_tr_mcls %>%
    mutate(weight = importance_weights(rep(1:0, each = 500)))

  sim_tr_reg_cw <- sim_tr_reg %>%
    mutate(weight = importance_weights(rep(1:0, each = 500)))

  # classification ------------------------------------------------------------
  set.seed(125)
  # General use
  xgb_rec_cw <-
    recipe(class ~ ., data = sim_tr_cls_cw) %>%
    step_discretize_xgb(all_predictors(), outcome = "class")

  set.seed(28)
  xgb_rec_cw <- prep(xgb_rec_cw, training = sim_tr_cls_cw)

  exp_rules <- list()

  set.seed(28)
  for (col_names in c("x", "z")) {
    exp_rules[[col_names]] <- xgb_binning(
      sim_tr_cls_cw %>% select(-weight),
      "class",
      col_names,
      sample_val = 0.20,
      learn_rate = 0.3,
      num_breaks = 10,
      tree_depth = 1,
      min_n = 5,
      as.numeric(sim_tr_cls_cw$weight)
    )
  }
  expect_identical(
    exp_rules,
    xgb_rec_cw$steps[[1]]$rules
  )

  # multi-classification ------------------------------------------------------
  set.seed(125)
  # General use
  xgb_rec_cw <-
    recipe(class ~ ., data = sim_tr_mcls_cw) %>%
    step_discretize_xgb(all_predictors(), outcome = "class")

  set.seed(28)
  xgb_rec_cw <- prep(xgb_rec_cw, training = sim_tr_mcls_cw)

  exp_rules <- list()

  set.seed(28)
  for (col_names in c("x", "z")) {
    exp_rules[[col_names]] <- xgb_binning(
      sim_tr_mcls_cw %>% select(-weight),
      "class",
      col_names,
      sample_val = 0.20,
      learn_rate = 0.3,
      num_breaks = 10,
      tree_depth = 1,
      min_n = 5,
      as.numeric(sim_tr_mcls_cw$weight)
    )
  }
  expect_identical(
    exp_rules,
    xgb_rec_cw$steps[[1]]$rules
  )

  # regression ----------------------------------------------------------------
  set.seed(125)
  # General use
  xgb_rec_cw <-
    recipe(y ~ ., data = sim_tr_reg_cw) %>%
    step_discretize_xgb(all_predictors(), outcome = "y")

  set.seed(28)
  xgb_rec_cw <- prep(xgb_rec_cw, training = sim_tr_reg_cw)

  exp_rules <- list()

  set.seed(28)
  for (col_names in c("x", "z")) {
    exp_rules[[col_names]] <- xgb_binning(
      sim_tr_reg_cw %>% select(-weight),
      "y",
      col_names,
      sample_val = 0.20,
      learn_rate = 0.3,
      num_breaks = 10,
      tree_depth = 1,
      min_n = 5,
      as.numeric(sim_tr_reg_cw$weight)
    )
  }
  expect_identical(
    exp_rules,
    xgb_rec_cw$steps[[1]]$rules
  )

  # printing ------------------------------------------------------------------
  expect_snapshot(xgb_rec_cw)
})