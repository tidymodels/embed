context("woe")

source(testthat::test_path("test_helpers.R"))

# ------------------------------------------------------------------------------

data("credit_data", package = "modeldata")

set.seed(342)
in_training <- sample(1:nrow(credit_data), 2000)

credit_tr <- credit_data[ in_training, ]
credit_te <- credit_data[-in_training, ]


set.seed(1)
df <- data.frame(x1 = sample(c("A", "B", "C"), size = 20, replace = TRUE) %>% factor,
                 x2 = sample(c("A", "B", "C"), size = 20, replace = TRUE),
                 stringsAsFactors = TRUE) %>%
  mutate(y = rbinom(20, 1, prob = 1/(1 + exp(-1 * (-4 + as.numeric(x1) + as.numeric(x2)))))) %>%
  mutate(y = if_else(y == 1, "A", "B"))

#------------------------------------
# woe_table

test_that("woe_table do not accept different length inputs", {
  expect_error(embed:::woe_table(rep(c(0, 1), 20), rep(letters[1:4], 5)))
})

test_that("woe_table accepts only outcome with 2 distinct categories", {
  expect_error(embed:::woe_table(rep(letters[1:3], 10), rep(c(0, 1, 2), 10)))
  expect_error(embed:::woe_table(rep(letters[1:3], 10), rep(c(0), 30)))
  expect_error(embed:::woe_table(df$x2, df$x1))
})

test_that("woe_table returns a proper tibble", {
  expect_equal(dim(embed:::woe_table(df$x1, df$y)), c(3, 7))
  expect_identical(names(embed:::woe_table(df$x1, df$y)),
                   c("predictor", "n_tot", "n_A", "n_B", "p_A", "p_B", "woe"))
})

test_that("logical outcome variables are treated properly", {
  expect_equal(
    dim(
      embed:::woe_table(c("A", "A", "A", "B"), c(TRUE, FALSE, TRUE, FALSE))), 
    c(2, 7)
  )
})

test_that("logical predictor variable are treated properly", {
  expect_equal(
    class(embed:::woe_table(c(TRUE, FALSE, TRUE, FALSE), c("A", "A", "A", "B"))$predictor), 
    "character"
  )
})

test_that("woe_table ruturns no messages nor warnings", {
  expect_silent(embed:::woe_table(c(TRUE, FALSE, TRUE, FALSE), c("A", "A", "A", "B")))
  expect_silent(embed:::woe_table(c(TRUE, FALSE, TRUE, FALSE, NA), c("A", "A", "A", "B", "B")))
  expect_silent(embed:::woe_table(as.factor(c(TRUE, FALSE, TRUE, FALSE, NA)), c("A", "A", "A", "B", "B")))
  expect_silent(embed:::woe_table(df$x1, df$y))
})

test_that("Laplace works", {
  expect_true(all(is.finite(embed:::woe_table(c("A", "A", "B", "B"), c(0, 0, 0, 1), Laplace = 1e-6)$woe)))
  expect_false(all(is.finite(embed:::woe_table(c("A", "A", "B", "B"), c(0, 0, 0, 1), Laplace = 0)$woe)))
})


#------------------------------------
# dictionary

test_that("dictionary returns a proper tibble", {
  expect_equal(dictionary(df, "y") %>% class, c("tbl_df", "tbl", "data.frame"))
  expect_equal(dictionary(df, "y") %>% dim, c(6, 9))
  expect_identical(dictionary(df, "y") %>% names, 
                   c("variable", "predictor", "n_tot", "n_A", "n_B", 
                     "p_A", "p_B", "woe", "outcome"))
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
  expect_silent(dictionary(df %>% mutate(x3 = rep(c(TRUE, FALSE), 10)), "y", x3))
})


#------------------------------------
# add_woe

test_that("add_woe returns a proper tibble", {
  expect_equal(add_woe(df, "y") %>% class(), c("tbl_df", "tbl", "data.frame"))
  expect_equal(add_woe(df, "y") %>% dim, c(20, 5))
  expect_identical(add_woe(df, "y") %>% names, c("x1", "x2", "y", "woe_x1", "woe_x2"))
})

test_that("add_woe accepts only outcome with 2 distinct categories", {
  expect_error(dictionary(df %>% filter(y %in% "B"), y))
})

test_that("add_woe ruturns no messages nor warnings nor errors", {
  expect_silent(add_woe(df, "y", x1))
  expect_silent(add_woe(df %>% mutate(x3 = rep(c(TRUE, FALSE), 10)), "y", x3))
})

test_that("add_woe accepts numeric, logical and character predictor variables", {
  expect_equal(add_woe(df %>% mutate(x3 = rep(c(TRUE, FALSE), 10),
                                     x4 = rep(c(20, 30), 10)), "y") %>% dim, c(20, 9))
})

test_that("add_woe returns woe only for those variables that exists in both data and dictionary", {
  expect_equal(names(add_woe(df, "y", x2, dictionary = dictionary(df, "y", x1))), c("x1", "x2", "y"))
  expect_equal(names(add_woe(df, "y", x1, dictionary = dictionary(df, "y", x1))), c("x1", "x2", "y", "woe_x1"))
  expect_equal(names(add_woe(df, "y", dictionary = dictionary(df, "y", x1))), c("x1", "x2", "y", "woe_x1"))
  expect_equal(names(add_woe(df, "y", x1, x2, dictionary = dictionary(df, "y", x1))), c("x1", "x2", "y", "woe_x1"))
})

test_that("add_woe do not accept dictionary with unexpected layout", {
  expect_error(add_woe(df, outcome = "y", x1, dictionary = iris))
  expect_error(add_woe(df, outcome = "y", x1, dictionary = iris %>% mutate(variable = 1)))
})

test_that("add_woe warns user if the variable has too many levels", {
  expect_warning(credit_data %>% add_woe("Status", Expenses))
})

#------------------------------------
# step_woe

test_that("step_woe", {

  rec <-
    recipe(Status ~ ., data = credit_tr) %>%
    step_woe(Job, Home, outcome = vars(Status))

  woe_models <- prep(rec, training = credit_tr)

  woe_dict <- credit_tr %>% dictionary("Status", Job, Home)
  expect_equal(woe_dict, woe_models$steps[[1]]$dictionary)

  bake_woe_output <- bake(woe_models, new_data = credit_te)
  add_woe_output <-
    credit_te %>%
    add_woe("Status", Job, Home, dictionary = woe_dict)  %>%
    dplyr::select(one_of(names(bake_woe_output)))

  #
  expect_equal(bake_woe_output, add_woe_output)

  tidy_output <- tidy(woe_models, number = 1)
  woe_dict_output <- 
    dictionary(credit_tr, Job, Home, outcome = "Status") %>% 
    dplyr::rename(terms = variable, value = predictor)

  #
  expect_equal(tidy_output %>% dplyr::select(-id), woe_dict_output)

  rec_all_nominal <- recipe(Status ~ ., data = credit_tr) %>%
    step_woe(all_nominal(), outcome = vars(Status))

  #
  expect_output(prep(rec_all_nominal, training = credit_tr, verbose = TRUE))


  rec_all_numeric <- recipe(Status ~ ., data = credit_tr) %>%
    step_woe(all_predictors(), outcome = vars(Status))

  #
  expect_error(prep(rec_all_numeric, training = credit_tr))

  rec_discretize <- recipe(Status ~ ., data = credit_tr) %>% step_discretize(Price)
  rec_discretize_woe <- rec_discretize %>%step_woe(Price, outcome = vars(Status))

  prep_discretize <- prep(rec_discretize, training = credit_tr)
  prep_discretize_woe <- prep(rec_discretize_woe, training = credit_tr)

  bake_discretize <- bake(prep_discretize, new_data = credit_te)
  bake_discretize_woe <- bake(prep_discretize_woe, new_data = credit_te)

  expect_equal(sort(as.character(unique(bake_discretize$Price))),
               sort(prep_discretize_woe$steps[[2]]$dictionary$predictor))
})

test_that("printing", {
  woe_extract <- recipe(Status ~ ., data = credit_tr) %>%
    step_woe(Job, Home, outcome = vars(Status))
  expect_output(print(woe_extract))
  expect_output(prep(woe_extract, training = credit_tr, verbose = TRUE))
})


test_that("2-level factors", {
  iris3 <- iris
  iris3$group <- factor(rep(letters[1:5], each = 30))

  expect_error(
    recipe(Species ~ ., data = iris3) %>%
    step_woe(group, outcome = vars(Species)) %>%
    prep()
  )
})

