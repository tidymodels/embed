
test_that("collapsing factors", {
  skip_if_not_installed("modeldata")
  data(ames, package = "modeldata")
  
  expect_error({
    rec_1 <- 
      recipe(Sale_Price ~ ., data = ames) %>% 
      step_collapse_stringdist(MS_SubClass, distance = 5) %>% 
      prep()
  }, 
  regex = NA)
  
  expect_true(length(rec_1$steps[[1]]$results) == 1)
  expect_equal(names(rec_1$steps[[1]]$results), "MS_SubClass")
  
  expect_true(
    length(unique(rec_1$steps[[1]]$results$Neighborhood$.group)) <
      length(levels(ames$Neighborhood))
  )
  
  expect_equal(
    ames %>% select(-MS_SubClass, -Sale_Price),
    juice(rec_1) %>% select(-MS_SubClass, -Sale_Price)
  )
  
  expect_false(isTRUE(all.equal(juice(rec_1)$MS_SubClass, ames$MS_SubClass)))
  
  expect_error({
    rec_2 <- 
      recipe(Sale_Price ~ ., data = ames) %>% 
      step_collapse_stringdist(MS_SubClass, Overall_Cond, distance = 10) %>% 
      prep()
  }, 
  regex = NA)
  
  expect_true(length(rec_2$steps[[1]]$results) == 2)
  expect_equal(names(rec_2$steps[[1]]$results), c("MS_SubClass", "Overall_Cond"))
  
  expect_true(
    length(rec_2$steps[[1]]$results$MS_SubClass) <
      length(rec_1$steps[[1]]$results$MS_SubClass)
  )
})

test_that("collapsing factors manual test", {
  data0 <- tibble(
    x1 = c("a", "b", "d", "e", "aaaaaa", "bbbbbb"),
    x2 = c("ak", "b", "djj", "e", "aaaaaa", "aaaaaa")
  )
  
  rec <- recipe(~., data = data0) %>%
    step_collapse_stringdist(all_predictors(), distance = 1) %>%
    prep()
  
  exp_result <- tibble(
    x1 = factor(c("a", "a", "a", "a", "aaaaaa", "bbbbbb")),
    x2 = factor(c("ak", "b", "djj", "b", "aaaaaa", "aaaaaa"))
  )
  expect_equal(
    bake(rec, new_data = NULL),
    exp_result
  )
  
  rec <- recipe(~., data = data0) %>%
    step_collapse_stringdist(all_predictors(), distance = 2) %>%
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

test_that("failed collapsing", {
  skip_if_not_installed("modeldata")
  data(ames, package = "modeldata")
  
  # too many splits
  expect_error({
    rec_4 <- 
      recipe(Sale_Price ~ ., data = ames) %>% 
      step_collapse_stringdist(MS_SubClass, distance = 0) %>% 
      prep()
  }, 
  regex = NA)
  
  expect_equal(
    length(rec_4$steps[[1]]$results$MS_SubClass),
    length(levels(ames$MS_SubClass))
  )
  
  # too few splits
  expect_error({
    rec_5 <- 
      recipe(Sale_Price ~ ., data = ames) %>% 
      step_collapse_stringdist(MS_SubClass, distance = 10000) %>% 
      prep()
  }, 
  regex = NA)
  
  expect_equal(
    length(rec_5$steps[[1]]$results$MS_SubClass),
    1
  )
})

