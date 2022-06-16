
test_that("collapsing factors", {
  skip_if_not_installed("modeldata")
  data(ames, package = "modeldata")
  ames$Sale_Price <- log10(ames$Sale_Price)
  
  expect_error({
    rec_1 <- 
      recipe(Sale_Price ~ ., data = ames) %>% 
      step_collapse_cart(Neighborhood, Central_Air, outcome = vars(Sale_Price)) %>% 
      prep()
  }, 
  regex = NA)
  
  expect_true(length(rec_1$steps[[1]]$results) == 1)
  expect_equal(names(rec_1$steps[[1]]$results), "Neighborhood")
  
  expect_true(
    length(unique(rec_1$steps[[1]]$results$Neighborhood$.group)) <
      length(levels(ames$Neighborhood))
  )
  
  expect_equal(
    ames %>% select(-Neighborhood, -Sale_Price),
    juice(rec_1) %>% select(-Neighborhood, -Sale_Price)
  )
  
  expect_false(isTRUE(all.equal(juice(rec_1)$Neighborhood, ames$Neighborhood)))
  
  expect_error({
    rec_2 <- 
      recipe(Sale_Price ~ ., data = ames) %>% 
      step_collapse_cart(Neighborhood, Central_Air, outcome = vars(Sale_Price), 
                         min_n = 100, cost_complexity = 0.1) %>% 
      prep()
  }, 
  regex = NA)
  
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
  expect_error({
    rec_3 <- 
      recipe(Sale_Price2 ~ ., data = ames) %>% 
      step_collapse_cart(Neighborhood, Central_Air, outcome = vars(Sale_Price2)) %>% 
      prep()
  }, 
  regex = NA)
  
  expect_true(length(rec_3$steps[[1]]$results) == 0)
  
  # too many splits
  expect_error({
    rec_4 <- 
      recipe(Sale_Price ~ ., data = ames) %>% 
      step_collapse_cart(Neighborhood, outcome = vars(Sale_Price), cost_complexity = 0, min_n = 1) %>% 
      prep()
  }, 
  regex = NA)
  
  expect_true(length(rec_4$steps[[1]]$results) == 0)
  
  # too many splits
  expect_error({
    rec_5 <- 
      recipe(Sale_Price ~ ., data = ames) %>% 
      step_collapse_cart(Central_Air, outcome = vars(Sale_Price)) %>% 
      prep()
  }, 
  regex = NA)
  
  expect_true(length(rec_5$steps[[1]]$results) == 0)
})

