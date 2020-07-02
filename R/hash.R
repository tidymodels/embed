#' Dummy Variables Creation via Feature Hashing
#'
#' `step_feature_hash` creates a a *specification* of a recipe step that will
#'  convert nominal data (e.g. character or factors) into one or more numeric
#'  binary columns using the levels of the original data.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'  operations for this recipe.
#' @param trained A logical to indicate if the quantities for preprocessing
#'  have been estimated.
#' @param skip A logical. Should the step be skipped when the recipe is baked
#'  by [recipes::bake.recipe()]? While all operations are baked when
#'  [recipes::prep.recipe()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)). Care should
#'  be taken when using `skip = TRUE` as it may affect the computations for
#'  subsequent operations.
#' @param id A character string that is unique to this step to identify it. 
#' @param ... One or more selector functions to choose which _factor_
#'  variables will be used to create the dummy variables. See [selections()] for
#'  more details. The selected variables must be factors. For the `tidy` method,
#'  these are not currently used.
#' @param role For model terms created by this step, what analysis role should
#'  they be assigned?. By default, the function assumes that the binary dummy
#'  variable columns created by the original variables will be used as
#'  predictors in a model.
#' @param num_hash The number of resulting dummy variable columns.
#' @param preserve A single logical; should the selected column(s) be retained
#'  (in addition to the new dummy variables)?
#' @param columns A character vector for the selected columns. This is `NULL` 
#'  until the step is trained by [recipes::prep.recipe()].
#' @return An updated version of `recipe` with the new step added to the
#'  sequence of existing steps (if any). For the `tidy` method, a tibble with
#'  columns `terms` (the selectors or original variables selected).
#' @keywords datagen
#' @concept preprocessing
#' @concept dummy_variables
#' @concept model_specification
#' @concept variable_encodings
#' @export
#' @details `step_feature_hash()` will create a set of binary dummy variables
#'  from a factor or character variable. The values themselves are used to
#'  determine which row that the dummy variable should be assigned (as opposed
#'  to having a specific column that the value will map to).
#' 
#' Since this method does not rely on a pre-determined assignment of levels to
#'  columns, new factor levels can be added to the selected columns without
#'  issue. Missing values result in missing values for all of the hashed columns.
#'
#' Note that the assignment of the levels to the hashing columns does not try
#'  to maximize the allocation. It is likely that multiple levels of the column
#'  will map to the same hashed columns (even with small data sets). Similarly,
#'  it is likely that some columns will have all zeros. A zero-variance filter
#'  (via [recipes::step_zv()]) is recommended for any recipe that uses hashed
#'  columns.
#' @references 
#' Weinberger, K, A Dasgupta, J Langford, A Smola, and J Attenberg. 2009.
#'  "Feature Hashing for Large Scale Multitask Learning." In Proceedings of the
#'  26th Annual International Conference on Machine Learning, 1113–20. ACM.
#' 
#' Kuhn and Johnson (2020) _Feature Engineering and Selection: A Practical
#'  Approach for Predictive Models_. CRC/Chapman Hall
#'  \url{https://bookdown.org/max/FES/encoding-predictors-with-many-categories.html}
#' @seealso [recipes::step_dummy()], [recipes::step_zv()]
#' @examples
#' \donttest{
#' data(okc, package = "modeldata")
#' 
#' # This may take a while: 
#' rec <- 
#'   recipe(Class ~ age + location, data = okc) %>%
#'   step_feature_hash(location, num_hash = 2^6, preserve = TRUE) %>% 
#'   prep()
#' 
#' # How many of the 135 locations ended up in each hash column?
#' results <- 
#'   juice(rec, starts_with("location")) %>% 
#'   distinct() 
#' 
#' apply(results %>% select(-location), 2, sum) %>% table()
#' }

step_feature_hash <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           num_hash = 2^6,
           preserve = FALSE,
           columns = NULL,
           skip = FALSE,
           id = rand_id("feature_hash")) {
    # warm start for tf to avoid a bug in tensorflow
    tensorflow::tf_version()
    
    add_step(
      recipe,
      step_feature_hash_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        num_hash = num_hash,
        preserve = preserve,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_feature_hash_new <-
  function(terms, role, trained, num_hash, preserve, columns, skip, id) {
    step(
      subclass = "feature_hash",
      terms = terms,
      role = role,
      trained = trained,
      num_hash = num_hash,
      preserve = preserve,
      columns = columns,
      skip = skip,
      id = id
    )
  }

passover <- function(cmd) {
  # cat("`step_feature_hash()` was not able to select any columns. ",
  #     "No dummy variables will be created.\n")
} # figure out how to return a warning() without exiting

#' @export
prep.step_feature_hash <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info, empty_fun = passover)
  check_type(training[, col_names], quant = FALSE)
  
  step_feature_hash_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_hash = x$num_hash,
    preserve = x$preserve,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

make_hash_vars <- function(x, prefix, num_hash = 2^8) {
  if (!is.character(x)) {
    x <- as.character(x)
  }
  
  tmp <- tibble(data = x, ..order = seq_along(x))
  
  uni_x <- unique(x)
  
  column_int <-
    purrr::map_int(
      uni_x,
      keras::text_hashing_trick,
      n = num_hash,
      filters = "",
      split = "dont split characters",
      lower = FALSE
    )
  column_int[is.na(uni_x)] <- NA
  
  nms <- recipes::names0(num_hash, prefix)
  make_hash_tbl(column_int, nms) %>% 
    dplyr::mutate(data = uni_x) %>% 
    dplyr::left_join(tmp, by = "data") %>% 
    dplyr::arrange(..order) %>% 
    dplyr::select(-data, -..order)
}

make_row <- function(ind, p) {
  if (!is.na(ind)) {
    x <- rep(0, p)
    x[ind] <- 1
  } else {
    x <- rep(NA_real_, p)
  }
  x
}

make_hash_tbl <- function(ind, nms) {
  p <- length(nms)
  x <- purrr::map(ind, make_row, p = p)
  x <- do.call("rbind", x)
  colnames(x) <- nms
  tibble::as_tibble(x)
}

#' @export
bake.step_feature_hash <- function(object, new_data, ...) {
  
  # If no terms were selected
  if (length(object$columns) == 0) {
    return(new_data)
  }
  
  new_names <- paste0(object$columns, "_hash_")
  
  new_data <- 
    new_data %>% dplyr::bind_cols(
      purrr::map2_dfc(
        new_data[, object$columns], 
        new_names, make_hash_vars, 
        num_hash = 
          object$num_hash
      )
    )
  
  if (!object$preserve) {
    new_data <- new_data %>% dplyr::select(-one_of(!!!object$columns))
  }
  
  new_data
}

print.step_feature_hash <-
  function(x, width = max(20, options()$width - 31), ...) {
    cat("Feature hashed dummy variables for ", sep = "")
    printer(names(x$mapping), x$terms, x$trained, width = width)
    invisible(x)
  }


#' @rdname step_feature_hash
#' @param x A `step_feature_hash` object.
#' @export
tidy.step_feature_hash <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$columns)
  }
  else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res$id <- x$id
  res
}
