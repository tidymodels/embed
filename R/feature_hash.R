#' Dummy Variables Creation via Feature Hashing
#'
#' @description `r lifecycle::badge("soft-deprecated")`
#'
#'   `step_feature_hash()` is being deprecated in favor of
#'   [textrecipes::step_dummy_hash()]. This function creates a *specification*
#'   of a recipe step that will convert nominal data (e.g. character or factors)
#'   into one or more numeric binary columns using the levels of the original
#'   data.
#'
#' @inheritParams recipes::step_pca
#' @param num_hash The number of resulting dummy variable columns.
#' @param preserve Use `keep_original_cols` instead to specify whether the
#'   selected column(s) should be retained in addition to the new dummy
#'   variables.
#' @param columns A character vector for the selected columns. This is `NULL`
#'   until the step is trained by [recipes::prep()].
#' @template step-return
#' @details
#'
#' `step_feature_hash()` will create a set of binary dummy variables from a
#' factor or character variable. The values themselves are used to determine
#' which row that the dummy variable should be assigned (as opposed to having a
#' specific column that the value will map to).
#'
#' Since this method does not rely on a pre-determined assignment of levels to
#' columns, new factor levels can be added to the selected columns without
#' issue. Missing values result in missing values for all of the hashed columns.
#'
#' Note that the assignment of the levels to the hashing columns does not try to
#' maximize the allocation. It is likely that multiple levels of the column will
#' map to the same hashed columns (even with small data sets). Similarly, it is
#' likely that some columns will have all zeros. A zero-variance filter (via
#' [recipes::step_zv()]) is recommended for any recipe that uses hashed columns.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns `terms`
#' (the columns that is selected)  is returned.
#'
#' @template case-weights-not-supported
#'
#' @references
#'
#' Weinberger, K, A Dasgupta, J Langford, A Smola, and J Attenberg. 2009.
#' "Feature Hashing for Large Scale Multitask Learning." In Proceedings of the
#' 26th Annual International Conference on Machine Learning, 1113–20. ACM.
#'
#' Kuhn and Johnson (2020) _Feature Engineering and Selection: A Practical
#' Approach for Predictive Models_. CRC/Chapman Hall
#' \url{https://bookdown.org/max/FES/encoding-predictors-with-many-categories.html}
#' @seealso [recipes::step_dummy()], [recipes::step_zv()]
#' @examplesIf is_tf_available() && rlang::is_installed("modeldata")
#' data(grants, package = "modeldata")
#' rec <-
#'   recipe(class ~ sponsor_code, data = grants_other) %>%
#'   step_feature_hash(
#'     sponsor_code,
#'     num_hash = 2^6, keep_original_cols = TRUE
#'   ) %>%
#'   prep()
#'
#' # How many of the 298 locations ended up in each hash column?
#' results <-
#'   bake(rec, new_data = NULL, starts_with("sponsor_code")) %>%
#'   distinct()
#'
#' apply(results %>% select(-sponsor_code), 2, sum) %>% table()
#' @export
step_feature_hash <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           num_hash = 2^6,
           preserve = deprecated(),
           columns = NULL,
           keep_original_cols = FALSE,
           skip = FALSE,
           id = rand_id("feature_hash")) {
    lifecycle::deprecate_soft(
      "0.2.0",
      "embed::step_feature_hash()",
      "textrecipes::step_dummy_hash()"
    )

    if (lifecycle::is_present(preserve)) {
      lifecycle::deprecate_soft(
        "0.1.5",
        "step_feature_hash(preserve = )",
        "step_feature_hash(keep_original_cols = )"
      )
      keep_original_cols <- preserve
    }

    # warm start for tf to avoid a bug in tensorflow
    is_tf_available()

    add_step(
      recipe,
      step_feature_hash_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        num_hash = num_hash,
        preserve = keep_original_cols,
        columns = columns,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_feature_hash_new <-
  function(terms, role, trained, num_hash, preserve, columns,
           keep_original_cols, skip, id) {
    step(
      subclass = "feature_hash",
      terms = terms,
      role = role,
      trained = trained,
      num_hash = num_hash,
      preserve = preserve,
      columns = columns,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_feature_hash <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  if (length(col_names) > 0) {
    check_type(training[, col_names], types = c("string", "factor", "ordered"))
  }

  step_feature_hash_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_hash = x$num_hash,
    preserve = x$preserve,
    columns = col_names,
    keep_original_cols = get_keep_original_cols(x),
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

  nms <- names0(num_hash, prefix)
  make_hash_tbl(column_int, nms) %>%
    dplyr::mutate(data = uni_x) %>%
    dplyr::left_join(tmp, by = "data", multiple = "all") %>%
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
  check_new_data(names(object$columns), object, new_data)

  # If no terms were selected
  if (length(object$columns) == 0) {
    return(new_data)
  }

  new_names <- paste0(object$columns, "_hash_")

  new_cols <- purrr::map2_dfc(
    new_data[, object$columns],
    new_names, make_hash_vars,
    num_hash =
      object$num_hash
  )
  
  new_cols <- check_name(new_cols, new_data, object, names(new_cols))
  
  new_data <- vec_cbind(new_data, new_cols)

  keep_original_cols <- get_keep_original_cols(object)
  if (!keep_original_cols) {
    new_data <- new_data %>% dplyr::select(-one_of(!!!object$columns))
  }

  new_data
}

#' @export
print.step_feature_hash <-
  function(x, width = max(20, options()$width - 31), ...) {
    title <- "Feature hashed dummy variables for "
    print_step(names(x$mapping), x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_feature_hash` object.
#' @export
tidy.step_feature_hash <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = unname(x$columns))
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.embed
#' @export
required_pkgs.step_feature_hash <- function(x, ...) {
  c("keras", "embed")
}
