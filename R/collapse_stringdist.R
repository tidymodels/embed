#' collapse factor levels using stringdist
#'
#' `step_collapse_stringdist()` creates a *specification* of a recipe step that
#' will collapse factor levels that have a low stringdist between them.
#'
#' @inheritParams recipes::step_center
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See [selections()] for more details.  For the `tidy`
#'   method, these are not currently used.
#' @param role Not used by this step since no new variables are created.
#' @param distance Integer, value to determine which strings should be collapsed
#'   with which. The value is being used inclusive, so `2` will collapse levels
#'   that have a string distance between them of 2 or lower.
#' @param method Character, method for distance calculation. The default is 
#'   `"osa"`, see [stringdist::stringdist-metrics].
#' @param options List, other arguments passed to
#'   [stringdist::stringdistmatrix()] such as `weight`, `q`, `p`, and `bt`, that
#'   are used for different values of `method`. 
#' @param results A list denoting the way the labels should be collapses is
#'   stored here once this preprocessing step has be trained by [prep()].
#' @param columns A character string of variable names that will be populated
#'   (eventually) by the `terms` argument.
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of existing steps (if any). For the `tidy` method, a tibble with
#'   columns `terms` (the columns that will be affected) and `base`.
#'
#' @details
#'
#' # Tidying
#' 
#' When you [`tidy()`][tidy.recipe()] this step, a tibble is returned with
#' columns `terms`, `from`, `to`, and `id`:
#' 
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{from}{character, the old levels}
#'   \item{too}{character, the new levels}
#'   \item{id}{character, id of this step}
#' }
#'
#' @template case-weights-not-supported
#'
#' @examplesIf rlang::is_installed("stringdist")
#' library(recipes)
#' library(tibble)
#' data0 <- tibble(
#'   x1 = c("a", "b", "d", "e", "sfgsfgsd", "hjhgfgjgr"),
#'   x2 = c("ak", "b", "djj", "e", "hjhgfgjgr", "hjhgfgjgr")
#' )
#'
#' rec <- recipe(~., data = data0) %>%
#'   step_collapse_stringdist(all_predictors(), distance = 1) %>%
#'   prep()
#'
#' rec %>%
#'   bake(new_data = NULL)
#'
#' tidy(rec, 1)
#'
#' rec <- recipe(~., data = data0) %>%
#'   step_collapse_stringdist(all_predictors(), distance = 2) %>%
#'   prep()
#'
#' rec %>%
#'   bake(new_data = NULL)
#'
#' tidy(rec, 1)
#' @export
step_collapse_stringdist <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           distance = NULL,
           method = "osa",
           options = list(),
           results = NULL,
           columns = NULL,
           skip = FALSE,
           id = rand_id("collapse_stringdist")) {
    check_number_decimal(distance, min = 0)
    check_string(method)

    add_step(
      recipe,
      step_collapse_stringdist_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        distance = distance,
        method = method,
        options = options,
        results = results,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_collapse_stringdist_new <-
  function(terms, role, trained, distance, method, options, results, columns, 
           skip, id) {
    step(
      subclass = "collapse_stringdist",
      terms = terms,
      role = role,
      trained = trained,
      distance = distance,
      method = method,
      options = options,
      results = results,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_collapse_stringdist <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  values <- lapply(
    training[, col_names],
    collapse_stringdist_impl, 
    dist = x$distance,
    method = x$method,
    options = x$options
  )

  step_collapse_stringdist_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    distance = x$distance,
    method = x$method,
    options = x$options,
    results = values,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

collapse_stringdist_impl <- function(x, dist, method, options) {
  if (is.factor(x)) {
    x <- levels(x)
  } else {
    x <- unique(x)
  }
  
  cl <- rlang::call2(
    "stringdistmatrix",
    .ns = "stringdist",
    a = x, b = x, method = method,
    !!!options
  )
  
  dists <- rlang::eval_tidy(cl)

  pairs <- which(dists <= dist, arr.ind = TRUE)

  empty_logical <- logical(length(x))

  groups <- list()

  while (nrow(pairs) > 0) {
    group <- empty_logical
    selected <- pairs[1, 2]

    repeat {
      group[selected] <- TRUE
      new_selected <- pairs[pairs[, 2] %in% selected, 1]
      if (length(new_selected) == 0) break
      pairs <- pairs[!pairs[, 2] %in% selected, , drop = FALSE]
      selected <- new_selected
    }

    groups <- c(groups, list(which(group)))
  }

  lapply(groups, function(.x) x[.x])
}

#' @export
bake.step_collapse_stringdist <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    new_data[[col_name]] <- collapse_apply(
      new_data[[col_name]],
      object$results[[col_name]]
    )
  }
  as_tibble(new_data)
}

collapse_apply <- function(x, dict) {
  dict <- purrr::map_dfr(dict, ~ list(from = .x, to = .x[1]))

  res <- dict$to[match(x, dict$from)]
  
  factor(res, levels = unique(dict$to))
}

#' @export
print.step_collapse_stringdist <-
  function(x, width = max(20, options()$width - 31), ...) {
    title <- "Collapsing factor levels using stringdist"
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname step_collapse_stringdist
#' @usage NULL
#' @export
tidy.step_collapse_stringdist <- function(x, ...) {
  if (is_trained(x)) {
    if (length(x$results) == 0) {
      res <- tibble(terms = character())
    } else {
      res <- purrr::map_dfr(
        x$results,
        ~ purrr::map_dfr(.x, ~ list(from = .x, to = .x[1])),
        .id = "terms"
      )
    }
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.embed
#' @export
required_pkgs.step_collapse_stringdist <- function(x, ...) {
  c("embed", "stringdist")
}
