#' collapse factor levels using stringdist
#'
#' `step_collapse_stringdist` creates a *specification* of a recipe step that
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
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns `"terms"`
#' (the column being modified), `"from"` (the old levels), `"to"` (the new
#' levels), and `"id"`.
#'
#' @template case-weights-not-supported
#' 
#' @examples
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
           results = NULL,
           columns = NULL,
           skip = FALSE,
           id = rand_id("collapse_stringdist")) {
    if (is.null(distance)) {
      rlang::abort("`distance` argument must be set.")
    }

    add_step(
      recipe,
      step_collapse_stringdist_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        distance = distance,
        results = results,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_collapse_stringdist_new <-
  function(terms, role, trained, distance, results, columns, skip, id) {
    step(
      subclass = "collapse_stringdist",
      terms = terms,
      role = role,
      trained = trained,
      distance = distance,
      results = results,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_collapse_stringdist <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  values <- lapply(training[, col_names], collapse_stringdist_impl, x$distance)

  step_collapse_stringdist_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    distance = x$distance,
    results = values,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

collapse_stringdist_impl <- function(x, dist) {
  if (is.factor(x)) {
    x <- levels(x)
  } else {
    x <- unique(x)
  }
  dists <- stringdist::stringdistmatrix(x, x)

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
  col_names <- object$columns
  # for backward compat
  check_new_data(names(col_names), object, new_data)

  for (i in seq_along(col_names)) {
    new_data[, col_names[i]] <- collapse_apply(
      new_data[[col_names[i]]],
      object$results[[i]]
    )
  }
  as_tibble(new_data)
}

collapse_apply <- function(x, dict) {
  dict <- purrr::map_dfr(dict, ~ list(from = .x, to = .x[1]))

  dict$to[match(x, dict$from)]
}

#' @export
print.step_collapse_stringdist <-
  function(x, width = max(20, options()$width - 31), ...) {
    title <- "Collapsing factor levels using stringdist"
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_collapse_stringdist` object.
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
