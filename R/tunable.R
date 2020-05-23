#' tunable methods for embed
#'
#' These functions define what parameters _can_ be tuned for specific steps.
#'  They also define the recommended objects from the `dials` package that can
#'  be used to generate new parameter values and other characteristics.
#' @param x A recipe step object
#' @param ... Not used.
#' @return A tibble object.
#' @keywords internal
#' @export
tunable.step_embed <- function(x, ...) {
  tibble::tibble(
    name = c("num_terms", "hidden_units"),
    call_info = list(
      list(pkg = "dials", fun = "num_terms", range = c(2, 10)),
      list(pkg = "dials", fun = "hidden_units", range = c(0, 10))
    ),
    source = "recipe",
    component = "step_embed",
    component_id = x$id
  )
}


#' @export
#' @rdname tunable.step_embed
tunable.step_umap <- function(x, ...) {
  tibble::tibble(
    name = c("num_comp", "neighbors", "min_dist", "learn_rate", "epochs"),
    call_info = list(
      list(pkg = "dials", fun = "num_comp", range = c(1, 10)),
      list(pkg = "dials", fun = "neighbors", range = c(5, 25)),
      list(pkg = "dials", fun = "min_dist", range = c(-4, 0)),
      list(pkg = "dials", fun = "learn_rate"),
      list(pkg = "dials", fun = "epochs", range = c(100, 700))
    ),
    source = "recipe",
    component = "step_umap",
    component_id = x$id
  )
}

#' @export
#' @rdname tunable.step_embed
tunable.step_woe <- function(x, ...) {
  tibble::tibble(
    name = "Laplace",
    call_info = list(
      list(pkg = "dials", fun = "Laplace")
    ),
    source = "recipe",
    component = "step_woe",
    component_id = x$id
  )
}

#' @export
#' @rdname tunable.step_embed
tunable.step_discretize_xgb <- function(x, ...) {
  tibble::tibble(
    name = c("sample_val", "learn_rate", "num_breaks", "tree_depth", "min_n"),
    call_info = list(
      list(pkg = "dials", fun = "sample_val", range = c(0.05, 0.7)),
      list(pkg = "dials", fun = "learn_rate", range = log10(c(0.05, 0.3))),
      list(pkg = "dials", fun = "num_breaks", range = c(5, 30)),
      list(pkg = "dials", fun = "tree_depth", range = c(10, 30)),
      list(pkg = "dials", fun = "min_n")
    ),
    source = "recipe",
    component = "step_discretize_xgb",
    component_id = x$id
  )
}

#' @export
#' @rdname tunable.step_embed
tunable.step_discretize_cart <- function(x, ...) {
  tibble::tibble(
    name = c("cost_complexity", "tree_depth", "min_n"),
    call_info = list(
      list(pkg = "dials", fun = "cost_complexity"), 
      list(pkg = "dials", fun = "tree_depth"), 
      list(pkg = "dials", fun = "min_n")
    ),
    source = "recipe",
    component = "step_discretize_cart",
    component_id = x$id
  )
}
