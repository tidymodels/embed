#' Supervised and unsupervised uniform manifold approximation and projection
#' (UMAP)
#'
#' `step_umap()` creates a *specification* of a recipe step that will project a
#' set of features into a smaller space.
#'
#' @inheritParams recipes::step_pca
#' @param min_dist The effective minimum distance between embedded points.
#' @param num_comp An integer for the number of UMAP components. If `num_comp`
#'   is greater than the number of selected columns minus one, the smaller value
#'   is used.
#' @param neighbors An integer for the number of nearest neighbors used to
#'   construct the target simplicial set. If `neighbors` is greater than the
#'   number of data points, the smaller value is used.
#' @param metric Character, type of distance metric to use to find nearest 
#'   neighbors. See [uwot::umap()] for more details. Default to `"euclidean"`.
#' @param epochs Number of iterations for the neighbor optimization. See
#'   [uwot::umap()] for more details.
#' @param learn_rate Positive number of the learning rate for the optimization
#'   process.
#' @param outcome A call to `vars` to specify which variable is used as the
#'   outcome in the encoding process (if any).
#' @param options A list of options to pass to [uwot::umap()]. The arguments
#'   `X`, `n_neighbors`, `n_components`, `min_dist`, `n_epochs`, `ret_model`,
#'   and `learning_rate` should not be passed here. By default, `verbose` and
#'   `n_threads` are set.
#' @param seed Two integers to control the random numbers used by the numerical
#'   methods. The default pulls from the main session's stream of numbers and
#'   will give reproducible results if the seed is set prior to calling [prep()]
#'   or [bake()].
#' @param retain Use `keep_original_cols` instead to specify whether the
#'   original predictors should be retained along with the new embedding
#'   variables.
#' @param object An object that defines the encoding. This is `NULL` until the
#'   step is trained by [recipes::prep()].
#' @template step-return
#' @details
#'
#' UMAP, short for Uniform Manifold Approximation and Projection, is a nonlinear
#' dimension reduction technique that finds local, low-dimensional
#' representations of the data. It can be run unsupervised or supervised with
#' different types of outcome data (e.g. numeric, factor, etc).
#'
#' ```{r, echo = FALSE, results="asis"}
#' prefix <- "UMAP"
#' result <- knitr::knit_child("man/rmd/num_comp.Rmd")
#' cat(result)
#' ```
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, a tibble with columns `terms`
#' (the selectors or variables selected) is returned.
#' 
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_umap"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-not-supported
#'
#' @template bundle
#'
#' @references
#'
#' McInnes, L., & Healy, J. (2018). UMAP: Uniform Manifold
#' Approximation and Projection for Dimension Reduction. 
#' \url{https://arxiv.org/abs/1802.03426}.
#'
#' "How UMAP Works"
#' \url{https://umap-learn.readthedocs.io/en/latest/how_umap_works.html}
#'
#' @examplesIf rlang::is_installed("ggplot2") && rlang::is_installed("irlba", version = "2.3.5.2")
#' library(recipes)
#' library(ggplot2)
#'
#' split <- seq.int(1, 150, by = 9)
#' tr <- iris[-split, ]
#' te <- iris[split, ]
#'
#' set.seed(11)
#' supervised <-
#'   recipe(Species ~ ., data = tr) %>%
#'   step_center(all_predictors()) %>%
#'   step_scale(all_predictors()) %>%
#'   step_umap(all_predictors(), outcome = vars(Species), num_comp = 2) %>%
#'   prep(training = tr)
#'
#' theme_set(theme_bw())
#'
#' bake(supervised, new_data = te, Species, starts_with("umap")) %>%
#'   ggplot(aes(x = UMAP1, y = UMAP2, col = Species)) +
#'   geom_point(alpha = .5)
#' @export
step_umap <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           outcome = NULL,
           neighbors = 15,
           num_comp = 2,
           min_dist = 0.01,
           metric = "euclidean",
           learn_rate = 1,
           epochs = NULL,
           options = list(verbose = FALSE, n_threads = 1),
           seed = sample(10^5, 2),
           prefix = "UMAP",
           keep_original_cols = FALSE,
           retain = deprecated(),
           object = NULL,
           skip = FALSE,
           id = rand_id("umap")) {
    if (lifecycle::is_present(retain)) {
      lifecycle::deprecate_soft(
        "0.1.5",
        "step_umap(retain = )",
        "step_umap(keep_original_cols = )"
      )
      keep_original_cols <- retain
    }

    recipes_pkg_check(required_pkgs.step_umap())
    if (is.numeric(seed) && !is.integer(seed)) {
      seed <- as.integer(seed)
    }
    if (length(seed) != 2) {
      rlang::abort("Two integers are required for `seed`.")
    }

    add_step(
      recipe,
      step_umap_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        outcome = outcome,
        neighbors = neighbors,
        num_comp = num_comp,
        min_dist = min_dist,
        metric = metric,
        learn_rate = learn_rate,
        epochs = epochs,
        options = options,
        seed = seed,
        prefix = prefix,
        keep_original_cols = keep_original_cols,
        retain = keep_original_cols,
        object = object,
        skip = skip,
        id = id
      )
    )
  }

step_umap_new <-
  function(terms, role, trained, outcome, neighbors, num_comp, min_dist, metric,
           learn_rate, epochs, options, seed, prefix, keep_original_cols,
           retain, object, skip, id) {
    step(
      subclass = "umap",
      terms = terms,
      role = role,
      trained = trained,
      outcome = outcome,
      neighbors = neighbors,
      num_comp = num_comp,
      min_dist = min_dist,
      metric = metric,
      learn_rate = learn_rate,
      epochs = epochs,
      options = options,
      seed = seed,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      retain = retain,
      object = object,
      skip = skip,
      id = id
    )
  }

umap_fit_call <- function(obj, y = NULL) {
  cl <- rlang::call2(
    "umap",
    .ns = "uwot",
    X = rlang::expr(training[, col_names])
  )
  if (!is.null(y)) {
    cl$y <- rlang::expr(training[[y_name]])
  }
  cl$n_neighbors <- obj$neighbors
  cl$n_components <- obj$num_comp
  cl$n_epochs <- obj$epochs
  cl$learning_rate <- obj$learn_rate
  cl$min_dist <- obj$min_dist
  cl$metric <- obj$metric
  if (length(obj$options) > 0) {
    cl <- rlang::call_modify(cl, !!!obj$options)
  }
  cl$ret_model <- TRUE
  cl
}

#' @export
prep.step_umap <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  if (length(col_names) > 0) {
    if (length(x$outcome) > 0) {
      y_name <- recipes_eval_select(x$outcome, training, info)
    } else {
      y_name <- NULL
    }
    x$neighbors <- min(nrow(training) - 1, x$neighbors)
    x$num_comp <- min(length(col_names) - 1, x$num_comp)
    withr::with_seed(
      x$seed[1],
      res <- rlang::eval_tidy(umap_fit_call(x, y = y_name))
    )
    res$xnames <- col_names
  } else {
    res <- list()
    y_name <- character(0)
  }

  step_umap_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    outcome = y_name,
    neighbors = x$neighbors,
    num_comp = x$num_comp,
    min_dist = x$min_dist,
    metric = x$metric,
    learn_rate = x$learn_rate,
    epochs = x$epochs,
    options = x$options,
    seed = x$seed,
    prefix = x$prefix,
    keep_original_cols = get_keep_original_cols(x),
    retain = x$retain,
    object = res,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_umap <- function(object, new_data, ...) {
  col_names <- names(object$object$xnames)
  check_new_data(col_names, object, new_data)

  if (length(col_names) == 0) {
    return(new_data)
  }

  withr::with_seed(
    object$seed[2],
    res <-
      uwot::umap_transform(
        model = object$object,
        X = new_data[, col_names]
      )
  )

  if (is.null(object$prefix)) {
    object$prefix <- "UMAP"
  }

  colnames(res) <- names0(object$num_comp, prefix = object$prefix)
  res <- as_tibble(res)
  
  res <- check_name(res, new_data, object, names(res))
  new_data <- vec_cbind(new_data, res)

  new_data <- remove_original_cols(new_data, object, col_names)

  new_data
}

#' @export
print.step_umap <-
  function(x, width = max(20, options()$width - 31), ...) {
    title <- "UMAP embedding for "
    print_step(x$object$xnames, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_umap` object.
#' @export
tidy.step_umap <- function(x, ...) {
  if (is_trained(x)) {
    if (length(x$object$xnames) == 0) {
      res <- tibble(terms = character())
    } else {
      res <- tibble(terms = x$object$xnames)
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
required_pkgs.step_umap <- function(x, ...) {
  c("uwot", "embed")
}

#' @export
#' @rdname tunable_embed
tunable.step_umap <- function(x, ...) {
  tibble::tibble(
    name = c("num_comp", "neighbors", "min_dist", "learn_rate", "epochs"),
    call_info = list(
      list(pkg = "dials", fun = "num_comp", range = c(1, 10)),
      list(pkg = "dials", fun = "neighbors", range = c(5, 200)),
      list(pkg = "dials", fun = "min_dist", range = c(-4, -0.69897)),
      list(pkg = "dials", fun = "learn_rate"),
      list(pkg = "dials", fun = "epochs", range = c(100, 700))
    ),
    source = "recipe",
    component = "step_umap",
    component_id = x$id
  )
}
