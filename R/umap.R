#' Supervised and unsupervised uniform manifold approximation and projection (UMAP) 
#'
#' `step_umap` creates a *specification* of a recipe step that
#'  will project a set of features into a smaller space. 
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables. For
#'  `step_umap`, this indicates the variables to be encoded into a numeric
#'  format. Numeric and factor variables can be used. See
#'  [recipes::selections()] for more details. For the `tidy` method, these are
#'  not currently used.
#' @param role For model terms created by this step, what analysis role should
#'  they be assigned?. By default, the function assumes that the new embedding
#'  columns created by the original variables will be used as predictors in a
#'  model.
#' @param min_dist The effective minimum distance between embedded points.
#' @param num_comp An integer for the number of UMAP components. 
#' @param neighbors An integer for the number of nearest neighbors used to construct 
#'  the target simplicial set.
#' @param epochs Number of iterations for the neighbor optimization. 
#' @param learn_rate Positive number of the learning rate for the optimization
#'  process. 
#' @param outcome A call to `vars` to specify which variable is
#'  used as the outcome in the encoding process (if any).
#' @param options A list of options to pass to [uwot::umap()]. The arguments
#'  `X`, `n_neighbors`, `n_components`, `min_dist`, `n_epochs`, `ret_model`, and
#'  `learning_rate` should not be passed here. By default, `verbose` and
#'  `n_threads` are set.
#' @param seed Two integers to control the random numbers used by the
#'  numerical methods. The default pulls from the main session's stream of
#'  numbers and will give reproducible results if the seed is set prior to
#'  calling [prep.recipe()] or [bake.recipe()].
#' @param retain A single logical for whether the original predictors should
#'  be kept (in addition to the new embedding variables).
#' @param object A list of tibble results that define the encoding. This is
#'  `NULL` until the step is trained by [recipes::prep.recipe()].
#' @param skip A logical. Should the step be skipped when the recipe is baked
#'  by [recipes::bake.recipe()]? While all operations are baked when
#'  [recipes::prep.recipe()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)). Care should
#'  be taken when using `skip = TRUE` as it may affect the computations for
#'  subsequent operations
#' @param trained A logical to indicate if the quantities for preprocessing
#'  have been estimated.
#' @param id A character string that is unique to this step to identify it.
#' @return An updated version of `recipe` with the new step added to the
#'  sequence of existing steps (if any). For the `tidy` method, a tibble with a
#'  column called `terms` (the selectors or variables for embedding) is
#'  returned.
#' @keywords datagen 
#' @concept preprocessing encoding
#' @export
#' @details For each factor predictor
#' 
#' 
#' @references 
#' McInnes, L., & Healy, J. (2018). UMAP: Uniform Manifold Approximation and 
#'  Projection for Dimension Reduction. \url{ https://arxiv.org/abs/1802.03426}.
#' 
#' @examples
#' library(recipes)
#' library(dplyr)
#' library(ggplot2)
#' 
#' set.seed(1957)
#' split <- sample(1:150, size = 100)
#' tr <- iris[ split, ]
#' te <- iris[-split, ]
#' 
#' set.seed(11)
#' sup <- 
#'   recipe(Species ~ ., data = tr) %>%
#'   step_center(all_predictors()) %>% 
#'   step_scale(all_predictors()) %>% 
#'   step_umap(all_predictors(), outcome = vars(Species), num_comp = 2) %>% 
#'   prep(training = tr)
#' 
#' bake(sup, new_data = te, Species, starts_with("umap")) %>% 
#'   ggplot(aes(x = umap_1, y = umap_2, col = Species)) + 
#'   geom_point(alpha = .5) 
#' @importFrom recipes add_step step terms_select sel2char ellipse_check rand_id
step_umap <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           outcome = NULL,
           neighbors = 15,
           num_comp = 2,
           min_dist = 0.01,
           learn_rate = 1,
           epochs = NULL,
           options = list(verbose = FALSE, n_threads = 1),
           seed = sample(10^5, 2),
           retain = FALSE,
           object = NULL,
           skip = FALSE,
           id = rand_id("umap")) {
    
    recipes::recipes_pkg_check("uwot")
    if (is.numeric(seed) & !is.integer(seed)) {
      seed <- as.integer(seed)
    }
    if (length(seed) != 2) {
      stop("Two integers are required for `seed`.", call. = FALSE)
    }

    add_step(
      recipe,
      step_umap_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        outcome = outcome,
        neighbors = neighbors,
        num_comp = num_comp,
        min_dist = min_dist,
        learn_rate = learn_rate,
        epochs = epochs,
        options = options,
        seed = seed,
        retain = retain,
        object = object,
        skip = skip,
        id = id
      )
    )
  }

# TODO sep options for fit and predict, rm tr embeddings, 

step_umap_new <-
  function(terms, role, trained, outcome, neighbors, num_comp, min_dist, 
           learn_rate, epochs, options, seed, retain, object, skip, id) {
    step(
      subclass = "umap",
      terms = terms,
      role = role,
      trained = trained,
      outcome = outcome,
      neighbors = neighbors,
      num_comp = num_comp,
      min_dist = min_dist,
      learn_rate = learn_rate,
      epochs = epochs,
      options = options,
      seed = seed,
      retain = retain,
      object = object,
      skip = skip,
      id = id
    )
  }

#' @importFrom uwot umap
umap_fit_call <- function(obj, y = NULL) {
  cl <- rlang::call2("umap", .ns = "uwot", X = rlang::expr(training[, col_names]))
  if (!is.null(y)) {
    cl$y <- rlang::expr(training[[y_name]])
  }
  cl$n_neighbors <- obj$neighbors
  cl$n_components <- obj$num_comp
  cl$n_epochs <- obj$epochs
  cl$learning_rate <- obj$learn_rate
  cl$min_dist <- obj$min_dist
  if (length(obj$options) > 0) {
    cl <- rlang::call_modify(cl, !!!obj$options)
  }
  cl$ret_model <- TRUE
  cl
}


#' @importFrom recipes check_type
#' @importFrom withr with_seed
#' @export
prep.step_umap <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  if (length(x$outcome) > 0) {
    y_name <- terms_select(x$outcome, info = info)
  } else {
    y_name <- NULL
  }
  withr::with_seed(
    x$seed[1],
    res <- rlang::eval_tidy(umap_fit_call(x, y = y_name))
  )
  res$xnames <- col_names
  
  step_umap_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    outcome = y_name,
    neighbors = x$neighbors,
    num_comp = x$num_comp,
    min_dist = x$min_dist,
    learn_rate = x$learn_rate,
    epochs = x$epochs,
    options = x$options,
    seed = x$seed,
    retain = x$retain,
    object = res,
    skip = x$skip,
    id = x$id
  )
}

#' @import rlang
#' @importFrom recipes bake prep
#' @importFrom uwot umap_transform
#' @importFrom purrr map
#' @export
bake.step_umap <- function(object, new_data, ...) {
  withr::with_seed(
    object$seed[2],
    res <-
      uwot::umap_transform(
        model = object$object, 
        X = new_data[, object$object$xnames]
      )
  )
  
  res <- dplyr::as_tibble(res)
  res <- setNames(res, names0(ncol(res), "umap_"))
  
  new_data <- bind_cols(new_data, res)
  if (!object$retain) {
    new_data[, object$object$xnames] <- NULL
  }
  new_data
}

#' @importFrom recipes printer
#' @export
print.step_umap <-
  function(x, width = max(20, options()$width - 31), ...) {
    cat("UMAP embedding for ", sep = "")
    printer(x$object$xnames, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @importFrom dplyr bind_rows
#' @importFrom recipes is_trained
#' @rdname step_umap
#' @param x A `step_umap` object.
#' @export
#' @export tidy.step_umap
tidy.step_umap <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$object$xnames)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res$id <- x$id
  res
}

#' @importFrom utils globalVariables
utils::globalVariables(
  c("training", "col_names", "y_name")
)


