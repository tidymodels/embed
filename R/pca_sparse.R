#' Sparse PCA Signal Extraction
#'
#' `step_pca_sparse()` creates a *specification* of a recipe step that will convert
#'  numeric data into one or more principal components that can have some zero
#'  coefficients.
#'
#' @inheritParams step_lencode_bayes
#' @inherit step_lencode_bayes return
#' @param ... One or more selector functions to choose which variables will be
#'  used to compute the components. See [selections()] for more details. For the
#'  `tidy` method, these are not currently used.
#' @param role For model terms created by this step, what analysis role should
#'  they be assigned?. By default, the function assumes that the new principal
#'  component columns created by the original variables will be used as
#'  predictors in a model.
#' @param num_comp The number of PCA components to retain as new predictors.
#'  If `num_comp` is greater than the number of columns or the number of
#'  possible components, a smaller value will be used.
#' @param predictor_prop The maximum number of original predictors that can have
#'  non-zero coefficients for each PCA component (via regularization).
#' @param options A list of options to the default method for `irlba::ssvd()`.
#' @param res The rotation matrix once this
#'  preprocessing step has be trained by [prep.recipe()].
#' @param prefix A character string that will be the prefix to the resulting
#'  new variables. See notes below.
#' @return An updated version of `recipe` with the new step added to the
#'  sequence of existing steps (if any). For the `tidy` method, a tibble with
#'  columns `terms` (the selectors or variables selected), `value` (the
#'  loading), and `component`.
#' @keywords datagen
#' @concept preprocessing
#' @concept pca
#' @concept projection_methods
#' @export
#' @details
#' The `irlba` is required for this step. If it is not installed, the user
#'  will be prompted to do so when the step is defined. The `ssvd()` function is
#'  used to encourage sparsity; that documentation has details about this method. 
#'  
#' The argument `num_comp` controls the number of components that
#'  will be retained (the original variables that are used to derive
#'  the components are removed from the data). The new components
#'  will have names that begin with `prefix` and a sequence of
#'  numbers. The variable names are padded with zeros. For example,
#'  if `num_comp < 10`, their names will be `PC1` - `PC9`.
#'  If `num_comp = 101`, the names would be `PC001` -
#'  `PC101`.
#'
#' @examples
#' library(recipes)
#' library(ggplot2)
#' 
#' data(ad_data, package = "modeldata")
#' 
#' ad_rec <- 
#'   recipe(Class ~ ., data = ad_data) %>% 
#'   step_zv(all_predictors()) %>% 
#'   step_YeoJohnson(all_numeric_predictors()) %>% 
#'   step_normalize(all_numeric_predictors()) %>% 
#'   step_pca_sparse(all_numeric_predictors(), 
#'                   predictor_prop = 0.75,
#'                   num_comp = 3, 
#'                   id = "sparse pca") %>% 
#'   prep()
#' 
#' tidy(ad_rec, id = "sparse pca") %>% 
#'   mutate(value = ifelse(value == 0, NA, value)) %>%
#'   ggplot(aes(x = component, y = terms, fill = value)) + 
#'   geom_tile() + 
#'   scale_fill_gradient2() + 
#'   theme(axis.text.y = element_blank())
step_pca_sparse <- function(recipe,
                            ...,
                            role = "predictor",
                            trained = FALSE,
                            num_comp  = 5,
                            predictor_prop = 1.0,
                            options = list(),
                            res = NULL,
                            prefix = "PC",
                            skip = FALSE,
                            id = rand_id("pca_sparse")) {
  
  rlang:: check_installed("irlba")
  
  add_step(
    recipe,
    step_pca_sparse_new(
      terms = ellipse_check(...),
      role = role,
      trained = trained,
      num_comp = num_comp,
      predictor_prop = predictor_prop,
      options = options,
      res = res,
      prefix = prefix,
      skip = skip,
      id = id
    )
  )
}

step_pca_sparse_new <-
  function(terms, role, trained, num_comp, predictor_prop, options, res,
           prefix, skip, id) {
    step(
      subclass = "pca_sparse",
      terms = terms,
      role = role,
      trained = trained,
      num_comp = num_comp,
      predictor_prop = predictor_prop,
      options = options,
      res = res,
      prefix = prefix,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_pca_sparse <- function(x, training, info = NULL, ...) {
  col_names <- recipes:::eval_select_recipes(x$terms, training, info)
  check_type(training[, col_names])
  
  p <- length(col_names)
  x$num_comp <- min(x$num_comp, p)
  
  # Convert proportion to number of terms
  x$predictor_prop <- max(x$predictor_prop, 0.00001)
  x$predictor_prop <- min(x$predictor_prop, 1)
  num_dense <- prop2int(x$predictor_prop, p)
  
  
  if (x$num_comp > 0) {
    cl <-
      rlang::call2(
        "ssvd",
        .ns = "irlba",
        x = rlang::expr(as.matrix(training[, col_names])),
        k = x$num_comp,
        n = num_dense,
        !!!x$options
      )
    res <- rlang::eval_tidy(cl)
    rotation <- standardize_pca_coefs(res$v)
  } else {
    # fake a rotation matrix so that the resolved names can be used for tidy()
    rotation <- matrix(NA, nrow = length(col_names), ncol = p)
  }
  rownames(rotation) <- col_names
  colnames(rotation) <- names0(x$num_comp, prefix = x$prefix)
  
  step_pca_sparse_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_comp = x$num_comp,
    predictor_prop = x$predictor_prop,
    options = x$options,
    res = rotation,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_pca_sparse <- function(object, new_data, ...) {
  if (!all(is.na(object$res))) {
    pca_vars <- rownames(object$res)
    x <- as.matrix(new_data[, pca_vars])
    comps <- x %*% object$res
    comps <- check_name(comps, new_data, object)
    new_data <- bind_cols(new_data, as_tibble(comps))
    new_data <- new_data[, !(colnames(new_data) %in% pca_vars), drop = FALSE]
  }
  as_tibble(new_data)
}

#' @export
print.step_pca_sparse <-
  function(x, width = max(20, options()$width - 29), ...) {
    if (all(is.na(x$res))) {
      cat("No Sparse PCA components were extracted.\n")
    } else {
      cat("Sparse PCA extraction with ")
      printer(rownames(x$res), x$terms, x$trained, width = width)
    }
    
    invisible(x)
  }

pca_coefs <- function(x) {
  rot <- as.data.frame(x$res)
  vars <- rownames(rot)
  if (x$num_comp > 0) {
    npc <- ncol(rot)
    res <- utils::stack(rot)
    colnames(res) <- c("value", "component")
    res$component <- as.character(res$component)
    res$terms <- rep(vars, npc)
    res <- as_tibble(res)[, c("terms", "value", "component")]
  } else {
    res <- tibble::tibble(terms = vars, value = rlang::na_dbl,
                          component = rlang::na_chr)
  }
  res
}

#' @rdname step_pca_sparse
#' @param x A `step_pca_sparse` object.
#' @export
tidy.step_pca_sparse <- function(x, ...) {
  if (!is_trained(x)) {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, value = na_dbl, component  = na_chr)
  } else {
    res <- pca_coefs(x)
  }
  res$id <- x$id
  res
}



#' @rdname tunable.step
#' @export
tunable.step_pca_sparse <- function(x, ...) {
  tibble::tibble(
    name = c("num_comp", "predictor_prop"),
    call_info = list(
      list(pkg = "dials", fun = "num_comp", range = c(1L, 4L)),
      list(pkg = "dials", fun = "predictor_prop")
    ),
    source = "recipe",
    component = "step_pca_sparse",
    component_id = x$id
  )
}

# ------------------------------------------------------------------------------

standardize_pca_coefs <- function(x) {
  apply(x, 2, leading_positive)
}

leading_positive <- function(x) {
  if (x[1] < 0) {
    x <- -x
  }
  x
}


