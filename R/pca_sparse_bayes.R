#' Sparse, Bayesian PCA Signal Extraction
#'
#' `step_pca_sparse_bayes()` creates a *specification* of a recipe step that will convert
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
#'  possible components, a smaller value will be used. A value of zero indicates
#'  that PCA will _not_ be used on the data. 
#' @param prior_slab_dispersion This value is proportional to the dispersion 
#' (or scale) parameter for the slab portion of the prior. Smaller values result 
#' in an increase in zero coefficients.
#' @param prior_mixture_threshold The parameter that trades-off the spike and
#' slab components of the prior. Increasing this parameter increases the number
#' if zero coefficients. 
#' @param options A list of options to the default method for 
#' `VBsparsePCA::VBsparsePCA()`.
#' @param res The rotation matrix once this preprocessing step has be trained 
#' by [prep.recipe()].
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
#' @references Ning, B. (2021). Spike and slab Bayesian sparse principal 
#' component analysis. arXiv:2102.00305.
#' @export
#' @details
#' The `VBsparsePCA` is required for this step. If it is not installed, the user
#'  will be prompted to do so when the step is defined. 
#' 
#' A spike-and-slab prior is a mixture of two priors. One (the "spike") has
#'  all of its mass at zero and represents a variable that has no contribution
#'  to the PCA coefficients. The other prior is a broader distribution that
#'  reflects the coefficient distribution of variables that do affect the PCA
#'  analysis. This is the "slab". The narrower the slab, the more likely that a
#'  coefficient will be zero (or are regularized to be closer to zero). The
#'  mixture of these two priors is governed by a mixing parameter, which itself
#'  has a prior distribution and a hyper-parameter prior.
#'  
#' PCA coefficients and their resulting scores are unique only up to sign. This 
#' step will attempt to make the sign of the components more consistent from 
#' run-to-run. However, the sparsity constraint may interfere with this goal.  
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
#'   step_pca_sparse_bayes(all_numeric_predictors(), 
#'                         prior_mixture_threshold = 0.95,
#'                         prior_slab_dispersion = 0.05, 
#'                         num_comp = 3, 
#'                         id = "sparse bayesian pca") %>% 
#'   prep()
#' 
#' tidy(ad_rec, id = "sparse bayesian pca") %>% 
#'   mutate(value = ifelse(value == 0, NA, value)) %>%
#'   ggplot(aes(x = component, y = terms, fill = value)) + 
#'   geom_tile() + 
#'   scale_fill_gradient2() + 
#'   theme(axis.text.y = element_blank())
step_pca_sparse_bayes <- function(recipe,
                                  ...,
                                  role = "predictor",
                                  trained = FALSE,
                                  num_comp  = 5,
                                  prior_slab_dispersion = 1.0,
                                  prior_mixture_threshold = 0.1,
                                  options = list(),
                                  res = NULL,
                                  prefix = "PC",
                                  skip = FALSE,
                                  id = rand_id("pca_sparse_bayes")) {
  
  rlang:: check_installed("VBsparsePCA")
  
  add_step(
    recipe,
    step_pca_sparse_bayes_new(
      terms = ellipse_check(...),
      role = role,
      trained = trained,
      num_comp = num_comp,
      prior_slab_dispersion = prior_slab_dispersion,
      prior_mixture_threshold = prior_mixture_threshold,
      options = options,
      res = res,
      prefix = prefix,
      skip = skip,
      id = id
    )
  )
}

step_pca_sparse_bayes_new <-
  function(terms, role, trained, num_comp, prior_slab_dispersion, 
           prior_mixture_threshold, options, res, prefix, skip, id) {
    step(
      subclass = "pca_sparse_bayes",
      terms = terms,
      role = role,
      trained = trained,
      num_comp = num_comp,
      prior_slab_dispersion = prior_slab_dispersion,
      prior_mixture_threshold = prior_mixture_threshold,
      options = options,
      res = res,
      prefix = prefix,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_pca_sparse_bayes <- function(x, training, info = NULL, ...) {
  col_names <- recipes:::eval_select_recipes(x$terms, training, info)
  check_type(training[, col_names])
  
  p <- length(col_names)
  x$num_comp <- min(x$num_comp, p)
  
  # Check range again
  x$prior_mixture_threshold <- max(x$prior_mixture_threshold, 0.00001)
  x$prior_mixture_threshold <- min(x$prior_mixture_threshold, 1)
  
  scale_param <- 1/x$prior_slab_dispersion
  
  if (x$num_comp > 0) {
    cl <-
      rlang::call2(
        "VBsparsePCA",
        .ns = "VBsparsePCA",
        dat = rlang::expr(as.matrix(training[, col_names])),
        r = x$num_comp,
        lambda = scale_param,
        threshold = x$prior_mixture_threshold,
        !!!x$options
      )
    res <- rlang::eval_tidy(cl)
    rotation <- standardize_pca_coefs(svd(res$loadings)$u)
  } else {
    # fake a rotation matrix so that the resolved names can be used for tidy()
    rotation <- matrix(NA, nrow = length(col_names), ncol = p)
  }
  rownames(rotation) <- col_names
  colnames(rotation) <- names0(x$num_comp, prefix = x$prefix)
  
  step_pca_sparse_bayes_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_comp = x$num_comp,
    prior_slab_dispersion = x$prior_slab_dispersion,
    prior_mixture_threshold = x$prior_mixture_threshold,
    options = x$options,
    res = rotation,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_pca_sparse_bayes <- function(object, new_data, ...) {
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
print.step_pca_sparse_bayes <-
  function(x, width = max(20, options()$width - 29), ...) {
    if (all(is.na(x$res))) {
      cat("No Sparse PCA components were extracted.\n")
    } else {
      cat("Sparse PCA extraction with ")
      printer(rownames(x$res), x$terms, x$trained, width = width)
    }
    
    invisible(x)
  }

#' @rdname step_pca_sparse_bayes
#' @param x A `step_pca_sparse_bayes` object.
#' @export
tidy.step_pca_sparse_bayes <- function(x, ...) {
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
tunable.step_pca_sparse_bayes <- function(x, ...) {
  tibble::tibble(
    name = c("num_comp", "prior_slab_dispersion", "prior_mixture_threshold"),
    call_info = list(
      list(pkg = "dials", fun = "num_comp", range = c(1L, 4L)),
      list(pkg = "dials", fun = "prior_slab_dispersion"),
      list(pkg = "dials", fun = "prior_mixture_threshold")
    ),
    source = "recipe",
    component = "step_pca_sparse_bayes",
    component_id = x$id
  )
}
