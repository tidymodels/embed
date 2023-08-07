#' Truncated PCA Signal Extraction
#'
#' `step_pca_truncated()` creates a *specification* of a recipe step that will
#' convert numeric data into one or more principal components. It is truncated
#' as it only calculates the number of components it is asked instead of all of
#' them as is done in [recipes::step_pca()].
#'
#' @inheritParams recipes::step_pca
#' @param options A list of options to the default method for
#'   [irlba::prcomp_irlba()]. Argument defaults are set to `retx = FALSE`,
#'   `center = FALSE`, `scale. = FALSE`, and `tol = NULL`. **Note** that the
#'   argument `x` should not be passed here (or at all).
#' @param res The [irlba::prcomp_irlba()] object is stored here once this
#'   preprocessing step has be trained by [prep()].
#' @template step-return
#' @details
#'
#' Principal component analysis (PCA) is a transformation of a group of
#' variables that produces a new set of artificial features or components. These
#' components are designed to capture the maximum amount of information (i.e.
#' variance) in the original variables. Also, the components are statistically
#' independent from one another. This means that they can be used to combat
#' large inter-variables correlations in a data set.
#'
#' It is advisable to standardize the variables prior to running PCA. Here, each
#' variable will be centered and scaled prior to the PCA calculation. This can
#' be changed using the `options` argument or by using [step_center()] and
#' [step_scale()].
#'
#' The argument `num_comp` controls the number of components that will be
#' retained (the original variables that are used to derive the components are
#' removed from the data). The new components will have names that begin with
#' `prefix` and a sequence of numbers. The variable names are padded with zeros.
#' For example, if `num_comp < 10`, their names will be `PC1` - `PC9`. If
#' `num_comp = 101`, the names would be `PC001` - `PC101`.
#'
#' # Tidying
#'
#' When you [`tidy()`][tidy.recipe()] this step, use either `type = "coef"` for
#' the variable loadings per component or `type = "variance"` for how much
#' variance each component accounts for.
#' 
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_pca_truncated"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-unsupervised
#'
#' @references
#'
#' Jolliffe, I. T. (2010). *Principal Component Analysis*. Springer.
#'
#' @examples
#' rec <- recipe(~., data = mtcars)
#' pca_trans <- rec %>%
#'   step_normalize(all_numeric()) %>%
#'   step_pca_truncated(all_numeric(), num_comp = 2)
#' pca_estimates <- prep(pca_trans, training = mtcars)
#' pca_data <- bake(pca_estimates, mtcars)
#'
#' rng <- extendrange(c(pca_data$PC1, pca_data$PC2))
#' plot(pca_data$PC1, pca_data$PC2,
#'   xlim = rng, ylim = rng
#' )
#'
#' tidy(pca_trans, number = 2)
#' tidy(pca_estimates, number = 2)
#' @export
step_pca_truncated <- function(recipe,
                               ...,
                               role = "predictor",
                               trained = FALSE,
                               num_comp = 5,
                               options = list(),
                               res = NULL,
                               columns = NULL,
                               prefix = "PC",
                               keep_original_cols = FALSE,
                               skip = FALSE,
                               id = rand_id("pca_truncated")) {
  add_step(
    recipe,
    step_pca_truncated_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      num_comp = num_comp,
      options = options,
      res = res,
      columns = columns,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id,
      case_weights = NULL
    )
  )
}

step_pca_truncated_new <-
  function(terms, role, trained, num_comp, options, res, columns,
           prefix, keep_original_cols, skip, id, case_weights) {
    step(
      subclass = "pca_truncated",
      terms = terms,
      role = role,
      trained = trained,
      num_comp = num_comp,
      options = options,
      res = res,
      columns = columns,
      prefix = prefix,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }

#' @export
prep.step_pca_truncated <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))

  wts <- get_case_weights(info, training)
  were_weights_used <- are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used)) {
    wts <- NULL
  }

  if (x$num_comp > 0 && length(col_names) > 0) {
    if (is.null(wts)) {
      prc_call <-
        expr(irlba::prcomp_irlba(
          retx = FALSE,
          center = FALSE,
          scale. = FALSE
        ))
      if (length(x$options) > 0) {
        prc_call <- rlang::call_modify(prc_call, !!!x$options)
      }
      prc_call <- rlang::call_modify(prc_call, n = x$num_comp)

      prc_call$x <- expr(training[, col_names, drop = FALSE])
      prc_obj <- eval(prc_call)
    } else {
      prc_obj <- recipes::pca_wts(training[, col_names, drop = FALSE], wts = wts)
    }
    rownames(prc_obj$rotation) <- col_names
  } else {
    prc_obj <- NULL
    prc_obj$rotation <- matrix(nrow = 0, ncol = 0)
  }

  

  if (is.null(prc_obj$center)) {
    prc_obj$center <- FALSE
  }

  step_pca_truncated_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    num_comp = x$num_comp,
    options = x$options,
    res = prc_obj,
    columns = col_names,
    prefix = x$prefix,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}

#' @export
bake.step_pca_truncated <- function(object, new_data, ...) {
  if (is.null(object$columns)) {
    object$columns <- stats::setNames(nm = rownames(object$res$rotation))
  }

  if (length(object$columns) == 0 || all(is.na(object$res$rotation))) {
    return(new_data)
  }
  
  check_new_data(object$columns, object, new_data)

  pca_vars <- rownames(object$res$rotation)
  comps <- scale(new_data[, pca_vars], object$res$center, object$res$scale) %*%
    object$res$rotation
  comps <- comps[, 1:object$num_comp, drop = FALSE]
  comps <- as_tibble(comps)
  comps <- check_name(comps, new_data, object)
  new_data <- vec_cbind(new_data, comps)
  keep_original_cols <- get_keep_original_cols(object)

  if (!keep_original_cols) {
    new_data <- new_data[, !(colnames(new_data) %in% pca_vars), drop = FALSE]
  }
  
  new_data
}

#' @export
print.step_pca_truncated <-
  function(x, width = max(20, options()$width - 29), ...) {
    if (x$trained) {
      if (is.null(x$columns)) {
        x$columns <- stats::setNames(nm = rownames(x$res$rotation))
      }

      if (length(x$columns) == 0 || all(is.na(x$res$rotation))) {
        title <- "No truncated PCA components were extracted from "
        columns <- names(x$columns)
      } else {
        title <- glue("Truncated PCA extraction with ")
        columns <- rownames(x$res$rotation)
      }
    } else {
      title <- "Truncated PCA extraction with "
    }
    print_step(columns, x$terms, x$trained, title, width,
      case_weights = x$case_weights
    )
    invisible(x)
  }

pca_variances <- function(x) {
  if (x$num_comp > 0 && length(x$columns) > 0) {
    variances <- x$res$sdev^2
    p <- length(variances)
    tot <- sum(variances)
    y <- c(
      variances,
      cumsum(variances),
      variances / tot * 100,
      cumsum(variances) / tot * 100
    )
    x <-
      rep(
        c(
          "variance",
          "cumulative variance",
          "percent variance",
          "cumulative percent variance"
        ),
        each = p
      )

    res <- tibble::tibble(
      terms = x,
      value = y,
      component = rep(1:p, 4)
    )
  } else {
    res <- tibble::tibble(
      terms = unname(x$columns),
      value = rep(rlang::na_dbl, length(x$columns)),
      component = rep(rlang::na_chr, length(x$columns))
    )
  }
  res
}

#' @rdname tidy.recipe
#' @param type For `step_pca_truncated`, either `"coef"` (for the variable
#'   loadings per component) or `"variance"` (how much variance does each
#'   component account for).
#' @export
tidy.step_pca_truncated <- function(x, type = "coef", ...) {
  if (!is_trained(x)) {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      value = na_dbl,
      component = na_chr
    )
  } else {
    if (length(x$terms) == 0) {
      res <- tibble(
        terms = character(),
        value = double(),
        component = character()
      )
    } else {
      type <- match.arg(type, c("coef", "variance"))
      if (type == "coef") {
        x$res <- x$res$rotation
        res <- pca_coefs(x)
      } else {
        res <- pca_variances(x)
      }
    }
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.embed
#' @export
required_pkgs.step_pca_truncated <- function(x, ...) {
  c("embed", "irlba")
}

#' @export
tunable.step_pca_truncated <- function(x, ...) {
  tibble::tibble(
    name = c("num_comp"),
    call_info = list(
      list(pkg = "dials", fun = "num_comp", range = c(1L, 4L))
    ),
    source = "recipe",
    component = "step_pca_truncated",
    component_id = x$id
  )
}
