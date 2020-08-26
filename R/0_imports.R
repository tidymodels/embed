#' @import recipes 
#' @import rlang
#' @importFrom utils globalVariables capture.output packageVersion stack compareVersion
#' @importFrom uwot umap_transform umap
#' @importFrom lme4 glmer lmer
#' @importFrom rstanarm stan_glmer
#' @importFrom keras keras_model_sequential layer_embedding layer_flatten
#' @importFrom keras layer_dense compile fit get_layer backend keras_model
#' @importFrom keras layer_concatenate layer_input
#' @importFrom stats as.formula glm binomial coef gaussian na.omit
#' @importFrom stats setNames model.matrix complete.cases
#' @importFrom purrr map
#' @importFrom tibble rownames_to_column as_tibble tibble
#' @importFrom dplyr bind_cols bind_rows mutate filter left_join %>% arrange 
#' @importFrom dplyr ends_with contains one_of
#' @importFrom dplyr tibble mutate filter left_join %>% arrange 
#' @importFrom tidyr gather
#' @importFrom withr with_seed

# ------------------------------------------------------------------------------

utils::globalVariables(
  c(
    "Feature", "Missing", "No", "Node", "Split", "Yes",
    "training", "col_names", "y_name",
    "n", "p", "predictor", "summary_outcome", "value", "woe", "select", 
    "variable", ".",
    "type", "loss", "epochs", "..level", "..order",
    "data"
    )
  )

# ------------------------------------------------------------------------------

# nocov start
.onLoad <- function(libname, pkgname) {
  # This package has specific methods for the `tunable` generic. That generic
  # is defined in the `tune` package. As of R 4.0, we need to register them.
  embed_exports <- getNamespaceExports(ns = "embed")
  names <- names(embed_exports)
  tunable_steps <- grep("tunable.step", embed_exports, fixed = TRUE, 
                        value = TRUE)
  for (i in tunable_steps) {
    s3_register("tune::tunable", i)
  }
  
  # ----------------------------------------------------------------------------
  
  req_pkgs_names <- grep("^required_pkgs\\.", names, value = TRUE)
  req_pkgs_classes <- gsub("required_pkgs.", "", req_pkgs_names)
  
  for (i in seq_along(req_pkgs_names)) {
    class <- req_pkgs_classes[[i]]
    s3_register("tune::required_pkgs", class)
  }
}

s3_register <- function(generic, class, method = NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)
  
  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]
  
  caller <- parent.frame()
  
  get_method_env <- function() {
    top <- topenv(caller)
    if (isNamespace(top)) {
      asNamespace(environmentName(top))
    } else {
      caller
    }
  }
  get_method <- function(method, env) {
    if (is.null(method)) {
      get(paste0(generic, ".", class), envir = get_method_env())
    } else {
      method
    }
  }
  
  method_fn <- get_method(method)
  stopifnot(is.function(method_fn))
  
  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(package, "onLoad"),
    function(...) {
      ns <- asNamespace(package)
      
      # Refresh the method, it might have been updated by `devtools::load_all()`
      method_fn <- get_method(method)
      
      registerS3method(generic, class, method_fn, envir = ns)
    }
  )
  
  # Avoid registration failures during loading (pkgload or regular)
  if (!isNamespaceLoaded(package)) {
    return(invisible())
  }
  
  envir <- asNamespace(package)
  
  # Only register if generic can be accessed
  if (exists(generic, envir)) {
    registerS3method(generic, class, method_fn, envir = envir)
  }
  
  invisible()
}

# nocov end
