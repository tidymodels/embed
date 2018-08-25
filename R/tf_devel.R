#' Encoding Factors into Multiple Columns
#'
#' `step_embed2` creates a *specification* of a recipe step that
#'  will convert a nominal (i.e. factor) predictor into a set of
#'  scores derived from a tensorflow model via a word-embedding model.
#'  `embed_control` is a simple wrapper for setting default options. 
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'  For `step_embed2`, this indicates the variables to be encoded
#'  into a numeric format. See [recipes::selections()] for more
#'  details. For the `tidy` method, these are not currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param outcome A call to `vars` to specify which variable is
#'  used as the outcome in the neural network. Only
#'  numeric and two-level factors are currently supported.
#' @param predictors An optional call to `vars` to specify any
#'  variables to be added as additional predictors in the neural
#'  network. These variables should be numeric and perhaps centered
#'  and scaled.
#' @param num_terms An integer for the number of resulting variables.
#' @param hidden_units An integer for the number of hidden units
#'  in a dense ReLu layer between the embedding and output later.
#'  Use a value of zero for no intermediate layer (see Details
#'  below).
#' @param options A list of options for the model fitting process.
#' @param mapping A list of tibble results that define the
#'  encoding. This is `NULL` until the step is trained by
#'  [recipes::prep.recipe()].
#' @param history A tibble with the convergence statistics for
#'  each term. This is `NULL` until the step is trained by
#'  [recipes::prep.recipe()].
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [recipes::bake.recipe()]? While all
#'  operations are baked when [recipes::prep.recipe()] is run, some
#'  operations may not be able to be conducted on new data (e.g.
#'  processing the outcome variable(s)). Care should be taken when
#'  using `skip = TRUE` as it may affect the computations for
#'  subsequent operations.
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
#' @return An updated version of `recipe` with the new step added
#'  to the sequence of existing steps (if any). For the `tidy`
#'  method, a tibble with columns `terms` (the selectors or
#'  variables for encoding), `level` (the factor levels), and
#'  several columns containing `embed` in the name.
#' @keywords datagen
#' @concept preprocessing encoding
#' @export
#' @details Factor levels are initially assigned at random to the
#'  new variables and these variables are used in a neural network
#'  to optimize both the allocation of levels to new columns as well
#'  as estimating a model to predict the outcome. See Section 6.1.2
#'  of Francois and Allaire (2018) for more details.
#'
#'  The new variables are mapped to the specific levels seen at the
#'  time of model training and an extra instance of the variables
#'  are used for new levels of the factor.
#'
#' This model is trained separately for each factor predictor
#'  given in the recipe step.
#' 
#' When the outcome is numeric, a linear activation function is
#'  used in the last layer while softmax is used for factor outcomes
#'  (with any number of levels).
#'  
#' For example, the `keras` code for a numeric outcome, one
#'  categorical predictor, and no hidden units used here would be
#' 
#' ```
#'   keras_model_sequential() \\%>\\% 
#'   layer_embedding(
#'     input_dim = num_factor_levels_x + 1,
#'     output_dim = num_terms,
#'     input_length = 1
#'   ) \\%>\\%
#'   layer_flatten() \\%>\\%
#'   layer_dense(units = 1, activation = 'linear')
#' ```
#'
#' If a factor outcome is used and 10 hidden units were requested, the code 
#' would be
#'
#' ```
#'   keras_model_sequential() \\%>\\% 
#'   layer_embedding(
#'     input_dim = num_factor_levels_x + 1,
#'     output_dim = num_terms,
#'     input_length = 1
#'    ) \\%>\\%
#'   layer_flatten() \\%>\\%
#'   layer_dense(units = hidden_units, activation = "relu") \\%>\\%
#'   layer_dense(units = num_factor_levels_y, activation = 'softmax')
#' ```
#' 
#' Other variables specified by `predictors` are added as an
#'  additional dense layer after `layer_flatten` and before the
#'  hidden layer.
#'
#' Also note that it may be difficult to obtain reproducible
#'  results using this step due to the nature of Tensorflow (see
#'  link in References).
#' 
#' tensorflow models cannot be run in parallel within the same 
#'  session (via `foreach`) or the `parallel` package. If using a
#'  recipes with this step with `caret`, avoid parallel processing. 
#' 
#' @references Francois C and Allaire JJ (2018) 
#' _Deep Learning with R_, Manning
#' 
#' "How can I obtain reproducible results using Keras during 
#' development?" \url{https://tinyurl.com/keras-repro}
#' 
#' @examples
#' data(okc)
#' 
#' rec <- recipe(Class ~ age + location, data = okc) %>%
#'   step_embed2(location, outcome = vars(Class),
#'              options = embed_control(epochs = 10))
#' 
#' # See https://tidymodels.github.io/embed/ for examples


#' @importFrom recipes add_step step terms_select sel2char ellipse_check
step_embed2 <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           outcome = NULL,
           predictors = NULL,
           num_terms = 2,
           hidden_units = 0,
           options = embed_control(),
           mapping = NULL,
           history = NULL,
           skip = FALSE) {
    if (is.null(outcome))
      stop("Please list a variable in `outcome`", call. = FALSE)
    add_step(
      recipe,
      step_embed2_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        outcome = outcome,
        predictors = predictors,
        num_terms = num_terms,
        hidden_units = hidden_units,
        options = options,
        mapping = mapping,
        history = history,
        skip = skip
      )
    )
  }

step_embed2_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           outcome = NULL,
           predictors = NULL,
           num_terms = NULL,
           hidden_units = NULL,
           options = NULL,
           mapping = NULL,
           history = NULL,
           skip = FALSE) {
    step(
      subclass = "embed2",
      terms = terms,
      role = role,
      num_terms = num_terms,
      hidden_units = hidden_units,
      options = options,
      trained = trained,
      outcome = outcome,
      predictors = predictors,
      mapping = mapping,
      history = history,
      skip = skip
    )
  }

#' @importFrom recipes check_type
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @export
prep.step_embed2 <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  check_type(training[, col_names], quant = FALSE)
  y_name <- terms_select(x$outcome, info = info)
  if (length(x$predictors) > 0) {
    pred_names <- terms_select(x$predictors, info = info)
    check_type(training[, pred_names], quant = TRUE)
  }
  else
    pred_names <- NULL
  
  x$options <- tf_options_check(x$options)
  res <-
    tf_coefs2(
      x = training[, col_names], 
      y = training[, y_name], 
      z = if(is.null(pred_names)) NULL else training[, pred_names],
      opt = x$options,
      num = x$num_terms,
      h = x$hidden_units
    )

  x$mapping <- res$layer_values
  x$history <- 
    as_tibble(res$history$metrics) %>%
    mutate(epochs = 1:res$history$params$epochs) %>%
    gather(type, loss, -epochs)
  x$trained <- TRUE
  x
}

#' @importFrom keras keras_model_sequential layer_embedding layer_flatten
#' @importFrom keras layer_dense compile fit get_layer backend keras_model
#' @importFrom keras layer_concatenate layer_input
#' @importFrom dplyr bind_cols as_tibble ends_with
#' @importFrom stats setNames
tf_coefs2 <- function(x, y, z, opt, num, lab, h, ...) {
  vars <- names(x)
  p <- length(vars)
  
  on.exit(keras::backend()$clear_session())
  
  lvl <- lapply(x, levels)
  
  mats <- lapply(x, function(x) matrix(as.numeric(x), ncol = 1))
  
  y <- y[[1]]
  if(is.character(y))
    y <- as.factor(y)
  factor_y <- is.factor(y)
  
  if (factor_y)
    y <- class2ind(y)
  else
    y <- matrix(y, ncol = 1)
  
  for(i in 1:p) {
    tmp_in <- layer_input(shape = 1, name = paste0("input_", vars[i]))
    if(i == 1)
      inputs <- tmp_in
    else 
      inputs <- c(inputs, tmp_in)
  }
  
  for(i in 1:p) {
    tmp_emb <- 
      inputs[[i]] %>%
      layer_embedding(
        input_dim = length(lvl[[i]]) + 1,
        output_dim = num,
        input_length = 1,
        name = paste0("layer_", vars[i])
      ) %>%  
      layer_flatten()
    if(i == 1)
      layers <- tmp_emb
    else 
      layers <- c(layers, tmp_emb)
  } 
  
  if (is.null(z)) {
    model <- layer_concatenate(layers)
  } else {
    mats$z <- as.matrix(z)
    pred_layer <- layer_input(shape = ncol(z), name = 'other_pred')
    model <- layer_concatenate(c(layers, pred_layer))
    inputs <- c(inputs, pred_layer)
  }
  
  if (h > 0)
    model <- model %>%
    layer_dense(units = h, activation = "relu", name = "hidden_layer")
  
  if (factor_y)
    model <- model %>%
    layer_dense(units = ncol(y), activation = 'softmax', name = "output_layer")
  else
    model <- model %>%
    layer_dense(units = 1, activation = 'linear', name = "output_layer")
  
  model <-
    keras::keras_model(inputs = inputs, outputs = model)  
  
  model <- model %>%
    compile(
      loss = opt$loss,
      metrics = opt$metrics,
      optimizer = opt$optimizer
    )
  
  history <- 
    model %>% 
    fit(
      x = unname(mats),
      y = y,
      epochs = opt$epochs,
      validation_split = opt$validation_split,
      batch_size = opt$batch_size,
      verbose = opt$verbose
    )

  layer_values <- vector(mode = "list", length = p)
  
  for (i in 1:p) {
    layer_values[[i]] <-
      get_layer(model, paste0("layer_", vars[i]))$get_weights() %>%
      as.data.frame() %>%
      setNames(recipes::names0(num, paste0(vars[i], "_embed_"))) %>%
      as_tibble() %>%
      mutate(..level = c("..new", lvl[[i]]))
  }
  names(layer_values) <- vars
  
  list(layer_values = layer_values, history = history)
}

#' @importFrom dplyr tibble mutate filter left_join %>% arrange contains
#' @importFrom stats complete.cases
#' @importFrom recipes names0
map_tf_coef2 <- function(dat, mapping, prefix) {
  new_val <- mapping %>%
    dplyr::filter(..level == "..new") %>%
    dplyr::select(-..level)
  dat <- dat %>% 
    mutate(..order = 1:nrow(dat)) %>%
    set_names(c("..level", "..order")) %>%
    mutate(..level = as.character(..level))
  mapping <- mapping %>% dplyr::filter(..level != "..new")
  dat <- left_join(dat, mapping, by = "..level") %>%
    arrange(..order)
  
  dat <- dat %>% dplyr::select(contains("_embed"))
  dat[!complete.cases(dat),] <- new_val
  dat
  
}

#' @import rlang
#' @importFrom recipes bake prep
#' @importFrom purrr map
#' @importFrom dplyr bind_cols
#' @export
bake.step_embed2 <- function(object, newdata, ...) {
  for (col in names(object$mapping)) {
    tmp <- map_tf_coef2(newdata[, col], object$mapping[[col]], prefix = col)
    newdata <- bind_cols(newdata, tmp)
    rm(tmp)
  }
  newdata <- newdata[, !(names(newdata) %in% names(object$mapping))]
  
  newdata
}

#' @importFrom recipes printer
#' @export
print.step_embed2 <-
  function(x, width = max(20, options()$width - 31), ...) {
    cat("Embedding of factors via tensorflow for ", sep = "")
    printer(names(x$mapping), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @importFrom utils globalVariables
utils::globalVariables(
  c("type", "loss", "epochs")
)

