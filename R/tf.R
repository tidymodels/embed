#' Encoding Factors into Multiple Columns
#'
#' `step_embed` creates a *specification* of a recipe step that
#'  will convert a nominal (i.e. factor) predictor into a set of
#'  scores derived from a tensorflow model via a word-embedding model.
#'  `embed_control` is a simple wrapper for setting default options. 
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'  For `step_embed`, this indicates the variables to be encoded
#'  into a numeric format. See [recipes::selections()] for more
#'  details. For the `tidy` method, these are not currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the embedding variables created will be used as predictors in a model.
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
#' @param id A character string that is unique to this step to identify it.
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
#' One model is created for each call to `step_embed`. All terms
#'  given to the step are estimated and encoded in the same model
#'  which would also contain predictors give in `predictors` (if 
#'  any). 
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
#' If a factor outcome is used and hidden units were requested, the code 
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
#'  session (via `foreach` or `futures`) or the `parallel` package.
#'  If using a recipes with this step with `caret`, avoid parallel
#'  processing.
#' 
#' @references Francois C and Allaire JJ (2018) 
#' _Deep Learning with R_, Manning
#' 
#' "How can I obtain reproducible results using Keras during 
#' development?" \url{https://tinyurl.com/keras-repro}
#' 
#' "Concatenate Embeddings for Categorical Variables with Keras" 
#'  \url{https://flovv.github.io/Embeddings_with_keras_part2/}
#' 
#' @examples
#' data(okc)
#' 
#' rec <- recipe(Class ~ age + location, data = okc) %>%
#'   step_embed(location, outcome = vars(Class),
#'              options = embed_control(epochs = 10))
#' 
#' # See https://tidymodels.github.io/embed/ for examples


#' @importFrom recipes add_step step terms_select sel2char ellipse_check
step_embed <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           outcome = NULL,
           predictors = NULL,
           num_terms = 2,
           hidden_units = 0,
           options = embed_control(),
           mapping = NULL,
           history = NULL,
           skip = FALSE,
           id = rand_id("lencode_bayes")) {
    if (is.null(outcome))
      stop("Please list a variable in `outcome`", call. = FALSE)
    add_step(
      recipe,
      step_embed_new(
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
        skip = skip,
        id = id
      )
    )
  }

step_embed_new <-
  function(terms, role, trained, outcome, predictors, num_terms, hidden_units,
           options, mapping, history, skip, id) {
    step(
      subclass = "embed",
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
      skip = skip,
      id = id
    )
  }

#' @importFrom recipes check_type
#' @importFrom dplyr bind_rows
#' @importFrom tibble as_tibble
#' @export
prep.step_embed <- function(x, training, info = NULL, ...) {
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
  
  # compute epochs actuually trained for
  epochs <- min(res$history$params$epochs, length(res$history$metrics[[1]]))

  step_embed_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    outcome = x$outcome,
    predictors = x$predictors,
    num_terms = x$num_terms,
    hidden_units = x$hidden_units,
    options = x$options,
    mapping = res$layer_values,
    history = 
      as_tibble(res$history$metrics) %>%
      mutate(epochs = 1:epochs) %>%
      gather(type, loss, -epochs),
    skip = x$skip,
    id = x$id
  )
}

#' @importFrom keras keras_model_sequential layer_embedding layer_flatten
#' @importFrom keras layer_dense compile fit get_layer backend keras_model
#' @importFrom keras layer_concatenate layer_input
#' @importFrom tensorflow use_session_with_seed
#' @importFrom dplyr bind_cols as_tibble ends_with
#' @importFrom stats setNames
tf_coefs2 <- function(x, y, z, opt, num, lab, h, seeds = sample.int(10000, 4), ...) {
  vars <- names(x)
  p <- length(vars)
  
  set.seed(seeds[1])
  tensorflow::use_session_with_seed(seeds[2])
  
  on.exit(keras::backend()$clear_session())
  
  lvl <- lapply(x, levels)
  
  # convert levels to integers; zero signifies a new level 
  mats <- lapply(x, function(x) matrix(as.numeric(x), ncol = 1))
  
  y <- y[[1]]
  if(is.character(y))
    y <- as.factor(y)
  factor_y <- is.factor(y)
  
  if (factor_y)
    y <- class2ind(y)
  else
    y <- matrix(y, ncol = 1)
  
  inputs <- vector(mode = "list", length = p)
  # For each categorical predictor, make an input layer
  for(i in 1:p) {
    inputs[[i]] <- layer_input(shape = 1, name = paste0("input_", vars[i]))
  }
  
  layers <- vector(mode = "list", length = p)
  # Now add embedding to each layer and then flatten
  for(i in 1:p) {
    layers[[i]] <- 
      inputs[[i]] %>%
      layer_embedding(
        input_dim = length(lvl[[i]]) + 1,
        output_dim = num,
        input_length = 1,
        name = paste0("layer_", vars[i])
      ) %>%  
      layer_flatten()
  } 
  
  if (is.null(z)) {
    if (p > 1)
      all_layers <- layer_concatenate(layers)
    else
      all_layers <- layers[[1]]
  } else {
    mats$z <- as.matrix(z)
    pred_layer <- layer_input(shape = ncol(z), name = 'other_pred')
    all_layers <- layer_concatenate(c(layers, pred_layer))
    inputs <- c(inputs, pred_layer)
  }
  
  if (h > 0)
    all_layers <- 
    all_layers %>%
    layer_dense(units = h, activation = "relu", name = "hidden_layer",
                kernel_initializer = keras::initializer_glorot_uniform(seed = seeds[3]))
  
  if (factor_y)
    all_layers <- 
    all_layers %>%
    layer_dense(units = ncol(y), activation = 'softmax', name = "output_layer",
                kernel_initializer = keras::initializer_glorot_uniform(seed = seeds[4]))
  else
    all_layers <- 
    all_layers %>%
    layer_dense(units = 1, activation = 'linear', name = "output_layer",
                kernel_initializer = keras::initializer_glorot_uniform(seed = seeds[4]))
  
  model <-
    keras::keras_model(inputs = inputs, outputs = all_layers)  
  
  model %>%
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
      verbose = opt$verbose,
      callbacks = opt$callbacks
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
bake.step_embed <- function(object, new_data, ...) {
  for (col in names(object$mapping)) {
    tmp <- map_tf_coef2(new_data[, col], object$mapping[[col]], prefix = col)
    new_data <- bind_cols(new_data, tmp)
    rm(tmp)
  }
  new_data <- new_data[, !(names(new_data) %in% names(object$mapping))]
  
  new_data
}

#' @importFrom dplyr bind_rows 
#' @importFrom tidyr gather
#' @importFrom recipes is_trained
#' @rdname step_embed
#' @param x A `step_embed` object.
#' @export
#' @export tidy.step_embed
tidy.step_embed <- function(x, ...) {
  if (is_trained(x)) {
    for(i in seq_along(x$mapping))
      x$mapping[[i]]$terms <- names(x$mapping)[i]
    res <- bind_rows(x$mapping)
    names(res) <- gsub("^\\.\\.", "", names(res))
    
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      level = rep(na_chr, length(term_names)),
      value = rep(na_dbl, length(term_names)),
      terms = term_names
    )
  }
  res$id <- x$id
  res
}


#' @importFrom recipes printer
#' @export
print.step_embed <-
  function(x, width = max(20, options()$width - 31), ...) {
    cat("Embedding of factors via tensorflow for ", sep = "")
    printer(names(x$mapping), x$terms, x$trained, width = width)
    invisible(x)
  }




#' @export
#' @rdname step_embed
#' @param optimizer,loss,metrics Arguments to pass to [keras::compile()]
#' @param epochs,validation_split,batch_size,verbose,callbacks Arguments to pass to [keras::fit()]
embed_control <- function(
  loss = "mse",
  metrics = NULL,
  optimizer = "sgd",
  epochs = 20,
  validation_split = 0,
  batch_size = 32,
  verbose = 0,
  callbacks = NULL
) {
  if(batch_size < 1)
    stop("`batch_size` should be a positive integer", call. = FALSE)
  if(epochs < 1)
    stop("`epochs` should be a positive integer", call. = FALSE)  
  if(validation_split < 0 | validation_split > 1)
    stop("`validation_split` should be on [0, 1)", call. = FALSE)
  list(
    loss = loss, metrics = metrics, optimizer = optimizer, epochs = epochs, 
    validation_split = validation_split, batch_size = batch_size,
    verbose = verbose, callbacks = callbacks)
}

tf_options_check <- function(opt) {
  exp_names <- c('loss',
                 'metrics',
                 'optimizer',
                 'epochs',
                 'validation_split',
                 'batch_size',
                 'verbose')
  
  if (length(setdiff(exp_names, names(opt))) > 0)
    stop("The following options are missing from the `options`: ",
         paste0(setdiff(exp_names, names(opt)), collapse = ",")) 
  opt
}


#' @importFrom stats model.matrix
class2ind <- function (x)  {
  if (!is.factor(x)) 
    stop("'x' should be a factor")
  y <- model.matrix(~x - 1)
  colnames(y) <- gsub("^x", "", colnames(y))
  attributes(y)$assign <- NULL
  attributes(y)$contrasts <- NULL
  y
}


#' @importFrom utils globalVariables
utils::globalVariables(
  c("type", "loss", "epochs", "..level", "..order")
)

