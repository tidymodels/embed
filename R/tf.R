#' Encoding Factors into Multiple Columns
#'
#' `step_tfembed` creates a *specification* of a recipe step that
#'  will convert a nominal (i.e. factor) predictor into a set of
#'  scores derived from a tensorflow model via a word-embedding model.
#'  `tfembed_control` is a simple wrapper for setting default options. 
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables.
#'  For `step_tfembed`, this indicates the variables to be encoded
#'  into a numeric format. See [recipes::selections()] for more details. For
#'  the `tidy` method, these are not currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param outcome A call to `vars` to specify which variable is
#'  used as the outcome in the generalized linear model. Only
#'  numeric and two-level factors are currently supported.
#' @param number An integer for the number of resulting variables.
#' @param hidden An integer for the number of hidden units in a dense ReLu
#'  layer between the embedding and output later. Use a 
#'  value of zero for no intermediate layer (see Details below). 
#' @param options A list of options for the model fitting process.
#' @param mapping A list of tibble results that define the
#'  encoding. This is `NULL` until the step is trained by
#'  [recipes::prep.recipe()].
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [recipes::bake.recipe()]? While all operations are baked
#'  when [recipes::prep.recipe()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
#' @return An updated version of `recipe` with the new step added
#'  to the sequence of existing steps (if any). For the `tidy`
#'  method, a tibble with columns `terms` (the selectors or
#'  variables for encoding), `level` (the factor levels), and
#'  `value` (the encodings).
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
#' For example, the `keras` code for a numeric outcome and no hidden units used
#' here would be
#' 
#' ```
#'  keras_model_sequential() \\%>\\% 
#'  layer_embedding(
#'    input_dim = num_factor_levels_x + 1,
#'    output_dim = number,
#'    input_length = 1
#'  ) \\%>\\%
#'    layer_flatten() \\%>\\%
#'    layer_dense(units = 1, activation = 'linear')
#' ```
#'
#' If a factor outcome is used and 10 hidden units were requested, the code 
#' would be
#'
#' ```
#'  keras_model_sequential() \\%>\\% 
#'  layer_embedding(
#'    input_dim = num_factor_levels_x + 1,
#'    output_dim = number,
#'    input_length = 1
#'  ) \\%>\\%
#'    layer_flatten() \\%>\\%
#'    layer_dense(units = hidden, activation = "relu") \\%>\\%
#'    layer_dense(units = num_factor_levels_y, activation = 'softmax')
#' ```
#' 
#' @references Francois C and Allaire JJ (2018) _Deep Learning with R_, Manning
#' 
#' @examples
#' data(okc)
#' 
#' rec <- recipe(Class ~ age + location, data = okc) %>%
#'   step_tfembed(location, outcome = vars(Class),
#'                options = tfembed_control(epochs = 10))
#' 
#' # see pkgdown url for more examples


#' @importFrom recipes add_step step terms_select sel2char ellipse_check
step_tfembed <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           outcome = NULL,
           number = 2,
           hidden = 0,
           options = tfembed_control(),
           mapping = NULL,
           skip = FALSE) {
    if (is.null(outcome))
      stop("Please list a variable in `outcome`", call. = FALSE)
    add_step(
      recipe,
      step_tfembed_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        outcome = outcome,
        number = number,
        hidden = hidden,
        options = options,
        mapping = mapping,
        skip = skip
      )
    )
  }

step_tfembed_new <-
  function(terms = NULL,
           role = NA,
           trained = FALSE,
           outcome = NULL,
           number = NULL,
           hidden = NULL,
           options = NULL,
           mapping = NULL,
           skip = FALSE) {
    step(
      subclass = "tfembed",
      terms = terms,
      role = role,
      number = number,
      hidden = hidden,
      options = options,
      trained = trained,
      outcome = outcome,
      mapping = mapping,
      skip = skip
    )
  }

#' @importFrom recipes check_type
#' @export
prep.step_tfembed <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)
  check_type(training[, col_names], quant = FALSE)
  y_name <- terms_select(x$outcome, info = info)
  # check options
  res <-
    map(
      training[, col_names], 
      tf_coefs, 
      y = training[, y_name], 
      opt = x$options,
      num = x$number,
      h = x$hidden
    )
  x$mapping <- res
  x$trained <- TRUE
  x
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


#' @importFrom keras keras_model_sequential layer_embedding layer_flatten
#' @importFrom keras layer_dense compile fit get_layer backend
#' @importFrom dplyr bind_cols as_tibble ends_with
#' @importFrom stats setNames
tf_coefs <- function(x, y, opt, num, lab, h, ...) {
  on.exit(keras::backend()$clear_session())
  
  if (is.character(x))
    x <- as.factor(x)
  
  lvl <- levels(x)
  x <- matrix(as.numeric(x), ncol = 1)
  y <- y[[1]]
  if(is.character(y))
    y <- as.factor(y)
  factor_y <- is.factor(y)
  
  if (factor_y)
    y <- class2ind(y)
  else
    y <- matrix(y, ncol = 1)
  
  model <- keras_model_sequential() %>% 
    layer_embedding(
    input_dim = max(x[,1]) + 1,
    output_dim = num,
    input_length = 1,
    name = "embedding"
  ) %>%
    layer_flatten()  
  
  if(h > 0) 
    model <- model %>%
      layer_dense(units = h, activation = "relu")
  
  if (factor_y)
    model <- model %>%
    layer_dense(units = ncol(y), activation = 'softmax')
  else
    model <- model %>%
    layer_dense(units = 1, activation = 'linear')
  
  print(model)
  
  model <- model %>%
    compile(
      loss = opt$loss,
      optimizer = opt$optimizer
      )
  
  model %>% fit(
    x = x,
    y = y,
    epochs = opt$epochs,
    validation_split = opt$validation_split,
    batch_size = opt$batch_size,
    verbose = opt$verbose
  )
  
  layer_values <- get_layer(model, "embedding")$get_weights()[[1]]
  layer_values <- as_tibble(layer_values) %>% 
    setNames(recipes::names0(num, prefix = "..emb")) %>%
    mutate(..level = c("..new", lvl))
  layer_values
}

#' @importFrom dplyr tibble mutate filter left_join %>% arrange contains
#' @importFrom stats complete.cases
#' @importFrom recipes names0
map_tf_coef <- function(dat, mapping, prefix) {
  new_val <- mapping[mapping$..level == "..new", grepl("\\.\\.emb", names(mapping))]
  dat <- dat %>% 
    mutate(..order = 1:nrow(dat)) %>%
    set_names(c("..level", "..order")) %>%
    mutate(..level = as.character(..level))
  mapping <- mapping %>% dplyr::filter(..level != "..new")
  emb_names <- grepl("\\.\\.emb", names(mapping))
  names(mapping)[emb_names] <- 
    names0(sum(emb_names), prefix = paste0(prefix, "_embed"))
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
bake.step_tfembed <- function(object, newdata, ...) {
  for (col in names(object$mapping)) {
    tmp <- map_tf_coef(newdata[, col], object$mapping[[col]], prefix = col)
    newdata <- bind_cols(newdata, tmp)
    rm(tmp)
  }
  newdata <- newdata[, !(names(newdata) %in% names(object$mapping))]
  
  newdata
}

#' @importFrom recipes printer
#' @export
print.step_tfembed <-
  function(x, width = max(20, options()$width - 31), ...) {
    cat("Embedding of factors via tensorflow for ", sep = "")
    printer(names(x$mapping), x$terms, x$trained, width = width)
    invisible(x)
  }

#' @importFrom dplyr bind_rows 
#' @importFrom tidyr gather
#' @importFrom recipes is_trained
#' @importFrom broom tidy
#' @rdname step_tfembed
#' @param x A `step_tfembed` object.
#' @export
tidy.step_tfembed <- function(x, ...) {
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
  res
}


#' @export
#' @rdname step_tfembed
#' @param optimizer,loss Arguments to pass to [keras::compile()]
#' @param epochs,validation_split,batch_size,verbose Arguments to pass to [keras::fit()]
tfembed_control <- function(
  loss = "mse",
  optimizer = "sgd",
  epochs = 20,
  validation_split = 0,
  batch_size = 32,
  verbose = 0
) {
  if(batch_size < 1)
    stop("`batch_size` should be a positive integer", call. = FALSE)
  if(epochs < 1)
    stop("`epochs` should be a positive integer", call. = FALSE)  
  if(validation_split < 0 | validation_split > 1)
    stop("`validation_split` should be on [0, 1)", call. = FALSE)
  list(
    loss = loss, optimizer = optimizer, epochs = epochs, 
    validation_split = validation_split, batch_size = batch_size,
    verbose = verbose)
}

#' @importFrom utils globalVariables
utils::globalVariables(
  c("..level", "..order")
)
