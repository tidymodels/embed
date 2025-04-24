#' Encoding Factors into Multiple Columns
#'
#' `step_embed()` creates a *specification* of a recipe step that will convert a
#' nominal (i.e. factor) predictor into a set of scores derived from a
#' tensorflow model via a word-embedding model. `embed_control` is a simple
#' wrapper for setting default options.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose variables. For
#'   `step_embed`, this indicates the variables to be encoded into a numeric
#'   format. See [recipes::selections()] for more details. For the `tidy`
#'   method, these are not currently used.
#' @param role For model terms created by this step, what analysis role should
#'   they be assigned?. By default, the function assumes that the embedding
#'   variables created will be used as predictors in a model.
#' @param outcome A call to `vars` to specify which variable is used as the
#'   outcome in the neural network.
#' @param predictors An optional call to `vars` to specify any variables to be
#'   added as additional predictors in the neural network. These variables
#'   should be numeric and perhaps centered and scaled.
#' @param num_terms An integer for the number of resulting variables.
#' @param hidden_units An integer for the number of hidden units in a dense ReLu
#'   layer between the embedding and output later. Use a value of zero for no
#'   intermediate layer (see Details below).
#' @param options A list of options for the model fitting process.
#' @param mapping A list of tibble results that define the encoding. This is
#'   `NULL` until the step is trained by [recipes::prep()].
#' @param history A tibble with the convergence statistics for each term. This
#'   is `NULL` until the step is trained by [recipes::prep()].
#' @param keep_original_cols A logical to keep the original variables in the
#'   output. Defaults to `FALSE`.
#' @param skip A logical. Should the step be skipped when the recipe is baked by
#'   [recipes::bake()]? While all operations are baked when [recipes::prep()] is
#'   run, some operations may not be able to be conducted on new data (e.g.
#'   processing the outcome variable(s)). Care should be taken when using `skip
#'   = TRUE` as it may affect the computations for subsequent operations.
#' @param trained A logical to indicate if the quantities for preprocessing have
#'   been estimated.
#' @param id A character string that is unique to this step to identify it.
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of existing steps (if any). For the `tidy` method, a tibble with
#'   columns `terms` (the selectors or variables for encoding), `level` (the
#'   factor levels), and several columns containing `embed` in the name.
#' @keywords datagen
#' @concept preprocessing encoding
#' @details
#'
#' Factor levels are initially assigned at random to the new variables and these
#' variables are used in a neural network to optimize both the allocation of
#' levels to new columns as well as estimating a model to predict the outcome.
#' See Section 6.1.2 of Francois and Allaire (2018) for more details.
#'
#' The new variables are mapped to the specific levels seen at the time of model
#' training and an extra instance of the variables are used for new levels of
#' the factor.
#'
#' One model is created for each call to `step_embed`. All terms given to the
#' step are estimated and encoded in the same model which would also contain
#' predictors give in `predictors` (if any).
#'
#' When the outcome is numeric, a linear activation function is used in the last
#' layer while softmax is used for factor outcomes (with any number of levels).
#'
#' For example, the `keras3` code for a numeric outcome, one categorical
#' predictor, and no hidden units used here would be
#'
#' ```
#'   keras_model_sequential() %>%
#'   layer_embedding(
#'     input_dim = num_factor_levels_x + 1,
#'     output_dim = num_terms
#'   ) %>%
#'   layer_flatten() %>%
#'   layer_dense(units = 1, activation = 'linear')
#' ```
#'
#' If a factor outcome is used and hidden units were requested, the code would
#' be
#'
#' ```
#'   keras_model_sequential() %>%
#'   layer_embedding(
#'     input_dim = num_factor_levels_x + 1,
#'     output_dim = num_terms
#'   ) %>%
#'   layer_flatten() %>%
#'   layer_dense(units = hidden_units, activation = "relu") %>%
#'   layer_dense(units = num_factor_levels_y, activation = 'softmax')
#' ```
#'
#' Other variables specified by `predictors` are added as an additional dense
#' layer after `layer_flatten` and before the hidden layer.
#'
#' Also note that it may be difficult to obtain reproducible results using this
#' step due to the nature of Tensorflow (see link in References).
#'
#' tensorflow models cannot be run in parallel within the same session (via
#' `foreach` or `futures`) or the `parallel` package. If using a recipes with
#' this step with `caret`, avoid parallel processing.
#'
#' # Tidying
#'
#' When you [`tidy()`][recipes::tidy.recipe] this step, a tibble is returned with
#' a number of columns with embedding information, and columns `terms`,
#' `levels`, and `id`:
#'
#' \describe{
#'   \item{terms}{character, the selectors or variables selected}
#'   \item{levels}{character, levels in variable}
#'   \item{id}{character, id of this step}
#' }
#'
#' ```{r, echo = FALSE, results="asis"}
#' step <- "step_embed"
#' result <- knitr::knit_child("man/rmd/tunable-args.Rmd")
#' cat(result)
#' ```
#'
#' @template case-weights-not-supported
#'
#' @references
#'
#' Francois C and Allaire JJ (2018) _Deep Learning with R_, Manning
#'
#' "Concatenate Embeddings for Categorical Variables with keras3"
#' \url{https://flovv.github.io/Embeddings_with_keras3_part2/}
#'
#' @examplesIf !embed:::is_cran_check() && rlang::is_installed(c("modeldata", "keras3"))
#' data(grants, package = "modeldata")
#'
#' set.seed(1)
#' grants_other <- sample_n(grants_other, 500)
#'
#' rec <- recipe(class ~ num_ci + sponsor_code, data = grants_other) %>%
#'   step_embed(sponsor_code,
#'     outcome = vars(class),
#'     options = embed_control(epochs = 10)
#'   )
#' @export
step_embed <-
  function(
    recipe,
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
    keep_original_cols = FALSE,
    skip = FALSE,
    id = rand_id("embed")
  ) {
    # warm start for tf to avoid a bug in tensorflow
    is_tf_available()

    if (is.null(outcome)) {
      cli::cli_abort("Please list a variable in {.arg outcome}.")
    }
    add_step(
      recipe,
      step_embed_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        outcome = outcome,
        predictors = predictors,
        num_terms = num_terms,
        hidden_units = hidden_units,
        options = options,
        mapping = mapping,
        history = history,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_embed_new <-
  function(
    terms,
    role,
    trained,
    outcome,
    predictors,
    num_terms,
    hidden_units,
    options,
    mapping,
    history,
    keep_original_cols,
    skip,
    id
  ) {
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
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_embed <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_number_whole(x$num_terms, min = 0, arg = "num_terms")
  check_number_whole(x$hidden_units, min = 0, arg = "hidden_units")

  if (length(col_names) > 0) {
    check_type(training[, col_names], types = c("string", "factor", "ordered"))
    y_name <- recipes_eval_select(x$outcome, training, info)
    if (length(x$predictors) > 0) {
      pred_names <- recipes_eval_select(x$predictors, training, info)
      check_type(training[, pred_names], types = c("double", "integer"))
    } else {
      pred_names <- NULL
    }

    x$options <- tf_options_check(x$options)
    res <-
      tf_coefs2(
        x = training[, col_names],
        y = training[, y_name],
        z = if (is.null(pred_names)) NULL else training[, pred_names],
        opt = x$options,
        num = x$num_terms,
        h = x$hidden_units
      )

    # compute epochs actually trained for
    epochs <- min(res$history$params$epochs, length(res$history$metrics[[1]]))
    .hist <- # TODO convert to pivot and get signature for below
      as_tibble(res$history$metrics) %>%
      mutate(epochs = 1:epochs) %>%
      tidyr::pivot_longer(c(-epochs), names_to = "type", values_to = "loss")
  } else {
    res <- NULL
    .hist <- tibble::tibble(
      epochs = integer(0),
      type = character(0),
      loss = numeric(0)
    )
  }

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
    history = .hist,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

is_tf_2 <- function() {
  if (!is_tf_available()) {
    cli::cli_abort(
      c(
        "tensorflow could now be found.",
        "i" = "Please run {.code tensorflow::install_tensorflow()} to install."
      )
    )
  }
  compareVersion("2.0", as.character(tensorflow::tf_version())) <= 0
}

tf_coefs2 <- function(x, y, z, opt, num, lab, h, seeds = sample.int(10000, 4),
                      ...) {
  rlang::check_installed("keras3")

  vars <- names(x)
  p <- length(vars)

  set.seed(seeds[1])

  if (is_tf_2()) {
    tensorflow::tf$random$set_seed(seeds[2])
  } else {
    tensorflow::use_session_with_seed(seeds[2])
  }

  on.exit(keras3::clear_session())

  lvl <- lapply(x, levels)

  # convert levels to integers; zero signifies a new level
  mats <- lapply(x, function(x) matrix(as.numeric(x), ncol = 1))

  y <- y[[1]]
  if (is.character(y)) {
    y <- as.factor(y)
  }
  factor_y <- is.factor(y)

  if (factor_y) {
    y <- class2ind(y)
  } else {
    y <- matrix(y, ncol = 1)
  }

  inputs <- vector(mode = "list", length = p)
  # For each categorical predictor, make an input layer
  for (i in 1:p) {
<<<<<<< HEAD
    inputs[[i]] <- keras::layer_input(
      shape = 1,
      name = paste0("input_", vars[i])
    )
=======
    inputs[[i]] <- keras3::layer_input(shape = 1, name = paste0("input_", vars[i]))
>>>>>>> fe613e2abd5279953d263b9e41372d2d69bc2d9b
  }

  layers <- vector(mode = "list", length = p)
  # Now add embedding to each layer and then flatten
  for (i in 1:p) {
    layers[[i]] <-
      inputs[[i]] %>%
      keras3::layer_embedding(
        input_dim = length(lvl[[i]]) + 1,
        output_dim = num,
        name = paste0("layer_", vars[i])
      ) %>%
      keras3::layer_flatten()
  }

  if (is.null(z)) {
    if (p > 1) {
      all_layers <- keras3::layer_concatenate(layers)
    } else {
      all_layers <- layers[[1]]
    }
  } else {
    mats$z <- as.matrix(z)
    pred_layer <- keras3::layer_input(shape = ncol(z), name = "other_pred")
    all_layers <- keras3::layer_concatenate(c(layers, pred_layer))
    inputs <- c(inputs, pred_layer)
  }

  if (h > 0) {
    all_layers <-
      all_layers %>%
<<<<<<< HEAD
      keras::layer_dense(
        units = h,
        activation = "relu",
        name = "hidden_layer",
        kernel_initializer = keras::initializer_glorot_uniform(seed = seeds[3])
=======
      keras3::layer_dense(
        units = h, activation = "relu", name = "hidden_layer",
        kernel_initializer = keras3::initializer_glorot_uniform(seed = seeds[3])
>>>>>>> fe613e2abd5279953d263b9e41372d2d69bc2d9b
      )
  }

  if (factor_y) {
    all_layers <-
      all_layers %>%
<<<<<<< HEAD
      keras::layer_dense(
        units = ncol(y),
        activation = "softmax",
        name = "output_layer",
        kernel_initializer = keras::initializer_glorot_uniform(seed = seeds[4])
=======
      keras3::layer_dense(
        units = ncol(y), activation = "softmax", name = "output_layer",
        kernel_initializer = keras3::initializer_glorot_uniform(seed = seeds[4])
>>>>>>> fe613e2abd5279953d263b9e41372d2d69bc2d9b
      )
  } else {
    all_layers <-
      all_layers %>%
<<<<<<< HEAD
      keras::layer_dense(
        units = 1,
        activation = "linear",
        name = "output_layer",
        kernel_initializer = keras::initializer_glorot_uniform(seed = seeds[4])
=======
      keras3::layer_dense(
        units = 1, activation = "linear", name = "output_layer",
        kernel_initializer = keras3::initializer_glorot_uniform(seed = seeds[4])
>>>>>>> fe613e2abd5279953d263b9e41372d2d69bc2d9b
      )
  }

  model <-
    keras3::keras_model(inputs = inputs, outputs = all_layers)

  model %>%
    keras3::compile(
      loss = opt$loss,
      metrics = opt$metrics,
      optimizer = opt$optimizer
    )

  history <-
    model %>%
    keras3::fit(
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
      keras3::get_layer(model, paste0("layer_", vars[i]))$get_weights() %>%
      as.data.frame() %>%
      setNames(names0(num, paste0(vars[i], "_embed_"))) %>%
      as_tibble() %>%
      mutate(..level = c("..new", lvl[[i]]))
  }
  names(layer_values) <- vars

  list(layer_values = layer_values, history = history)
}

map_tf_coef2 <- function(dat, mapping, prefix) {
  new_val <- mapping %>%
    dplyr::filter(..level == "..new") %>%
    dplyr::select(-..level)
  dat <- dat %>%
    mutate(..order = seq_len(nrow(dat))) %>%
    set_names(c("..level", "..order")) %>%
    mutate(..level = as.character(..level))
  mapping <- mapping %>% dplyr::filter(..level != "..new")
  dat <- left_join(dat, mapping, by = "..level") %>%
    arrange(..order)

  dat <- dat %>% dplyr::select(contains("_embed"))
  dat[!complete.cases(dat), ] <- new_val
  dat
}

#' @export
bake.step_embed <- function(object, new_data, ...) {
  col_names <- names(object$mapping)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    tmp <- map_tf_coef2(
      dat = new_data[, col_name], # map_tf_coef2() expects a tibble
      mapping = object$mapping[[col_name]],
      prefix = col_name
    )

    tmp <- recipes::check_name(tmp, new_data, object, names(tmp))

    new_data <- vec_cbind(new_data, tmp)
  }

  new_data <- remove_original_cols(new_data, object, col_names)

  new_data
}

#' @rdname step_embed
#' @usage NULL
#' @export
tidy.step_embed <- function(x, ...) {
  if (is_trained(x)) {
    if (length(x$mapping) == 0) {
      res <- tibble(
        terms = character(),
        level = character(),
        value = double()
      )
    } else {
      for (i in seq_along(x$mapping)) {
        x$mapping[[i]]$terms <- names(x$mapping)[i]
      }
      res <- bind_rows(x$mapping)
      names(res) <- gsub("^\\.\\.", "", names(res))
    }
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      level = rep(na_chr, length(term_names)),
      value = rep(na_dbl, length(term_names))
    )
  }
  res$id <- x$id
  res
}

#' @export
print.step_embed <-
  function(x, width = max(20, options()$width - 31), ...) {
    title <- "Embedding of factors via tensorflow for "
    print_step(names(x$mapping), x$terms, x$trained, title, width)
    invisible(x)
  }

#' @export
#' @rdname step_embed
#' @param optimizer,loss,metrics Arguments to pass to keras3::compile()
#' @param epochs,validation_split,batch_size,verbose,callbacks Arguments to pass
<<<<<<< HEAD
#'   to keras::fit()
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
=======
#'   to keras3::fit()
embed_control <- function(loss = "mse",
                          metrics = NULL,
                          optimizer = "sgd",
                          epochs = 20,
                          validation_split = 0,
                          batch_size = 32,
                          verbose = 0,
                          callbacks = NULL) {
>>>>>>> fe613e2abd5279953d263b9e41372d2d69bc2d9b
  if (batch_size < 1) {
    cli::cli_abort("{.arg batch_size} should be a positive integer.")
  }
  if (epochs < 1) {
    cli::cli_abort("{.arg epochs} should be a positive integer.")
  }
  if (validation_split < 0 || validation_split > 1) {
    cli::cli_abort("{.arg validation_split} should be on [0, 1).")
  }
  list(
    loss = loss,
    metrics = metrics,
    optimizer = optimizer,
    epochs = epochs,
    validation_split = validation_split,
    batch_size = batch_size,
    verbose = verbose,
    callbacks = callbacks
  )
}

tf_options_check <- function(opt) {
  exp_names <- c(
    "loss",
    "metrics",
    "optimizer",
    "epochs",
    "validation_split",
    "batch_size",
    "verbose"
  )

  if (length(setdiff(exp_names, names(opt))) > 0) {
    cli::cli_abort(
      "The options {.code {setdiff(exp_names, names(opt))}} are missing from 
      {.arg options}."
    )
  }
  opt
}

class2ind <- function(x) {
  if (!is.factor(x)) {
    cli::cli_abort("{.arg x} should be a factor.")
  }
  y <- model.matrix(~ x - 1)
  colnames(y) <- gsub("^x", "", colnames(y))
  attributes(y)$assign <- NULL
  attributes(y)$contrasts <- NULL
  y
}

is_tf_available <- function() {
  if (!rlang::is_installed("tensorflow")) {
    return(FALSE)
  }

  capture.output(
    res <- try(tensorflow::tf_config(), silent = TRUE),
    file = NULL
  )
  if (inherits(res, "try-error") || all(is.null(res))) {
    return(FALSE)
  } else {
    if (!(any(names(res) == "available"))) {
      return(FALSE)
    }
  }
  res$available
}

#' @rdname required_pkgs.embed
#' @export
required_pkgs.step_embed <- function(x, ...) {
  c("keras3", "embed")
}

#' @export
#' @rdname tunable_embed
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
