# Entity Embeddings of Categorical Variables using TensorFlow

The approach encodes categorical data as multiple numeric variables
using a *word embedding* approach. Originally intended as a way to take
a large number of word identifiers and represent them in a smaller
dimension. Good references on this are [Guo and Berkhahn
(2016)](https://arxiv.org/abs/1604.06737) and Chapter 6 of [Francois and
Allaire (2018)](https://www.manning.com/books/deep-learning-with-r).

The methodology first translates the *C* factor levels as a set of
integer values then randomly allocates them to the new *D* numeric
columns. These columns are optionally connected in a neural network to
an intermediate layer of hidden units. Optionally, other predictors can
be added to the network in the usual way (via the `predictors` argument)
that also link to the hidden layer. This implementation uses a single
layer with ReLu activations. Finally, an output layer is used with
either linear activation (for numeric outcomes) or softmax (for
classification).

To translate this model to a set of embeddings, the coefficients of the
original embedding layer are used to represent the original factor
levels.

As an example, we use the Ames housing data where the sale price of
houses are being predicted. One predictor, neighborhood, has the most
factor levels of the predictors.

``` r

library(tidymodels)
data(ames)
length(levels(ames$Neighborhood))
```

    ## [1] 29

The distribution of data in the neighborhood is not uniform:

``` r

ames |>
  count(Neighborhood) |>
  ggplot(aes(n, reorder(Neighborhood, n))) +
  geom_col() +
  labs(y = NULL) +
  theme_bw()
```

![Horizontal bar chart. n along the x axis, neighborhoods along the
y-axis. The lengths of the bars vary from near zero for Landmarks and
Green_Hills, to almost 450 for
North_Ames.](Tensorflow_files/figure-html/ames-xtab-1.png)

Fo plotting later, we calculate the simple means per neighborhood:

``` r

means <-
  ames |>
  group_by(Neighborhood) |>
  summarise(
    mean = mean(log10(Sale_Price)),
    n = length(Sale_Price),
    lon = median(Longitude),
    lat = median(Latitude)
  )
```

We’ll fit a model with 10 hidden units and 3 encoding columns:

``` r

library(embed)
tf_embed <-
  recipe(Sale_Price ~ ., data = ames) |>
  step_log(Sale_Price, base = 10) |>
  # Add some other predictors that can be used by the network
  # We preprocess them first
  step_YeoJohnson(Lot_Area, Full_Bath, Gr_Liv_Area) |>
  step_range(Lot_Area, Full_Bath, Gr_Liv_Area) |>
  step_embed(
    Neighborhood,
    outcome = vars(Sale_Price),
    predictors = vars(Lot_Area, Full_Bath, Gr_Liv_Area),
    num_terms = 5,
    hidden_units = 10,
    options = embed_control(epochs = 75, validation_split = 0.2)
  ) |>
  prep(training = ames)

theme_set(theme_bw() + theme(legend.position = "top"))

tf_embed$steps[[4]]$history |>
  filter(epochs > 1) |>
  ggplot(aes(x = epochs, y = loss, col = type)) +
  geom_line() +
  scale_y_log10()
```

![Line chart with 2 lines. epochs along the x-axis, loss along the
y-axis. The two lines are colored according to the type of loss, red for
normal loss and blue for validation loss. The lines have high values for
small epochs and lower values for higher epochs, with the validation
loss being lower at all
times.](Tensorflow_files/figure-html/ames-linear-1.png)

The embeddings are obtained using the `tidy` method:

``` r

hood_coef <-
  tidy(tf_embed, number = 4) |>
  dplyr::select(-terms, -id) |>
  dplyr::rename(Neighborhood = level) |>
  # Make names smaller
  rename_at(
    vars(contains("emb")),
    funs(gsub("Neighborhood_", "", ., fixed = TRUE))
  )
hood_coef
```

    ## # A tibble: 30 × 6
    ##     embed_1 embed_2   embed_3 embed_4  embed_5 Neighborhood      
    ##       <dbl>   <dbl>     <dbl>   <dbl>    <dbl> <chr>             
    ##  1  0.00622 -0.0422  0.00890   0.0153 -0.0140  ..new             
    ##  2  0.00537 -0.0668  0.0629   -0.0120  0.0131  North_Ames        
    ##  3 -0.0480  -0.0440  0.0364   -0.0552 -0.0487  College_Creek     
    ##  4  0.0392   0.0406  0.0328   -0.0203  0.0587  Old_Town          
    ##  5 -0.00674  0.0248  0.0697    0.0255  0.0382  Edwards           
    ##  6 -0.0208  -0.0539 -0.0247   -0.0590 -0.107   Somerset          
    ##  7 -0.0688  -0.111   0.0132   -0.0451 -0.147   Northridge_Heights
    ##  8 -0.0473  -0.0311  0.0135   -0.0350 -0.00211 Gilbert           
    ##  9 -0.0187  -0.0650 -0.0209    0.0144  0.0283  Sawyer            
    ## 10  0.0121  -0.0247  0.000337  0.0149 -0.0243  Northwest_Ames    
    ## # ℹ 20 more rows

``` r

hood_coef <-
  hood_coef |>
  inner_join(means, by = "Neighborhood")
hood_coef
```

    ## # A tibble: 28 × 10
    ##     embed_1  embed_2  embed_3 embed_4  embed_5 Neighborhood  mean     n
    ##       <dbl>    <dbl>    <dbl>   <dbl>    <dbl> <chr>        <dbl> <int>
    ##  1  0.00537 -0.0668   6.29e-2 -0.0120  0.0131  North_Ames    5.15   443
    ##  2 -0.0480  -0.0440   3.64e-2 -0.0552 -0.0487  College_Cre…  5.29   267
    ##  3  0.0392   0.0406   3.28e-2 -0.0203  0.0587  Old_Town      5.07   239
    ##  4 -0.00674  0.0248   6.97e-2  0.0255  0.0382  Edwards       5.09   194
    ##  5 -0.0208  -0.0539  -2.47e-2 -0.0590 -0.107   Somerset      5.35   182
    ##  6 -0.0688  -0.111    1.32e-2 -0.0451 -0.147   Northridge_…  5.49   166
    ##  7 -0.0473  -0.0311   1.35e-2 -0.0350 -0.00211 Gilbert       5.27   165
    ##  8 -0.0187  -0.0650  -2.09e-2  0.0144  0.0283  Sawyer        5.13   151
    ##  9  0.0121  -0.0247   3.37e-4  0.0149 -0.0243  Northwest_A…  5.27   131
    ## 10  0.0113  -0.00694 -2.15e-2 -0.0218 -0.0284  Sawyer_West   5.25   125
    ## # ℹ 18 more rows
    ## # ℹ 2 more variables: lon <dbl>, lat <dbl>

We can make a simple, interactive plot of the new features versus the
outcome:

``` r

tf_plot <-
  hood_coef |>
  dplyr::select(-lon, -lat) |>
  gather(variable, value, starts_with("embed")) |>
  # Clean up the embedding names
  # Add a new variable as a hover-over/tool tip
  mutate(
    label = paste0(gsub("_", " ", Neighborhood), " (n=", n, ")"),
    variable = gsub("_", " ", variable)
  ) |>
  ggplot(aes(x = value, y = mean)) +
  geom_point_interactive(aes(size = sqrt(n), tooltip = label), alpha = .5) +
  facet_wrap(~variable, scales = "free_x") +
  theme_bw() +
  theme(legend.position = "top") +
  labs(y = "Mean (log scale)", x = "Embedding")

girafe(ggobj = tf_plot)
```

However, this has induced some between-predictor correlations:

``` r

hood_coef |>
  dplyr::select(contains("emb")) |>
  cor() |>
  round(2)
```

    ##         embed_1 embed_2 embed_3 embed_4 embed_5
    ## embed_1    1.00    0.38    0.08    0.25    0.61
    ## embed_2    0.38    1.00    0.30    0.38    0.39
    ## embed_3    0.08    0.30    1.00    0.10    0.36
    ## embed_4    0.25    0.38    0.10    1.00    0.43
    ## embed_5    0.61    0.39    0.36    0.43    1.00
