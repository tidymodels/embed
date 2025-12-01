# Using Generalized Linear Models

This method uses a generalized linear model to estimate the effect of
each level of a factor predictor on the outcome. These values are
retained to serve as the new encodings for the factor levels. This is
sometimes referred to as *likelihood encodings*. `embed` has two
estimation methods for accomplishing this: with and without pooling.

The example used here is the Grant data from Kuhn and Johnson (2013),
these data are used to predict whether a grant application was accepted.
One predictor, the sponsor, is represented by the factor variable
`sponsor_code`. The frequencies of sponsors in the data set used here
vary between 0 person and 1874 per code. There are 298 sponsor codes in
the data. Rather than producing 297 indicator variables for a model, a
single numeric variable can be used to represent the *effect* or
*impact* of the factor level on the outcome. In this case, where a
factor outcome is being predicted (accepted or not), the effects are
quantified by the log-odds of the sponsor code for being accepted.

We first calculate the raw log-odds for the data (independent of any
model):

``` r
library(tidymodels)
library(embed)

tidymodels_prefer()
theme_set(theme_bw() + theme(legend.position = "top"))

data(grants)

props <-
  grants_other |>
  group_by(sponsor_code) |>
  summarise(
    prop = mean(class == "successful"),
    log_odds = log(prop / (1 - prop)),
    n = length(class)
  ) |>
  mutate(label = paste0(gsub("_", " ", sponsor_code), " (n=", n, ")"))

props |>
  select(-label)
```

    ## # A tibble: 291 × 4
    ##    sponsor_code  prop log_odds     n
    ##    <fct>        <dbl>    <dbl> <int>
    ##  1 100D         0.6      0.405    10
    ##  2 101A         0.125   -1.95     16
    ##  3 103C         0.111   -2.08      9
    ##  4 105A         0.167   -1.61      6
    ##  5 107C         1      Inf         2
    ##  6 10B          1      Inf         1
    ##  7 111C         0.167   -1.61      6
    ##  8 112D         0.667    0.693    12
    ##  9 113A         0.5      0        10
    ## 10 118B         0.5      0         2
    ## # ℹ 281 more rows

``` r
# later, for plotting
rng <- extendrange(props$log_odds[is.finite(props$log_odds)], f = 0.1)
```

In subsequent sections, a logistic regression model is used. When the
outcome variable is numeric, the steps automatically use linear
regression models to estimate effects.

## No Pooling

In this case, the effect of each sponsor code can be estimated
separately for each factor level. One method for conducting this
estimation step is to fit a logistic regression with the acceptance
classification as the outcome and the sponsor code as the predictor.
From this, the log-odds are naturally estimated by logistic regression.

For these data, a recipe is created and `step_lencode_glm` is used:

``` r
grants_glm <-
  recipe(class ~ ., data = grants_other) |>
  # specify the variable being encoded and the outcome
  step_lencode_glm(sponsor_code, outcome = vars(class)) |>
  # estimate the effects
  prep(training = grants_other)
```

The `tidy` method can be used to extract the encodings and are merged
with the raw estimates:

``` r
glm_estimates <-
  tidy(grants_glm, number = 1) |>
  dplyr::select(-terms, -id)
glm_estimates
```

    ## # A tibble: 292 × 2
    ##    level  value
    ##    <chr>  <dbl>
    ##  1 100D   0.405
    ##  2 101A  -1.95 
    ##  3 103C  -2.08 
    ##  4 105A  -1.61 
    ##  5 107C  16.6  
    ##  6 10B   16.6  
    ##  7 111C  -1.61 
    ##  8 112D   0.693
    ##  9 113A   0    
    ## 10 118B   0    
    ## # ℹ 282 more rows

``` r
glm_estimates <-
  glm_estimates |>
  set_names(c("sponsor_code", "glm")) |>
  inner_join(props, by = "sponsor_code")
```

For the sponsor codes with `n > 1`, the estimates are effectively the
same:

``` r
glm_estimates |>
  dplyr::filter(is.finite(log_odds)) |>
  mutate(difference = log_odds - glm) |>
  dplyr::select(difference) |>
  summary()
```

    ##    difference        
    ##  Min.   :-1.332e-15  
    ##  1st Qu.:-2.220e-16  
    ##  Median : 0.000e+00  
    ##  Mean   :-5.139e-17  
    ##  3rd Qu.: 1.604e-16  
    ##  Max.   : 8.882e-16

Note that there is also a effect that is used for a novel sponsor code
for future data sets that is the average effect:

``` r
tidy(grants_glm, number = 1) |>
  dplyr::filter(level == "..new") |>
  select(-id)
```

    ## # A tibble: 1 × 3
    ##   level value terms       
    ##   <chr> <dbl> <chr>       
    ## 1 ..new -2.88 sponsor_code

## Partial Pooling

This method estimates the effects by using all of the sponsor codes at
once using a hierarchical Bayesian generalized linear model. The sponsor
codes are treated as a random set that contributes a random intercept to
the previously used logistic regression.

Partial pooling estimates each effect as a combination of the separate
empirical estimates of the log-odds and the prior distribution. For
sponsor codes with small sample sizes, the final estimate is *shrunken*
towards the overall mean of the log-odds. This makes sense since we have
poor information for estimating these sponsor codes. For sponsor codes
with many data points, the estimates reply more on the empirical
estimates. [This
page](https://cran.r-project.org/web/packages/rstanarm/vignettes/glmer.html)
has a good discussion of pooling using Bayesian models.

``` r
# due to Matrix problems
knitr::knit_exit()
```
