`embed` is a package that contains extra methods for the [`recipes`](http://cran.rstudio.com/package=recipes) package for embedding categorical predictors into one or more numeric columns. All of the preprocessing methods are _supervised_. 

These steps are contained in a separate package because the package dependencies ([`rstanarm`](http://cran.rstudio.com/package= rstanarm) and [`keras`](http://cran.rstudio.com/package= keras)) are fairly heavy. 

The steps included are:

* `step_embed` estimates the effect of each of the factor levels on the outcome and these estimates are used as the new encoding. The estimates are estimated by a generalized linear model. This step can be executed without pooling (via `glm`) or with partial pooling (`stan_glm`). Currently implemented for numeric and two-class outcomes. 

* `step_tfembed` uses `keras::layer_embedding` to translate the original _C_ factor levels into a set of _D_ new variables (< _C_). The model fitting routine optimizes which factor levels are mapped to each of the new variables as well as the corresponding regression coefficients (i.e., neural network weights) that will be used as the new encodings.  

 To install it, use:

``` r
require("devtools")
install_github("topepo/embed")
```
