#' Compound solubility data
#'
#' @details
#'
#' Tetko et al. (2001) and Huuskonen (2000) investigated a set of compounds with
#' corresponding experimental solubility values using complex sets of
#' descriptors. They used linear regression and neural network models to
#' estimate the relationship between chemical structure and solubility. For our
#' analyses, we will use 1267 compounds and a set of more understandable
#' descriptors that fall into one of three groups: 208 binary "fingerprints"
#' that indicate the presence or absence of a particular chemical sub-structure,
#' 16 count descriptors (such as the number of bonds or the number of Bromine
#' atoms) and 4 continuous descriptors (such as molecular weight or surface
#' area).
#'
#' @name solubility
#' @aliases solubility
#' @docType data
#' @return \item{solubility}{a data frame}
#'
#' @source
#'
#' Tetko, I., Tanchuk, V., Kasheva, T., and Villa, A. (2001). Estimation of
#' aqueous solubility of chemical compounds using E-state indices. _Journal of
#' Chemical Information and Computer Sciences_, 41(6), 1488-1493.
#'
#' Huuskonen, J. (2000). Estimation of aqueous solubility for a diverse set of
#' organic compounds based on molecular topology. _Journal of Chemical
#' Information and Computer Sciences_, 40(3), 773-777.
#' @keywords datasets
#' @examples
#' data(solubility)
#' str(solubility)
NULL
