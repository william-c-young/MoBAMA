#' MoBAMA (Modeling of Binding Antibody Multiplex Assays)
#'
#' This package fits a Bayesian grouped mixture model for BAMA and
#' Fc array data using stan. The model identifies responses in the
#' data.
#'
#' @docType package
#' @name MoBAMA-package
#' @aliases MoBAMA
#' @useDynLib MoBAMA, .registration = TRUE
#'
#' @import methods
#' @import stats
#' @import Rcpp
#' @import rstantools
#' @import stringr
#' @import tidyr
#' @import ggplot2
#'
#' @seealso
#'   \itemize{
#'     \item \code{\link{MoBAMA}}, for the main model fitting routine.
#'   }
#'
#' 
NULL
