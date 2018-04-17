#' Calculates the cutoff probability for a requested false-discovery rate
#'
#' Calculates the minimum cutoff probability for defining a response from
#' a set of response probabilities such that the expected false-discovery
#' rate is no greater than the requested FDR
#'
#' @param responseProbs the response probabilities to use for calculating
#' the minimum cutoff probability
#' @param fdr the requested false-discovery rate
#' 
#' @return a probability cutoff for defining a response
#'
#' @export
fdr_cutoff <- function(responseProbs, fdr = 0.05) {
    probs <- sort(responseProbs, decreasing = TRUE)
    efdr <- cumsum(1-probs) / (1:length(probs))
    min(probs[efdr <= fdr])
}
