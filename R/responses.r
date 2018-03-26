#' Gets the inferred response probabilities for observations from
#' a MoBAMAResult object
#'
#' @param result the MoBAMAResult object to query
#'
#' @return a \code{data.frame} containing the response probability information
#'
#' @export
responses <- function(result) {
    respProbs <- result %>% extractParams2("z[", ",2]") %>%
        rename(responseProb = mean) %>%
        mutate(isResp = responseProb > 0.5) %>%
        select(isResp, responseProb, sd, q025, q975, n_eff, Rhat)
    bind_cols(result$data, respProbs)
}
