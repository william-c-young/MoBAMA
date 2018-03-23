#' Extracts parameters from the model results
#'
#' Extracts parameters from the model results that match
#' the string provided
#'
#' @param object the MoBAMAResult object to query
#' @param varStr the string which must match either the start or all of the parameter name
#' @param fullname indicates whether the whole variable name must be matched, or just the start
#'
#' @return a \link{data.frame} containing the extracted parameter information
extractParams <- function(object,
                          varStr,
                          fullname = FALSE) {
              if (fullname) {
                  object$parameters %>%
                      dplyr::filter(var == varStr)
              }
              else {
                  object$parameters %>%
                      dplyr::filter(str_sub(var, 1, nchar(varStr)) == varStr)
              }
}

#' Extracts parameters from the model results
#'
#' Extracts parameters from the model results that match
#' the start and ending strings provided
#'
#' @param object the MoBAMAResult object to query
#' @param startStr the string which must match the start of the parameter name
#' @param endStr the string which must match the end of the parameter name
#'
#' @return a \link{data.frame} containing the extracted parameter information
extractParams2 <- function(object,
                           startStr,
                           endStr) {
    object$parameters %>%
        dplyr::filter(str_sub(var, 1, nchar(startStr)) == startStr,
                      str_sub(var, -nchar(endStr), -1) == endStr)
}
