#' Extract parameters from a MoBAMAResult object
#'
#' This function extracts parameters from a MoBAMAResult object
#' that match the string provided.
#'
#' @param object The MoBAMAResult object to query.
#' @param varStr The string which must match either the start or
#'   all of the parameter name.
#' @param fullname A boolean indicating whether the whole variable
#'   name must be matched, or just the start. Defaults to \code{FALSE}.
#'
#' @return A \code{data.frame} containing the extracted parameter information.
#'
#' @noRd
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

#' Extract parameters from a MoBAMAResult object
#'
#' This function extracts parameters from the a MoBAMAResult object
#' that match the start and ending strings provided.
#'
#' @param object The MoBAMAResult object to query.
#' @param startStr The string which must match the start of the parameter name.
#' @param endStr The string which must match the end of the parameter name.
#'
#' @return A \code{data.frame} containing the extracted parameter information.
#'
#' @noRd
extractParams2 <- function(object,
                           startStr,
                           endStr) {
    object$parameters %>%
        dplyr::filter(str_sub(var, 1, nchar(startStr)) == startStr,
                      str_sub(var, -nchar(endStr), -1) == endStr)
}
