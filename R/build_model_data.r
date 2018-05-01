#' Convert validated and prepared MoBAMA data into
#' a form fit for passing to stan
#'
#' This function converts MoBAMA data into a list containing
#' all the fields for the model as used by stan.
#'
#' @param data The MoBAMA data to be converted.
#' @param dataType A string denoting the type of data
#'   to be modeled ('bama' or 'fc'). Defaults to 'fc'.
#'
#' @return A list containing all the fields necessary to pass
#'   to the stan sampler for the grouped mixture model.
#'
#' @noRd
build_model_data <- function(data, dataType) {
    list(N = nrow(data),
         T = max(data$tp),
         obs_to_t = data$tp,
         N_grp = max(data$groupId),
         obs_to_grp = data$groupId,
         N_ag = max(data$agId),
         obs_to_ag = data$agId,
         N_re = max(data$reId),
         obs_to_re = data$reId,
         y = data$val)
}
