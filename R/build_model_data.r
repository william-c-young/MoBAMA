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
