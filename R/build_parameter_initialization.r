build_parameter_initialization <- function(modelData) {
    list(ag_alpha = 1,
         ag_beta = 1,
         re_alpha = 1,
         re_beta = 1,
         sigma_alpha = 1,
         sigma_beta = 1,
         sigma = as.array(rep(1, modelData$T)),
         mu_ag = matrix(data = 1,
                        ncol = modelData$T,
                        nrow = modelData$N_ag),
         mu_re = matrix(data = 1,
                        ncol = modelData$T,
                        nrow = modelData$N_re),
         mu_0 = as.array(rep(0, modelData$T)))
}
