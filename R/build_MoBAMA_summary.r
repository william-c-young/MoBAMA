build_MoBAMA_summary <- function(modelFit) {
    parSumm <- as.data.frame(rstan::summary(modelFit)$summary)
    parSumm$var <- rownames(parSumm)
    names(parSumm) <- c("mean", "se_mean", "sd",
                         "q025", "q25", "q50", "q75", "q975",
                       "n_eff", "Rhat", "var")

    parSumm %>% select(var, mean, se_mean, sd, q025, q25, q50, q75, q975, n_eff, Rhat)
}
