source("simulate_data.r")

## Make data up
nTp <- 2
nGrp <- 2
nSubj <- 2
nAg <- 2
nRe <- 2

agAlpha <- 1
agBeta <- 1
reAlpha <- 5
reBeta <- 2
sigmaAlpha <- 20
sigmaBeta <- 20

mu0 <- 0.5
groupProbs <- c(0.93, 0.97)
agProbs <- rbeta(nAg, 0.34, 0.16)
reProbs <- rbeta(nRe, 1.04, 0.18)

testdata <- simulate_data(nTp, nGrp, nSubj, nAg, nRe,
                          agAlpha, agBeta, reAlpha, reBeta,
                          sigmaAlpha, sigmaBeta, mu0,
                          groupProbs, agProbs, reProbs)

simulated_data <- testdata$simData %>%
    select(subjectId, group, re, ag, val, tp) %>%
    tbl_df()

## test column name requirement
bad_name_data <- simulated_data %>% rename(subject = subjectId)
missing_column_data <- simulated_data %>% select(-ag)
extra_column_data <- simulated_data %>% mutate(extraCol = 1:n())

test_that("The columns are checked and selected correctly", {
    expect_error( validate_data(bad_name_data) )

    expect_error( validate_data(missing_column_data) )

    expect_identical(FC_COLUMN_NAMES,
                     colnames(validate_data(extra_column_data)))
})

## test non-numeric/NA values
single_na_data <- simulated_data
single_na_data$val[10] <- NA

multiple_na_data <- simulated_data
multiple_na_data$val[c(10, 14, 23:28)] <- NA

non_numeric_data <- simulated_data
non_numeric_data$val[22] <- "bad"
    
na_non_numeric_data <- non_numeric_data
na_non_numeric_data$val[11] <- NA

test_that("The val column is properly checked and filtered", {
    nrow_sim <- nrow(simulated_data)

    expect_warning( validate_data(single_na_data) )
    expect_equal(nrow_sim - 1, nrow(validate_data(single_na_data)))

    expect_warning( validate_data(multiple_na_data) )
    expect_equal(nrow_sim - 8, nrow(validate_data(multiple_na_data)))

    expect_warning( validate_data(non_numeric_data) )
    expect_equal(nrow_sim - 1, nrow(validate_data(non_numeric_data)))

    expect_warning( validate_data(na_non_numeric_data) )
    expect_equal(nrow_sim - 2, nrow(validate_data(na_non_numeric_data)))
})
    

## test non-standard tp
non_numeric_tp_data <- simulated_data
non_numeric_tp_data$tp[3] <- "tp"

non_dense_data <- simulated_data
non_dense_data$tp[c(1,4,7)] <- "1.5"

test_that("The tp column is properly checked and adjusted", {
    expect_error( validate_data(non_numeric_tp_data) )
    expect_warning( validate_data(non_dense_data) )
    dense_data <- validate_data(non_dense_data)
    dense_tps <- simulated_data$tp
    dense_tps[dense_tps == 2] <- 3
    dense_tps[c(1,4,7)] <- 2
    expect_equal(dense_tps, dense_data$tp)
})

