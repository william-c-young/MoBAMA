#' Runs a model on BAMA or Fc array data using stan
#'
#' Runs a chosen grouped mixture model on the provided data.
#' TODO: change params out to just list all the options?
#'       saving the stanfit object or returning it
#'       returning the summary in a better format
#'
#' @param bamaData the data to be modeled
#' @param dataType type of data to be modeled ('bama' or 'fc')
#' @param stanParams paramaters to pass to the stan command (number of cores, chains, iterations, etc.)
#' @param outFolder the folder to save the stan results to (or NULL to not save)
#' @param outFile the filename to save the stan results to -
#' defaults to '<yyyy-mm-dd>_MoBAMA_stanfit.rds'
#' @param ... additional parameters to pass to the stan function
#'
#' @return a summary of the model results
#'
#' @export
MoBAMA <- function(data,
                   dataType = "fc",
                   stanParams = list(iter = 2000, noChains = 1),
                   outFolder = NULL,
                   outFile = date_filename("MoBAMA_stanfit.rds"),
                   ...) {

    ## Model preparation
    dataType <- tolower(dataType)
    data <- prepare_data(data, dataType)
    modelData <- build_model_data(data, dataType)
    paramInit <- build_parameter_initialization(modelData)

    ## Run stan
    stanRes <- rstan::sampling(stanmodels[[paste0(dataType, "_model")]],
                    data = modelData,
                    init = function(){paramInit},
                    iter = stanParams$iter,
                    chains = stanParams$noChains,
                    ...)

    if (!is.null(outFolder)) {
        saveRDS(stanRes, file.path(outFolder, outFile))
    }
    
    output <- list(
        data = data,
        dataType = dataType,
        parameters = build_MoBAMA_summary(stanRes))

    ## output <- add_parameter_summaries(output)

    class(output) <- "MoBAMAResult"

    return(output)
}
