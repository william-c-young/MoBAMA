#' Summarizes the responses from analyzing (particularly) BAMA data
#' into a table of confusion matrices
#'
#' Calculates confusion matrices from a response dataset and truth
#' based on subjects (each subject considered a responder or not) for a series
#' of cutoff probabilities for calling responses and false-discovery rates.
#' Subsets on individual antigen and timepoints as well as aggregates.
#'
#' @param result the MoBAMAResult object
#' @param subjectData the data defining which subjects are believed to be responders.
#' data.frame with columns subjectId and isResponder
#' @param fdrs a vector of false-discovery rates
#' @param cutoffs a vector of cutoff probabilities for calling responses
#' 
#' @return a table of confusion matrix entries with columns
#' antigen, tp, fdr, cutoffProb, tp, fp, tn, fn
#'
#' @export
response_call_evaluation <- function(result, subjectData, fdrs, cutoffs) {
    resp <- responses(result)
    confData <- resp %>%
        left_join(subjectData, by = "subjectId") %>%
        select(ag, tp, responseProb, isResponder)

    fdrs_full <- c(sapply(cutoffs, efdr, resp = confData), fdrs)
    cutoffs_full <- c(cutoffs, sapply(fdrs, fdr_cutoff, resp = confData))
        
    respSummary <- .confusion_matrices(confData, cutoffs_full) %>%
        mutate(ag = "-All-", timepoint = "-All-", fdr = fdrs_full)
    for (filterTp in sort(unique(confData$tp))) {
        subData <- confData %>% dplyr::filter(tp == filterTp)
        respSummary <- respSummary %>%
            bind_rows(.confusion_matrices(subData, cutoffs_full) %>%
                      mutate(ag = "-All-",
                             timepoint = as.character(filterTp),
                             fdr = fdrs_full))
    }
    for (filterAg in sort(unique(confData$ag))) {
        subData <- confData %>% dplyr::filter(ag == filterAg)
        respSummary <- respSummary %>%
            bind_rows(.confusion_matrices(subData, cutoffs_full) %>%
                      mutate(ag = filterAg, timepoint = "-All-", fdr = fdrs_full))
        for (filterTp in sort(unique(confData$tp))) {
            subData2 <- subData %>% dplyr::filter(tp == filterTp)
            respSummary <- respSummary %>%
                bind_rows(.confusion_matrices(subData2, cutoffs_full) %>%
                          mutate(ag = filterAg,
                                 timepoint = as.character(filterTp),
                                 fdr = fdrs_full))
        }
    }
    respSummary %>% arrange(ag, timepoint, -cutoffProb)
}
