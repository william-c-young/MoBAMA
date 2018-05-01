.breadth_scores <- function(resp) {
    fab <- resp %>%
        group_by(group, subjectId, ag) %>%
        summarize(prob = max(responseProb)) %>%
        group_by(group, subjectId) %>%
        summarize(fabBreadth = mean(prob))
    fcr <- resp %>%
        group_by(group, subjectId, re) %>%
        summarize(prob = max(responseProb)) %>%
        group_by(group, subjectId) %>%
        summarize(fcRBreadth = mean(prob))
    fab %>%
        left_join(fcr, by = c("group", "subjectId"))
}

#' Gets fab and FcR breadth scores per subject from
#' a MoBAMAResult object
#'
#' @param result The MoBAMAResult object.
#' @param agClasses A named list of antigen classes, each item being
#' a vector of antigen names used in the data to filter by before computing scores.
#'
#' @return A \code{data.frame} containing breadth scores for each subject,
#' for all antigens as well as for each antigen class included in agClasses.
#'
#' @export
breadth_scores <- function(result,
                           agClasses = NULL) {
    resp <- responses(result)
    scores <- .breadth_scores(resp) %>%
        mutate(agClass = "--All--")
    for ( c in names(agClasses) ) {
        cScores <- .breadth_scores(
            resp %>% dplyr::filter(ag %in% agClasses[[c]])
        ) %>%
            mutate(agClass = c)
        scores <- scores %>%
            bind_rows(cScores)
    }
    scores
}
