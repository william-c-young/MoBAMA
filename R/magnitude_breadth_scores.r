.magnitude_breadth_scores <- function(resp, mu_ag, mu_re) {
    agNorm = sum(mu_ag %>%
                 group_by(ag) %>%
                 summarize(maxmu = max(mu_ag)) %>%
                 select(maxmu))
    fab <- resp %>%
        left_join(mu_ag %>% select(ag, mu_ag),
                  by = "ag") %>%
        group_by(group, subjectId, ag, mu_ag) %>%
        summarize(magProb = max(mu_ag) * max(responseProb)) %>%
        group_by(group, subjectId) %>%
        summarize(fabMagBreadth = sum(magProb) / agNorm)
    if (is.null(mu_re)) return(fab)
 
    reNorm = sum(mu_re %>%
                 group_by(re) %>%
                 summarize(maxmu = max(mu_re)) %>%
                 select(maxmu))
    fcr <- resp %>%
        left_join(mu_re %>% select(re, mu_re),
                  by = "re") %>%
        group_by(group, subjectId, re, mu_re) %>%
        summarize(magProb = max(mu_re) * max(responseProb)) %>%
        group_by(group, subjectId) %>%
        summarize(fcRMagBreadth = sum(magProb) / reNorm)
    fab %>%
        left_join(fcr, by = c("group", "subjectId"))
}

#' Gets fab and FcR breadth scores per subject from
#' a MoBAMAResult object
#'
#' @param result The MoBAMAResult object.
#' @param agClasses A named list of antigen classes, each item being
#' a vector of antigen names used in the data to filter by before computing scores.
#' Defaults to \code{NULL}, indicating no filtering.
#' @param reClasses A named list of Fc variable classes, each item being
#' a vector of Fc variable names used in the data to filter by before computing scores.
#' Defaults to \code{NULL}, indicating no filtering.
#' @param includeMagBreadth A boolean indicating whether to include
#' magnitude-weighted breadth scores. Defaults to \code{FALSE}.
#'
#' @return A \code{data.frame} containing breadth scores for each subject,
#' for all antigens as well as for each antigen class included in agClasses.
#'
#' @export
magnitude_breadth_scores <- function(result,
                                     agClasses = NULL,
                                     reClasses = NULL) {
    scores <- .collect_scores(result,
                              agClasses,
                              reClasses,
                              .magnitude_breadth_scores,
                              result$mu_ag,
                              result$mu_re
                              )
    if (result$dataType == "bama") {
        scores %>% select(-reClass, -fcRMagBreadth)
    }
    else { scores }
}
