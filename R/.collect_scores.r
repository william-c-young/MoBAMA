.collect_scores <- function(result,
                            agClasses,
                            reClasses,
                            breadthFn,
                            ...) {
    resp <- responses(result)
    scores <- breadthFn(resp, ...) %>%
        mutate(agClass = "--All--", reClass = "--All--")
    for (r in names(reClasses)) {
        rScores <- breadthFn(
            resp %>% dplyr::filter(re %in% reClasses[[r]]),
            ...
        ) %>% mutate(agClass = "--All--", reClass = r)
        scores <- scores %>%
            bind_rows(rScores)
    }
    for (c in names(agClasses)) {
        cScores <- breadthFn(
            resp %>% dplyr::filter(ag %in% agClasses[[c]]),
            ...
        ) %>% mutate(agClass = c, reClass = "--All--")
        scores <- scores %>%
            bind_rows(cScores)
        for (r in names(reClasses)) {
            rScores <- breadthFn(
                resp %>% dplyr::filter(ag %in% agClasses[[c]],
                                       re %in% reClasses[[r]]),
                ...
            ) %>% mutate(agClass = c, reClass = r)
            scores <- scores %>%
                bind_rows(rScores)
        }
    }
    scores
}
