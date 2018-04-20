#' Plots the response probabilities for all observations as a heatmap
#' TODO: add more options
#'         tp filtering?
#'         sorting axes
#'         colors (by group, ag/re?, user-specified)
#'       make the plot cleaner/nicer
#'       not hard code axis labels?
#'
#' @param result the MoBAMAResult object
#' @param responseThreshold if not NULL, the threshold probability defining a response,
#' resulting in a two-color heatmap rather than a continuous heatmap
#' @param groupby the field (ag or re) to group by
#'
#' @return a ggplot plot object
#'
#' @export
response_heatmap <- function(result,
                             responseThreshold = NULL,
                             groupby = "ag") {

    gbag <- groupby == "ag"
    xord <- NULL
    if (gbag) {
        xord <- result$data %>%
            select(ag, re, tp) %>%
            distinct() %>%
            arrange(ag, re, tp) %>%
            mutate(order = 1:n(),
                   label = ag,
                   color = "black")
    } else {
        xord <- result$data %>%
            select(ag, re, tp) %>%
            distinct() %>%
            arrange(re, ag, tp) %>%
            mutate(order = 1:n(),
                   label = re,
                   color = "black")
    }
    yord <- result$data %>%
        select(subjectId, group) %>%
        distinct() %>%
        arrange(group, subjectId) %>%
        mutate(order = 1:n(),
               label = subjectId,
               color = "black")

    response_heatmap_custom(result,
                            xord, yord,
                            xtext = ifelse(gbag, "Antigen", "Fc Variable"),
                            xlines = "white",
                            ytext = "Subject by Group",
                            ylines = NULL)
}
