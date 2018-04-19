#' Barplot of antigen means
#' TODO: add more options
#'         colors (by antigen group, user-specified)
#'       make the plot cleaner/nicer
#'       not hard code axis labels?
#'
#' @param result the MoBAMAResult object
#' @param includeIntervals include sampling intervals for the probabilities.
#' Defaults to false because of identifiability issues
#'
#' @return a ggplot plot object
#'
#' @export
antigen_barplot <- function(result,
                            includeIntervals = F) {
    barPlot <- ggplot(result$mu_ag, aes(ag, mu_ag)) +
        geom_bar(stat="identity", alpha = 0.7) +
        labs(x = "Antigen", y = "Mean") +
        coord_flip()
    if (includeIntervals) {
        barPlot <- barPlot +
            geom_point(size = 3, color = "gray30") +
            geom_errorbar(aes(ymin = q025, ymax = q975),
                          size = 1.5, color = "gray30")
    }
    barPlot
}
