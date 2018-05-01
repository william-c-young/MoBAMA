#' Plot the reagent means as a barplot
#'
#' This function plots the reagent means from a model fit as a
#'   barplot.
#' TODO: add more options
#'       tp filtering
#'       colors (by Fc group, user-specified)
#'       make the plot cleaner/nicer
#'       not hard code axis labels?
#'
#' @param result The MoBAMAResult object.
#' @param includeIntervals A boolean indicating whether to
#'   include sampling intervals for the reagent means.
#'   Defaults to \code{FALSE}.
#'
#' @return A ggplot barplot object.
#'
#' @export
reagent_barplot <- function(result,
                            includeIntervals = F) {
    barPlot <- ggplot(result$mu_re, aes(re, mu_re)) +
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
