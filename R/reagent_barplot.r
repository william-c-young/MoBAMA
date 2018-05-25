#' Plot the reagent means as a barplot
#'
#' This function plots the reagent means from a model fit as a
#'   barplot.
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
    reord <- result$mu_re %>%
        select(re, tp) %>%
        arrange(re, tp) %>% 
        mutate(order = 1:n(),
               label = re,
               group = re,
               color = "black",
               barlabel = tp,
               barcolor = rainbow(n=length(unique(tp)),
                                  s = 0.8)[tp])

    reagent_barplot_custom(result, reord,
                           incIntervals = includeIntervals)

    ## barPlot <- ggplot(result$mu_re, aes(re, mu_re)) +
    ##     geom_bar(stat="identity", alpha = 0.7) +
    ##     labs(x = "Antigen", y = "Mean") +
    ##     coord_flip()
    ## if (includeIntervals) {
    ##     barPlot <- barPlot +
    ##         geom_point(size = 3, color = "gray30") +
    ##         geom_errorbar(aes(ymin = q025, ymax = q975),
    ##                       size = 1.5, color = "gray30")
    ## }
    ## barPlot
}
