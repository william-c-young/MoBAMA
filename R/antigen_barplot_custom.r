#' Plot the antigen means as a barplot
#'
#' This function plots the antigen means from a model fit as a
#'   barplot.
#' TODO: add more options
#'       tp filtering
#'       colors (by antigen group, user-specified)
#'       make the plot cleaner/nicer
#'       not hard code axis labels?
#'
#' @param result The MoBAMAResult object.
#' @param includeIntervals A boolean indicating whether to
#'   include sampling intervals for the antigen means.
#'   Defaults to \code{FALSE}.
#'
#' @return A ggplot barplot object.
#'
#' @export
antigen_barplot_custom <- function(result,
                                   agOrderTable,
                                   agTitle = "Antigen",
                                   meanTitle = "Mean",
                                   colorTitle = "Timepoint",
                                   aglines = "white",
                                   incIntervals = F,
                                   intervalSize = 3,
                                   intervalBarSize = 1.5,
                                   intervalColor = "gray30") {
    barColors = agOrderTable %>%
        arrange(order) %>%
        select(barlabel, barcolor) %>%
        distinct()
    plotData <- result$mu_ag %>%
        inner_join(agOrderTable, by = c("ag", "tp"))
    agax <- plotData %>%
        select(order, label, group, color) %>%
        distinct() %>%
        arrange(order)
    agsep <- cumsum(table(agax$label))
    agsep2 <- ceiling((agsep + c(0, agsep[1:(length(agsep)-1)])) / 2)
    aggrpsep <- cumsum(table(agax$group))
        
    barPlot <- ggplot(plotData) +
        labs(y = meanTitle) +
        coord_flip()
    if (nrow(barColors) > 1) {
        barPlot <- barPlot +
            geom_bar(aes(order, mu_ag, fill = factor(barlabel)),
                     stat="identity", alpha = 0.9) +
        scale_fill_manual(limits = factor(barColors$barlabel),
                          values = barColors$barcolor) +
            labs(fill = colorTitle)
    } else {
        barPlot <- barPlot +
            geom_bar(aes(order, mu_ag),
                     stat="identity", alpha = 0.9)
    }
    if (!is.null(aglines)) {
        barPlot <- barPlot +
            geom_vline(xintercept = aggrpsep+0.5, color=aglines)
    }
    if (includeIntervals) {
        barPlot <- barPlot +
            geom_point(aes(order, mu_ag),
                       size = intervalSize,
                       color = intervalColor) +
            geom_errorbar(aes(order,
                              ymin = q025,
                              ymax = q975),
                          size = intervalBarSize,
                          color = intervalColor)
    }
    barPlot +
        scale_x_discrete(agTitle, limits = agax$order,
                         breaks = agax$order[agsep2],
                         labels = agax$label[agsep2]) +
        theme(axis.text.x = element_text(color = agax$color[agsep2]))
}
