#' Plot the antigen means as a barplot
#'
#' This function plots the antigen means from a model fit as a
#' barplot with additional options for sorting and filtering the
#' data displayed.
#'
#' @param result The MoBAMAResult object.
#' @param agOrderTable A \code{data.frame} with all ag/tp combinations
#' to include as well as ordering labeling and color information.
#' Should have the following columns:
#' ag, tp, order, label, group, color, barlabel, barcolor
#' @param agText The label for the antigen axis. Defaults to 'Antigen'.
#' @param meanText The label for the antigen mean axis. Defaults to 'Mean'.
#' @param colorText The label for the color legend. Defaults to 'Timepoint'.
#' @param agLines A string defining the color for lines separating groups
#' (by label) on the antigen axis or \code{NULL} for no lines.
#' Defaults to 'white'.
#' @param incIntervals A boolean indicating whether to include sampling
#' intervals for the antigen means. Defaults to \code{FALSE}.
#' @param intervalPtSize The size for the center point of the
#' sampling interval. Defaults to 3.
#' @param intervalBarSize The size for the sampling interval bars.
#' Defaults to 1.5
#' @param intervalColor A string defining the color for the sampling interval
#' error bars. Defaults to 'gray30'.
#'
#' @return A ggplot barplot object.
#'
#' @export
antigen_barplot_custom <- function(result,
                                   agOrderTable,
                                   agText = "Antigen",
                                   meanText = "Mean",
                                   colorText = "Timepoint",
                                   aglines = "white",
                                   incIntervals = F,
                                   intervalPtSize = 3,
                                   intervalBarSize = 1.5,
                                   intervalColor = "gray30") {
    agOrderTable <- agOrderTable %>%
        mutate(barlabel = as.character(barlabel))
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
        labs(y = meanText) +
        coord_flip()
    if (nrow(barColors) > 1) {
        barPlot <- barPlot +
            geom_bar(aes(order, mu_ag, fill = barlabel),
                     stat="identity", alpha = 0.9) +
        scale_fill_manual(limits = barColors$barlabel,
                          values = barColors$barcolor) +
            labs(fill = colorText)
    } else {
        barPlot <- barPlot +
            geom_bar(aes(order, mu_ag),
                     stat="identity", alpha = 0.9)
    }
    if (!is.null(aglines)) {
        barPlot <- barPlot +
            geom_vline(xintercept = aggrpsep+0.5, color=aglines)
    }
    if (incIntervals) {
        barPlot <- barPlot +
            geom_point(aes(order, mu_ag),
                       size = intervalPtSize,
                       color = intervalColor) +
            geom_errorbar(aes(order,
                              ymin = q025,
                              ymax = q975),
                          size = intervalBarSize,
                          color = intervalColor)
    }
    barPlot +
        scale_x_discrete(agText, limits = agax$order,
                         breaks = agax$order[agsep2],
                         labels = agax$label[agsep2]) +
        theme(axis.text.x = element_text(color = agax$color[agsep2]))
}
