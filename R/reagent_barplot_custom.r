#' Plot the reagent means as a barplot
#'
#' This function plots the reagent means from a model fit as a
#' barplot.
#'
#' @param result The MoBAMAResult object.
#' @param reOrderTable A \code{data.frame} with all re/tp combinations
#' to include as well as ordering labeling and color information.
#' Should have the following columns:
#' re, tp, order, label, group, color, barlabel, barcolor
#' @param reText The label for the Fc axis. Defaults to 'Fc Variable'.
#' @param meanText The label for the Fc mean axis. Defaults to 'Mean'.
#' @param colorText The label for the color legend. Defaults to 'Timepoint'.
#' @param reLines A string defining the color for lines separating groups
#' (by label) on the Fc axis or \code{NULL} for no lines.
#' Defaults to 'white'.
#' @param incIntervals A boolean indicating whether to include sampling
#' intervals for the Fc variable means. Defaults to \code{FALSE}.
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
reagent_barplot_custom <- function(result,
                                   reOrderTable,
                                   reText = "Fc Variable",
                                   meanText = "Mean",
                                   colorText = "Timepoint",
                                   relines = "white",
                                   incIntervals = F,
                                   intervalPtSize = 3,
                                   intervalBarSize = 1.5,
                                   intervalColor = "gray30") {
    reOrderTable <- reOrderTable %>%
        mutate(barlabel = as.character(barlabel))
    barColors = reOrderTable %>%
        arrange(order) %>%
        select(barlabel, barcolor) %>%
        distinct()
    plotData <- result$mu_re %>%
        inner_join(reOrderTable, by = c("re", "tp"))
    reax <- plotData %>%
        select(order, label, group, color) %>%
        distinct() %>%
        arrange(order)
    resep <- cumsum(table(reax$label))
    resep2 <- ceiling((resep + c(0, resep[1:(length(resep)-1)])) / 2)
    regrpsep <- cumsum(table(reax$group))
        
    barPlot <- ggplot(plotData) +
        labs(y = meanText) +
        coord_flip()
    if (nrow(barColors) > 1) {
        barPlot <- barPlot +
            geom_bar(aes(order, mu_re, fill = barlabel),
                     stat="identity", alpha = 0.9) +
        scale_fill_manual(limits = barColors$barlabel,
                          values = barColors$barcolor) +
            labs(fill = colorText)
    } else {
        barPlot <- barPlot +
            geom_bar(aes(order, mu_re),
                     stat="identity", alpha = 0.9)
    }
    if (!is.null(relines)) {
        barPlot <- barPlot +
            geom_vline(xintercept = regrpsep+0.5, color=relines)
    }
    if (incIntervals) {
        barPlot <- barPlot +
            geom_point(aes(order, mu_re),
                       size = intervalPtSize,
                       color = intervalColor) +
            geom_errorbar(aes(order,
                              ymin = q025,
                              ymax = q975),
                          size = intervalBarSize,
                          color = intervalColor)
    }
    barPlot +
        scale_x_discrete(reText, limits = reax$order,
                         breaks = reax$order[resep2],
                         labels = reax$label[resep2]) +
        theme(axis.text.x = element_text(color = reax$color[resep2]))
}
