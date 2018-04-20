#' Plots the response probabilities for all observations as a heatmap
#' with additional options for sorting and filtering both axes
#' TODO: implement color
#'
#' @param result the MoBAMAResult object
#' @param xorderTable a table with all ag/re/tp combinations to include
#' as well as ordering, labeling, and color information.
#' Should have the following columns: ag, re, tp, order, label, color
#' @param yorderTable a table with all subjectIds to include
#' as well as ordering, labeling, and color information.
#' Should have the following columns: subjectId, order, label, color
#' @param responseThreshold if not NULL, the threshold probability defining a response,
#' resulting in a two-color heatmap rather than a continuous heatmap
#' @param xtext the label for the x-axis
#' @param xlines the color for lines separating groups (by label) on the x-axis
#' or NULL for no lines
#' @param ytext the label for the y-axis
#' @param ylines the color for lines separating groups (by label) on the y-axis
#' or NULL for no lines
#'
#' @return a ggplot plot object
#'
#' @export
response_heatmap_custom <- function(result,
                             xorderTable,
                             yorderTable,
                             responseThreshold = NULL,
                             xtext = "Antigen/Fc Variable",
                             xlines = "white",
                             ytext = "SubjectId",
                             ylines = NULL) {
    resp <- responses(result) %>%
        dplyr::filter(paste(ag, re, tp) %in% paste(xorderTable$ag, xorderTable$re, xorderTable$tp),
               subjectId %in% yorderTable$subjectId)
    hmData <- resp %>%
        left_join(xorderTable %>%
                  rename(xorder = order,
                         xlabel = label,
                         xcolor = color),
                  by = c("ag", "re", "tp")) %>%
        left_join(yorderTable %>%
                  rename(yorder = order,
                         ylabel = label,
                         ycolor = color),
                  by = "subjectId") %>%
        arrange(xorder, yorder)

    xax <- hmData %>%
        select(xorder, xlabel, xcolor) %>%
        distinct() %>%
        arrange(xorder)
    xsep <- cumsum(table(xax$xlabel))
##    xsep2 <- floor((xsep + c(0, xsep[1:(length(xsep)-1)])) / 2)
    xsep2 <- ceiling((xsep + c(0, xsep[1:(length(xsep)-1)])) / 2)

    yax <- hmData %>%
        select(yorder, ylabel, ycolor) %>%
        distinct() %>%
        arrange(yorder)
    ysep <- cumsum(table(yax$ylabel))
##    ysep2 <- floor((ysep + c(0, ysep[1:(length(ysep)-1)])) / 2)
    ysep2 <- ceiling((ysep + c(0, ysep[1:(length(ysep)-1)])) / 2)

    hmPlot <- NULL
    if (is.null(responseThreshold)) {
        hmPlot <- ggplot(hmData) +
            geom_tile(aes(xorder, yorder, fill=responseProb))
    }
    else {
        ggplot(hmData) +
            geom_tile(aes(xorder, yorder,
                          fill=responseProb > responseThreshold)) +
            scale_fill_manual(values = c("darkred", "darkgreen"))
    }
    
    if (!is.null(xlines)) {
        hmPlot <- hmPlot +
            geom_vline(xintercept = xsep+0.5, color=xlines)
    }
    if (!is.null(ylines)) {
        hmPlot <- hmPlot +
            geom_hline(yintercept = ysep+0.5, color=ylines)
    }

    hmPlot +
        scale_x_discrete(xtext, limits=xax$xorder,
                         breaks=xax$xorder[xsep2], labels=xax$xlabel[xsep2]) +
        scale_y_discrete(ytext, limits=yax$yorder,
                         breaks=yax$yorder[ysep2], labels=yax$ylabel[ysep2]) +
        theme(axis.text.x = element_text(angle = 45, hjust=.5, vjust=.5,
                                         color = xax$xcolor[xsep2]),
              axis.text.y = element_text(color = yax$ycolor[ysep2]))
}
