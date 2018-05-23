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
antigen_barplot <- function(result,
                            includeIntervals = F) {
    agord <- fiFit$mu_ag %>%
    select(ag, tp) %>%
    arrange(ag, tp) %>% 
    mutate(order = 1:n(),
           label = ag,
           group = ag,
           color = "black",
           barlabel = tp,
           barcolor = rainbow(n=length(unique(tp)),
                              s = 0.8)[tp])

    antigen_barplot_custom(result, agord,
                           incIntervals = includeIntervals)
}
