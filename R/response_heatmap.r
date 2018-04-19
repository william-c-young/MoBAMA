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
    resp <- responses(result)
    gbag <- groupby == "ag"
    .pifelse <- function(test, yes, no) {
        if (test) {
            yes
        }
        else {
            no
        }
    }
    hmData <- resp %>%
        mutate(name = .pifelse(gbag, str_c(ag, " ", re), str_c(re, " ", ag)),
               label = .pifelse(gbag, ag, re),
               nonlabel = .pifelse(gbag, re, ag)) %>%
        arrange(group, subjectId, label, nonlabel)
    combos <- hmData %>%
        select(name, label) %>%
        distinct() %>%
        arrange(label)
    xsep <- cumsum(table(combos$label))
    xsep2 <- floor((xsep + c(0, xsep[1:(length(xsep)-1)])) / 2)
    xtext <- ifelse(gbag,
                    "Antigen", "Fc Variable")
    hmPlot <- NULL
    if (is.null(responseThreshold)) {
        hmPlot <- ggplot(hmData) +
            geom_tile(aes(name, subjectId, fill=responseProb))
    }
    else {
        ggplot(hmData) +
            geom_tile(aes(name, subjectId,
                          fill=responseProb > responseThreshold)) +
            scale_fill_manual(values = c("darkred", "darkgreen"))
    }
    hmPlot +
        geom_vline(xintercept = xsep+0.5, color="white") +
        scale_x_discrete(xtext, limits=combos$name,
                         breaks=combos$name[xsep2], labels=names(xsep2)) +
        labs(y = "Subject by Group") +
        theme(axis.text.x = element_text(angle = 45, hjust=.5, vjust=.5))
}
