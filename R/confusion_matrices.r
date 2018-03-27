##' Calculates a confusion matrix from response data
##'
##' Calculates a vector of cutoffProb = cutoff, tp, fp, tn, and fn
##' according to provided response data
##'
##' @param cutoff the cutoff probability to use for defining a response
##' @param rData the response data to check. Must include columns
##' isResponder and responseProb.
##'
##' @return a vector containing the given cutoff probability and
##' confusion matrix entries tp, fp, tn, and fn
.conf <- function(cutoff, rData) {
    c(cutoff, sum(rData$responseProb >= cutoff & rData$isResponder),
      sum(rData$responseProb >= cutoff & !rData$isResponder),
      sum(rData$responseProb < cutoff & !rData$isResponder),
      sum(rData$responseProb < cutoff & rData$isResponder))
}

##' Generates a table of confusion matrices response data
##'
##' @param respData the response data to check. Must include columns
##' isResponder and responseProb.
##' @param cutoffs a set of cutoff probabilities for defining response
##'
##' @return a data.frame of confusion matrices for the provided
##' cutoff probabilities with columns cutoffProb, tp, fp, tn, and fn
##' 
.confusion_matrices <- function(respData, cutoffs) {
    confMat <- as.data.frame(t(sapply(cutoffs, .conf, respData)))
    names(confMat) <- c("cutoffProb", "tp", "fp", "tn", "fn")
    confMat
}
