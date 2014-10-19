##' Plots a number of plots indicating which participants blinked often/long
##'
##' Provides a graphical summary of the overall proportion of samples that was coded as a blink 
##' per participant, the number of blinks (i.e., counting the number of onsets) per participants, and
##' the number of trials per participant in which more than 10 and 50% of all samples 
##' was marked as blink.  
##' 
##' @param blinkdata a list with in each component the output of \code{\link{blinkstatistics}}. Function assumes 
##' that component n contains the \code{\link{blinkstatistics}} of participant n.
##' 
##' @return Nothing
##' 
##' @details The bottom right plot, with the number of trials in which more than 50% of the samples was 
##' marked as a blink is a good indicator of tracking problems.
##' 
##' @author Hedderik van Rijn
##' @name plotblinkdiagnostics
##' @export plotblinkdiagnostics
##' @import data.table 

require(data.table)

plotblinkdiagnostics <- function(blinkinfo) {

    par(mfrow=c(2,2))
    
    x <- t(lapply(blinkinfo,function(X){if(is.null(X[1])) { NA } else {X$OverallProportion}}))
    range <- seq_along(x)
    plot(range,x,type="n",ylab="P(MissingDueToBlinks)",xlab="Participant Nr")
    text(range,x,range,cex=.8)
    
    x <- sapply(blinkinfo,function(X){if(is.null(X[1])) { NA } else {X$OverallOnsets}})
    plot(range,x,type="n",ylab="# BlinkOnsets",xlab="Participant Nr")
    text(range,x,range,cex=.8)
    
    for (blinkThreshold in c(.1,.5)) {
        x <- sapply(blinkinfo,function(X){if(is.null(X[1])) { NA } else {sum(X$PerTrialProportion$Proportion > blinkThreshold)}})
        plot(range,x,type="n",ylab=paste0("# Trials > Threshold (P>",blinkThreshold,")"),xlab="Participant Nr")
        text(range,x,range,cex=.8)
    }
    
}