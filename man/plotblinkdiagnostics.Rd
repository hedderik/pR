\name{plotblinkdiagnostics}
\alias{plotblinkdiagnostics}
\title{Plots a number of plots indicating which participants blinked often/long}
\arguments{
  \item{blinkdata}{a list with in each component the output
  of \code{\link{blinkstatistics}}. Function assumes that
  component n contains the \code{\link{blinkstatistics}} of
  participant n.}
}
\value{
  Nothing
}
\description{
  Provides a graphical summary of the overall proportion of
  samples that was coded as a blink per participant, the
  number of blinks (i.e., counting the number of onsets)
  per participants, and the number of trials per
  participant in which more than 10 and 50% of all samples
  was marked as blink.
}
\details{
  The bottom right plot, with the number of trials in which
  more than 50% of the samples was marked as a blink is a
  good indicator of tracking problems.
}
\author{
  Hedderik van Rijn
}

