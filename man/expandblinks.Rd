\name{expandblinks}
\alias{expandblinks}
\title{Expand blink regions to account for pre- and post-blink changes in dilatation}
\arguments{
  \item{pddt}{a pupil dilation \code{data.table} of a
  single participant containing at least the following
  three columns: \itemize{ \item{Dil} containing the
  dilation values with all blinks marked as NA.
  \item{Trial} indicating to which trial the current sample
  belongs. \item{Time} Real in miliseconds associated with
  each sample. Is used to calculate at which frequency this
  data was sampled. }}

  \item{rejectionWindow}{duration (in miliseconds) of the
  rejection window. All samples closer to a sample marked
  as a blink than this value with be set to \code{NA}.}
}
\value{
  Returns an updated copy of the original
  \code{data.table}: \itemize{ \item{Dil} a copy of the
  original \code{Dil} column, but samples replaced with
  \code{NA} when this sample is considered to be part of a
  blink }
}
\description{
  Sets samples within a specified time range before and
  after each observed blink to \code{NA} as just before and
  after a blink the pupillary response is often slightly
  effected. Note that the function assumes the input to be
  in chronological order within each trial.
}
\author{
  Hedderik van Rijn
}

