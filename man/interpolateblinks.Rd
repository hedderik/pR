\name{interpolateblinks}
\alias{interpolateblinks}
\title{Interpolate missing data (due to blinks) in pupil dilation data}
\arguments{
  \item{pddt}{a pupil dilation \code{data.table} of a
  single participant containing at least the following two
  columns: \itemize{ \item{Dil} containing the dilation
  values with all blinks marked as NA. \item{Trial}
  indicating to which trial the current sample belongs. }}

  \item{type}{The type of linear interpolation to be
  performed, currently the only supported option is
  \code{linear}.}
}
\value{
  Returns an updated copy of the original
  \code{data.table}: \itemize{ \item{Dil} a copy of the
  original \code{Dil} column, but \code{NA} samples
  demarcated with actual data replaced with interpolated
  data. }
}
\description{
  Replaces all \code{NA} values in the Dil column by
  interpolated values. Currently only linear interpolation
  is supported, which performs a linear interpolation based
  on the first value before and after each sequence of
  \code{NA} values. Although some papers report using
  non-linear interpolation, in our experience this method
  sometimes gives interpolated values that are highly
  unlikely.
}
\details{
  Note that any \code{NA} values at the beginning and end
  of each trial are not replaced as there is no starting or
  end value for the interpolation to work with. It is
  probably advisable to remove all trials from further
  analyses that still have a lot of \code{NA}s after
  \code{interpolateblinks}.

  Because of this, trials consisting of just \code{NA}s are
  left unchanged.
}
\author{
  Hedderik van Rijn
}
\seealso{
  \code{\link{blinkstatistics}}
}

