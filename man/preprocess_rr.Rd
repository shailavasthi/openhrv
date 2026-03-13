\name{preprocess_rr}
\alias{preprocess_rr}
\title{Preprocess RR intervals}
\usage{
preprocess_rr(
  x,
  artifact_method = c("automatic", "none"),
  detrend = c("smoothness_priors", "none"),
  lambda = 500,
  interpolate = c("cubic_spline", "none"),
  resample_hz = 4
)
}
\arguments{
  \item{x}{An \code{hrv_record} object.}
  \item{artifact_method}{Artifact handling method.}
  \item{detrend}{Detrending method.}
  \item{lambda}{Smoothness priors lambda.}
  \item{interpolate}{Interpolation method for evenly sampled tachograms.}
  \item{resample_hz}{Interpolation frequency in Hz.}
}
\value{
An object of class \code{hrv_preprocessed}.
}
\description{
Screen, correct, interpolate, and detrend RR interval series.
}
\examples{
rec <- hrv_record(rep(1000, 300))
prep <- preprocess_rr(rec)
print(prep)
}
