\name{analyze_hrv}
\alias{analyze_hrv}
\title{Analyze heart rate variability}
\usage{
analyze_hrv(
  x,
  domains = c("time", "frequency", "nonlinear"),
  spectrum_method = c("welch", "ar", "lomb"),
  recording_type = c("auto", "short_term", "long_term")
)
}
\arguments{
  \item{x}{An \code{hrv_record} or \code{hrv_preprocessed} object.}
  \item{domains}{Domains to compute.}
  \item{spectrum_method}{Spectrum estimator.}
  \item{recording_type}{Recording type.}
}
\value{
An object of class \code{hrv_result}.
}
\description{
Compute time-domain, frequency-domain, nonlinear, and derived HRV metrics.
}
\examples{
rr <- 1000 + 50 * sin(seq(0, 6 * pi, length.out = 300))
res <- analyze_hrv(hrv_record(rr))
print(res)
}
