\name{metrics_table}
\alias{metrics_table}
\title{Convert metrics to a flat table}
\usage{
metrics_table(
  x,
  domain = c("all", "time", "frequency", "nonlinear", "quality"),
  shape = c("wide", "long")
)
}
\arguments{
  \item{x}{An \code{hrv_result} object.}
  \item{domain}{Which domain to return.}
  \item{shape}{Output shape, either \code{"wide"} or \code{"long"}.}
}
\value{
A \code{data.frame}.
}
\description{
Flatten nested result metrics into a tabular format suitable for analysis pipelines.
}
\examples{
rr <- 1000 + 50 * sin(seq(0, 6 * pi, length.out = 300))
res <- analyze_hrv(hrv_record(rr))
metrics_table(res)
}
