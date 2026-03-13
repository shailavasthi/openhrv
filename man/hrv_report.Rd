\name{hrv_report}
\alias{hrv_report}
\title{Generate an HRV report}
\usage{
hrv_report(x, output = "html")
}
\arguments{
  \item{x}{An \code{hrv_result} object.}
  \item{output}{Output format. Only \code{"html"} is currently supported.}
}
\value{
The path to the generated report.
}
\description{
Render a simple HTML report for an HRV analysis result.
}
\examples{
\dontrun{
rr <- 1000 + 50 * sin(seq(0, 6 * pi, length.out = 300))
res <- analyze_hrv(hrv_record(rr))
hrv_report(res)
}
}
