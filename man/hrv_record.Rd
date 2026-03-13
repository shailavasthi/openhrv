\name{hrv_record}
\alias{hrv_record}
\title{Construct an HRV record}
\usage{
hrv_record(rr, time = NULL, unit = c("ms", "s"), metadata = list())
}
\arguments{
  \item{rr}{Numeric vector of RR or NN intervals.}
  \item{time}{Optional numeric vector of cumulative beat timestamps in seconds.}
  \item{unit}{Interval unit, either \code{"ms"} or \code{"s"}.}
  \item{metadata}{Named list of optional metadata.}
}
\value{
An object of class \code{hrv_record}.
}
\description{
Validate RR interval input and store it with timestamps and metadata.
}
\examples{
rr <- rep(1000, 300)
rec <- hrv_record(rr)
print(rec)
}
