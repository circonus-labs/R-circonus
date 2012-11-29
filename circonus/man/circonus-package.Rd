\name{circonus-package}
\alias{circonus-package}
\alias{circonus}
\docType{package}
\title{Import data sets from Circonus}
\description{Import data ses from via the Circonus public API}
\details{
\tabular{ll}{
Package: \tab circonus\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-11-28\cr
License: \tab BSD\cr
}

api <- circonus.default('<CIRCONUS_API_TOKEN>')
data <- api.fetch_numeric(1234,'metric_name','1997-04-01','1997-05-01', 1800, type="counter")
}
\author{
Theo Schlossnagle <theo.schlossnagle@circonus.com>

Maintainer: <support@circonus.com>
}
\references{
~~ Literature or other references for background information ~~
}
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
~~ simple examples of the most important functions ~~
}
