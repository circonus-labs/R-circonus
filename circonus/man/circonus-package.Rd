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

}
\author{
Theo Schlossnagle <theo.schlossnagle@circonus.com>

Maintainer: <support@circonus.com>
}
\references{
}
\keyword{ package }
\seealso{
}
\examples{

> require(circonus)
Loading required package: circonus

> api <- circonus.default('1b5ecf6c-ee1c-453d-8099-9445d7a84f66')
> checkid <- 1
> metric_name <- 'transactions'
> period <- 300 # (seconds)
> cdata <- circonus.fetch_numeric(api, checkid, metric_name,
                                  '2011-01-01', '2011-04-01', period)

> quantile(na.omit(data$counter), probs=c(0.95))
     95% 
209.2887 
}
