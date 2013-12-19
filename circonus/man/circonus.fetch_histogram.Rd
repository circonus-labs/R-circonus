\name{circonus.fetch_histogram}
\alias{circonus.fetch_histogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{pull histogram data from Circonus}
\description{this method will pull data for a specific metric over
the specified date range and granularity}
\usage{
circonus.fetch_histogram(obj, checkid, metric,
                         start, end, period,
                         verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{the api object returned from circonus.default(apikey)}
  \item{checkid}{the numeric ID of a Circonus check}
  \item{metric}{the name of a metric collected on the specific Circonus check}
  \item{start}{the beginning of the date range of interest}
  \item{end}{the end of the date range of interest}
  \item{period}{the period (granularity) in seconds on which data should be fetched}
  \item{verbose}{optional verbosity around the HTTP API calls}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The return value is a list of time series data.
\item{$whence}{a vector of all timestamps corresponding to data in other returned time series}
\item{$distributions}{each element in this vector represents a data.frame of bins and frequencies}
}
\references{
http://circonus.com/resources/api
}
\author{
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
