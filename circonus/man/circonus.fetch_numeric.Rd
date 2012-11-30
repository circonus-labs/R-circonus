\name{circonus.fetch_numeric}
\alias{circonus.fetch_numeric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{pull numeric data from Circonus}
\description{this method will pull data for a specific metric over
the specified date range and granularity}
\usage{
circonus.fetch_numeric(obj, checkid, metric,
                       start, end, period,
                       type = "", verbose = FALSE, na.rm = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{the api object returned from circonus.default(apikey)}
  \item{checkid}{the numeric ID of a Circonus check}
  \item{metric}{the name of a metric collected on the specific Circonus check}
  \item{start}{the beginning of the date range of interest}
  \item{end}{the end of the date range of interest}
  \item{period}{the period (granularity) in seconds on which data should be fetched}
  \item{type}{the facet of data you are interested in. possible values include "count", "value", "stddev", "derivative", "derivative_stddev", "counter", "counter_stddev".  IF the type is unrecognized, all will be returned.}
  \item{verbose}{optional verbosity around the HTTP API calls}
  \item{na.rm}{logical. If FALSE, NA values will be omitted from the resulting data set}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The return value is a list of time series data.
\item{$whence}{a vector of all timestamps corresponding to data in other returned time series}
\item{$count}{each element in this vector represents the number of samples collected over the period}
\item{$value}{each element in this vector represents the average of samples over the period}
\item{$stddev}{each element in this vector represents the standard deviation of samples over the period}
\item{$derivative}{each element in this vector represents the average per-second rate-of-change over the samples in the period}
\item{$derivative_stddev}{each element in this vector represents the standard devision over the per-second rate of change over the samples in the period}
\item{$counter}{each element in this vector represents the average positive per-second rate-of-change over the samples in the period}
\item{$counter_stddev}{each element in this vector represents the standard devision over the positive per-second rate of change over the samples in the period}

If \code{type} is specified, only the \code{$whence} and the column requested are returned.
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
