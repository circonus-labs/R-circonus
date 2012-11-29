\name{circonus.fetch_numeric}
\alias{circonus.fetch_numeric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{pull numeric data from Circonus}
\description{this method will pull data for a specific metric over
the specified date range and granularity}
\usage{
circonus.fetch_numeric(obj, checkid, metric, start, end, period, type = "", verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{the api object returned from circonus.default(apikey)}
  \item{checkid}{the numeric ID of a Circonus check}
  \item{metric}{the name of a metric collected on the specific Circonus check}
  \item{start}{the beginning of the date range of interest}
  \item{end}{the end of the date range of interest}
  \item{period}{the period (granularity) in seconds on which data should be fetched}
  \item{type}{the facet of data you are interested in. possible values include "count", "value", "stddev", "derive", "derive_stddev", "counter", "counter_stddev".  IF the type is unrecognized, all will be returned.}
  \item{verbose}{optional verbosity around the HTTP API calls}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
