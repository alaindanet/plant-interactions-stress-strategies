% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/date.r
\name{yearmon2month}
\alias{yearmon2month}
\title{Transform yearmon date in # of month since the start of the experiment}
\usage{
yearmon2month(date, starting = zoo::as.yearmon("nov. 2015"))
}
\arguments{
\item{date}{a yearmon object}

\item{starting}{a yearmon object}
}
\value{
Returns a integer vector corresponding to a number of month since the
start of the experiment.
}
\description{
Transform yearmon date in # of month since the start of the experiment
}
\details{
This function is a wrapper of lubridate functions to transform a
yearmonth object to a number of month since the start of the experiment. The
starting parameter is the starting month, defined by default to the month of
sapling transplantation.
}
\examples{
yearmon2month(date = zoo::as.yearmon("janv. 2016"))

}
\seealso{
zoo::as.yearmon lubridate::interval  lubridate::duration
}
