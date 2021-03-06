% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tiny_helpers.R
\name{between}
\alias{between}
\title{A point between two others}
\usage{
between(x, p, y = NULL)
}
\arguments{
\item{x}{The points between which the function should find a value}

\item{p}{How far between the two points}

\item{y}{Alternative way to specify the second point we should look between}
}
\value{
a single value, 100*p% of the way between x[1] and x[2] or between x and y.
}
\description{
A point between two others
}
