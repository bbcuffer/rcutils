% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tiny_helpers.R
\name{cut_at}
\alias{cut_at}
\title{Cut a vector at breakpoints defined by quantiles}
\usage{
cut_at(x, at, include.lowest = T, ...)
}
\arguments{
\item{x}{A numeric vector}

\item{at}{The quantiles at which to break x (defined in [0,1] rather than [0,100])}

\item{include.lowest}{Passed to \code{\link{cut}}. The default value used here (T)
overwrites \code{cut}'s usual behaviour
of excluding the lowest value in the dataset from any group.}

\item{...}{other arguments passed to \code{\link{cut}}}
}
\value{
A character vector, dividing x at the quantiles given of the form (e.g.) "0-25\%", "25-33\%"...
}
\description{
Cut a vector at breakpoints defined by quantiles
}
