#' Length of the Trues
#'
#' @param x a logical vector
#' @return the proportion of \code{x} that is T
lt <- function(x) sum(x)/length(x)

#' Old function for splitting continuous vectors into chunks.
#'
#' Replaced by \code{\link{cut_into_n}} and \code{\link{cut_at}}
quantiles <- function(x, n, include.lowest=T, ...){
  if(length(n)==1) split <- 0:n/n
  else split <- c(0,n,1)
  cut(x, quantile(x, split), include.lowest=include.lowest, ...)
}

#' A point between two others
#' @param x The points between which the function should find a value
#' @param p How far between the two points
#' @param y Alternative way to specify the second point we should look between
#' @return a single value, 100*p% of the way between x[1] and x[2] or between x and y.
between <- function(x, p, y=NULL){
   if(length(x)==2) x[1] + p*diff(x)
   else x + p*(y-x)
}

#' A smaller range
#' @param rng The original range
#' @param x How much to shrink it by
#' @return A new, smaller range
shrink <- function(rng, x)
   c( sum( c(1-x/2, x/2)*rng ), sum(c(x/2, 1-x/2)*rng) )
