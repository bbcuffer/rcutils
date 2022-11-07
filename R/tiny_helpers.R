#' Length of the Trues
#'
#' @param x a logical vector
#' @return the proportion of \code{x} that is T
#' @export
lt <- function(x) sum(x)/length(x)

#' A point between two others
#' @param x The points between which the function should find a value
#' @param p How far between the two points
#' @param y Alternative way to specify the second point we should look between
#' @return a single value, 100*p% of the way between x[1] and x[2] or between x and y.
#' @export
between <- function(x, p, y=NULL){
   if(length(x)==2) x[1] + p*diff(x)
   else x + p*(y-x)
}

#' A smaller range
#' @param rng The original range
#' @param x How much to shrink it by
#' @return A new, smaller range
#' @export
shrink <- function(rng, x)
   c( sum( c(1-x/2, x/2)*rng ), sum(c(x/2, 1-x/2)*rng) )
