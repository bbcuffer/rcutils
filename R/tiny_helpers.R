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

#' Cut a vector into n equal parts
#' @param x A numeric vector.
#' @param n The number of equal sized parts.
#' @param include.lowest Passed to \code{\link{cut}}. The default value used here (T)
#'    overwrites \code{cut}'s usual behaviour
#'    of excluding the lowest value in the dataset from any group.
#' @param ... other arguments passed to \code{\link{cut}}
#' @return An ordered factor, dividing x in to n-ths, with values
#' "1ofn" < "2ofn", < ... < "nofn"
cut_into_n <- function(x, n, include.lowest=T, ...){
  split <- 0:n/n
  labns <- paste0(1:n, "of", n)
  ## labns[c(1,n)] <- c("small","large")
  cut_up <- cut(x, quantile(x, split, na.rm=TRUE),
                include.lowest=include.lowest, labels=labns, ...)
  as.ordered(cut_up)
}

#' Cut a vector at breakpoints defined by quantiles
#'
#' @param x A numeric vector
#' @param at The quantiles at which to break x (defined in [0,1] rather than [0,100])
#' @inheritParams cut_into_n
#' @return A character vector, dividing x at the quantiles given of the form (e.g.) "0-25\%", "25-33\%"...
cut_at <- function(x, at, include.lowest=T, ...){

    if(x[1] != 0) x <- c(0, x)
    if(x[length(x)] != 1) x <- c(x, 1)
    breaks <- quantile(x, at, na.rm=TRUE)
    n <- length(at) - 1
    where <- gsub("\\%", "", names(breaks))
    labns <- paste0(where[1:n], "-", where[-1], "%")
    cut(x, breaks, include.lowest=include.lowest, labels=labns, ...)
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
