#' Cut a vector into n equal parts
#'
#' @param x A numeric vector.
#' @param n The number of equal sized parts.
#' @param labels Labels for the factor passed to \code{\link{cut}}.
#'    Default value is  "1ofn", "2ofn", ..."nofn"
#' @param ... other arguments passed to \code{\link{cut}}
#' @return An ordered factor, dividing x in to n-ths, with default values
#' "1ofn" < "2ofn", < ... < "nofn"
#' @examples
#' cut_into_n(100:1, 3)
#' @seealso \code{\link{cut_at}}
cut_into_n <- function(x, n, labels=paste0(1:n, "of", n), ...){
  split <- 0:n/n
  cut_up <- cut(x, quantile(x, split, na.rm=TRUE), labels=labels,
                include.lowest=T, ## Override cut's usual behaviour
                ## of excluding the lowest value in the dataset
                ## from any group.
                ordered_result=T,
                ...)
  return(cut_up)
}

#' Cut a vector at breakpoints defined by quantiles
#'
#' @param at The quantiles at which to break x
#'        (defined either in the range 0-100 or 0-1).
#' @inheritParams cut_into_n
#' @return An ordered factor, dividing x at the quantiles given by \code{at}. Labels are of the form (e.g.) "0-25\%", "25-33\%"...
#' @examples
#' cut_at(100:1, c(25, 75))
#' @seealso \code{\link{cut_into_n}}
cut_at <- function(x, at, ...){
    at <- sort(at)
    if(min(at) > 0) at <- c(0, at)
    if(max(at) > 1) at <- at/100 # Breaks can be specified in [0,1] or [0,100]
    if(max(at) < 1) at <- c(at, 1)
    breaks <- quantile(x, at, na.rm=TRUE)
    n <- length(at) - 1
    where <- gsub("\\%", "", names(breaks))
    labns <- paste0(where[1:n], "-", where[-1], "%")
    cut(x, breaks, include.lowest=T, labels=labns,
        ordered_result=T, ...)
}
