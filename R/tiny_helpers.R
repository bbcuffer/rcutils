lt <- function(x) sum(x)/length(x)
quantiles <- function(x, n, include.lowest=T, ...){
  if(length(n)==1) split <- 0:n/n
  else split <- c(0,n,1)
  cut(x, quantile(x, split), include.lowest=include.lowest, ...)
}
between <- function(x, p, y=NULL){
  if(length(x)==2) x[1] + p*diff(x)
  else x + p*(y-x)
}
shrink <- function(rng, x)
  c( sum( c(1-x/2, x/2)*rng ), sum(c(x/2, 1-x/2)*rng) )
