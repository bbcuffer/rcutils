quantiles <- function(x, n, include.lowest=T, ...){
  if(length(n)==1) split <- 0:n/n
  else split <- c(0,n,1)
  cut(x, quantile(x, split), include.lowest=include.lowest, ...)
}
