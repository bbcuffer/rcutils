shrink <- function(rng, x)
  c( sum( c(1-x/2, x/2)*rng ), sum(c(x/2, 1-x/2)*rng) )
