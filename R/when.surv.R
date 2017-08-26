when.surv <- function(mod, when){
  # Given a time, picks out the points on each stratum of
  # a KM survival curve closest to that time
  # can be combined with the models $surv and $upper/$lower
  # attributes to pull out point for a CI
  
  #when <- 52
  #mod <- mod1
  n.strata <- length(mod$strata)

  firsts <- 1+ c(0, cumsum(mod$strata)[1:(n.strata-1)])
  lasts <- cumsum(mod$strata)

  index0 <- numeric(n.strata)
  for(i in 1:n.strata){
    range <- firsts[i]:lasts[i]
    index0[i] <- which.min( (mod$time[range] - when)^2)
}

index <- index0 - 1 + firsts
names(index) <- names(mod$strata)
return(index)
}
