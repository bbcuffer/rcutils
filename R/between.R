between <- function(x, p, y=NULL){
  if(length(x)==2) x[1] + p*diff(x)
  else x + p*(y-x)
}
