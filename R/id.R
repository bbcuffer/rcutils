id <- function(x, y, lty = 3, ...) {
  usr <- par("usr")
  n <- length(x)
  xstart <- c(rep(usr[1], n), x)
  xend <- rep(x,2)
  ystart <- rep(y,2)
  yend <- c(y, rep(usr[3], n))
  segments(xstart, ystart, xend, yend, lty = lty, ...)
}
