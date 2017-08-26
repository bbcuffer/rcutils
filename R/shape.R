shape <- function (expr, from, to, n = 101, anchor=NULL, above=F, ...) 
{
    sexpr <- substitute(expr)
    if (is.name(sexpr)) {
        fcall <- paste(sexpr, "(x)")
        expr <- parse(text = fcall)
      }
    else {
        if (!(is.call(sexpr) && match("x", all.vars(sexpr), nomatch = 0))) 
            stop("'expr' must be a function or an expression containing 'x'")
        expr <- sexpr
      }
    x <- seq.int(from, to, length = n)
    y <- eval(expr, envir = list(x = x), enclos = parent.frame())
    if(is.null(anchor))
      anchor <- ifelse(above, max(y), min(y))
    polygon(x[c(1, 1:n, n)], c(anchor, y, anchor), 
        ...)

# Examples of the function's use    
# curve(dnorm(x), -2, 2, ylim=c(-.1, 0.5))
# shape(dnorm(x), -2, -1.5)
# shape(dnorm(x), -1.4, -1, col=3)
# shape(dnorm(x), -.8, 0, col=4, anchor=-.1)
# shape(dnorm(x), 0.3, 1, col=5, above=T)
# shape(dnorm(x), 1.2, 1.8, col=5, border=3, above=T, anchor=0.5)
    
}


