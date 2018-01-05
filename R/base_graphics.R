
# Old functions that work on base graphics

histrc <-
function (x, breaks = "Sturges", freq = NULL, probability = !freq, 
    include.lowest = TRUE, right = TRUE, density = NULL, angle = 45, 
    col = NULL, border = NULL, main = paste("Histogram of", xname), 
    xlim = range(breaks), ylim = NULL, xlab = xname, ylab, axes = TRUE, 
    plot = TRUE, labels = FALSE, nclass = NULL, ...) 
{
    if (!is.numeric(x)) 
        stop("'x' must be numeric")
    xname <- paste(deparse(substitute(x), 500), collapse = "\n")
    n <- length(x <- x[is.finite(x)])
    use.br <- !missing(breaks)
    if (use.br) {
        if (!missing(nclass)) 
            warning("'nclass' not used when 'breaks' is specified")
    }
    else if (!is.null(nclass) && length(nclass) == 1) 
        breaks <- nclass
    use.br <- use.br && (nB <- length(breaks)) > 1
    if (use.br) 
        breaks <- sort(breaks)
    else {
        if (!include.lowest) {
            include.lowest <- TRUE
            warning("'include.lowest' ignored as 'breaks' is not a vector")
        }
        if (is.character(breaks)) {
            breaks <- match.arg(tolower(breaks), c("sturges", 
                "fd", "freedman-diaconis", "scott"))
            breaks <- switch(breaks, sturges = nclass.Sturges(x), 
                `freedman-diaconis` = , fd = nclass.FD(x), scott = nclass.scott(x), 
                stop("unknown 'breaks' algorithm"))
        }
        else if (is.function(breaks)) {
            breaks <- breaks(x)
        }
        if (!is.numeric(breaks) || !is.finite(breaks) || breaks < 
            1) 
            stop("invalid number of 'breaks'")
        breaks <- pretty(range(x), n = breaks, min.n = 1)
        nB <- length(breaks)
        if (nB <= 1) 
            stop("hist.default: pretty() error, breaks=", format(breaks))
    }
    h <- diff(breaks)
    equidist <- !use.br || diff(range(h)) < 1e-07 * mean(h)
    if (!use.br && any(h <= 0)) 
        stop("'breaks' are not strictly increasing")
    freq1 <- freq
    if (is.null(freq)) {
        freq1 <- if (!missing(probability)) 
            !as.logical(probability)
        else equidist
    }
    else if (!missing(probability) && any(probability == freq)) 
        stop("'probability' is an alias for '!freq', however they differ.")
    diddle <- 1e-07 * stats::median(diff(breaks))
    fuzz <- if (right) 
        c(if (include.lowest) -diddle else diddle, rep.int(diddle, 
            length(breaks) - 1))
    else c(rep.int(-diddle, length(breaks) - 1), if (include.lowest) diddle else -diddle)
    fuzzybreaks <- breaks + fuzz
    h <- diff(fuzzybreaks)
    storage.mode(x) <- "double"
    storage.mode(fuzzybreaks) <- "double"
    counts <- .C("bincount", x, as.integer(n), fuzzybreaks, as.integer(nB), 
        counts = integer(nB - 1), right = as.logical(right), 
        include = as.logical(include.lowest), naok = FALSE, NAOK = FALSE, 
        DUP = FALSE, PACKAGE = "base")$counts
    if (any(counts < 0)) 
        stop("negative 'counts'. Internal Error in C-code for \"bincount\"")
    if (sum(counts) < n) 
        stop("some 'x' not counted; maybe 'breaks' do not span range of 'x'")
    #dens <- counts/(n * h)
    # Stop the STUPID behaviour of having the probabilities not
    # sum to one. 
    dens <- 100*counts/n
    mids <- 0.5 * (breaks[-1] + breaks[-nB])
    r <- structure(list(breaks = breaks, counts = counts, intensities = dens, 
        density = dens, mids = mids, xname = xname, equidist = equidist), 
        class = "histogram")
    if (plot) {
        plot(r, freq = freq1, col = col, border = border, angle = angle, 
            density = density, main = main, xlim = xlim, ylim = ylim, 
            xlab = xlab, ylab = ylab, axes = axes, labels = labels, 
            ...)
        invisible(r)
    }
    else {
        nf <- names(formals())
        nf <- nf[is.na(match(nf, c("x", "breaks", "nclass", "plot", 
            "include.lowest", "right")))]
        missE <- lapply(nf, function(n) substitute(missing(.), 
            list(. = as.name(n))))
        not.miss <- !sapply(missE, eval, envir = environment())
        if (any(not.miss)) 
            warning(sprintf(ngettext(sum(not.miss), "argument %s is not made use of", 
                "arguments %s are not made use of"), paste(sQuote(nf[not.miss]), 
                collapse = ", ")), domain = NA)
        r
    }
}
# Adjusts the labelling for piecharts - default behaviour of pie{} seems
# to be slightly left aligned

my.pie <- 
function (x, labels = names(x), edges = 200, radius = 0.8, clockwise = FALSE, 
    init.angle = if (clockwise) 90 else 0, density = NULL, angle = 45, 
    col = NULL, border = NULL, lty = NULL, main = NULL, ...) 
{
    if (!is.numeric(x) || any(is.na(x) | x < 0)) 
        stop("'x' values must be positive.")
    if (is.null(labels)) 
        labels <- as.character(1:length(x))
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)
    plot.new()
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1] > pin[2]) 
        xlim <- (pin[1]/pin[2]) * xlim
    else ylim <- (pin[2]/pin[1]) * ylim
    plot.window(xlim, ylim, "", asp = 1)
    if (is.null(col)) 
        col <- if (is.null(density)) 
            c("white", "lightblue", "mistyrose", "lightcyan", 
                "lavender", "cornsilk")
        else par("fg")
    col <- rep(col, length.out = nx)
    border <- rep(border, length.out = nx)
    lty <- rep(lty, length.out = nx)
    angle <- rep(angle, length.out = nx)
    density <- rep(density, length.out = nx)
    twopi <- if (clockwise) 
        -2 * pi
    else 2 * pi
    t2xy <- function(t) {
        t2p <- twopi * t + init.angle * pi/180
        list(x = radius * cos(t2p), y = radius * sin(t2p))
    }
    for (i in 1:nx) {
        n <- max(2, floor(edges * dx[i]))
        P <- t2xy(seq(x[i], x[i + 1], length = n))
        polygon(c(P$x, 0), c(P$y, 0), density = density[i], angle = angle[i], 
            border = border[i], col = col[i], lty = lty[i])
        P <- t2xy(mean(x[i + 0:1]))
        lab <- as.character(labels[i])
        if (!is.na(lab) && nchar(lab)) {
            lines(c(1, 1.05) * P$x, c(1, 1.05) * P$y)
#            text(1.1 * P$x, 1.1 * P$y, labels[i], xpd = TRUE, 
#                adj = ifelse(P$x < 0, 1, 0), ...)
            # Slight change to label adjustment
            text(1.1 * P$x, 1.1 * P$y, labels[i], xpd = TRUE, col=1,
                adj = c(.5*(1-P$x/radius),.5*(1-P$y/radius)), ...)
            
        }
    }
    title(main = main, ...)
    invisible(NULL)
}
id <- function(x, y, lty = 3, ...) {
  usr <- par("usr")
  n <- length(x)
  xstart <- c(rep(usr[1], n), x)
  xend <- rep(x,2)
  ystart <- rep(y,2)
  yend <- c(y, rep(usr[3], n))
  segments(xstart, ystart, xend, yend, lty = lty, ...)
}
## change the function legend so that the border for boxes in the legend
## isn't always black

legendrc <- function (x, y = NULL, legend, fill = NULL, col = par("col"), 
    lty, lwd, pch, angle = 45, density = NULL, bty = "o", bg = par("bg"), 
    box.lwd = par("lwd"), box.lty = par("lty"), pt.bg = NA, cex = 1, 
    pt.cex = cex, pt.lwd = lwd, xjust = 0, yjust = 1, x.intersp = 1, 
    y.intersp = 1, adj = c(0, 0.5), text.width = NULL, text.col = par("col"), 
    merge = do.lines && has.pch, trace = FALSE, plot = TRUE, 
    ncol = 1, horiz = FALSE, title = NULL, inset = 0) 
{
    if (missing(legend) && !missing(y) && (is.character(y) || 
        is.expression(y))) {
        legend <- y
        y <- NULL
    }
    mfill <- !missing(fill) || !missing(density)
    title <- as.graphicsAnnot(title)
    if (length(title) > 1) 
        stop("invalid title")
    legend <- as.graphicsAnnot(legend)
    n.leg <- if (is.call(legend)) 
        1
    else length(legend)
    if (n.leg == 0) 
        stop("'legend' is of length 0")
    auto <- if (is.character(x)) 
        match.arg(x, c("bottomright", "bottom", "bottomleft", 
            "left", "topleft", "top", "topright", "right", "center"))
    else NA
    if (is.na(auto)) {
        xy <- xy.coords(x, y)
        x <- xy$x
        y <- xy$y
        nx <- length(x)
        if (nx < 1 || nx > 2) 
            stop("invalid coordinate lengths")
    }
    else nx <- 0
    xlog <- par("xlog")
    ylog <- par("ylog")
    rect2 <- function(left, top, dx, dy, density = NULL, angle, 
        ...) {
        r <- left + dx
        if (xlog) {
            left <- 10^left
            r <- 10^r
        }
        b <- top - dy
        if (ylog) {
            top <- 10^top
            b <- 10^b
        }
        rect(left, top, r, b, angle = angle, density = density, 
            ...)
    }
    segments2 <- function(x1, y1, dx, dy, ...) {
        x2 <- x1 + dx
        if (xlog) {
            x1 <- 10^x1
            x2 <- 10^x2
        }
        y2 <- y1 + dy
        if (ylog) {
            y1 <- 10^y1
            y2 <- 10^y2
        }
        segments(x1, y1, x2, y2, ...)
    }
    points2 <- function(x, y, ...) {
        if (xlog) 
            x <- 10^x
        if (ylog) 
            y <- 10^y
        points(x, y, ...)
    }
    text2 <- function(x, y, ...) {
        if (xlog) 
            x <- 10^x
        if (ylog) 
            y <- 10^y
        text(x, y, ...)
    }
    if (trace) 
        catn <- function(...) do.call("cat", c(lapply(list(...), 
            formatC), list("\n")))
    cin <- par("cin")
    Cex <- cex * par("cex")
    if (is.null(text.width)) 
        text.width <- max(abs(strwidth(legend, units = "user", 
            cex = cex)))
    else if (!is.numeric(text.width) || text.width < 0) 
        stop("'text.width' must be numeric, >= 0")
    xc <- Cex * xinch(cin[1], warn.log = FALSE)
    yc <- Cex * yinch(cin[2], warn.log = FALSE)
    if (xc < 0) 
        text.width <- -text.width
    xchar <- xc
    xextra <- 0
    yextra <- yc * (y.intersp - 1)
    ymax <- yc * max(1, strheight(legend, units = "user", cex = cex)/yc)
    ychar <- yextra + ymax
    if (trace) 
        catn("  xchar=", xchar, "; (yextra,ychar)=", c(yextra, 
            ychar))
    if (mfill) {
        xbox <- xc * 0.8
        ybox <- yc * 0.5
        dx.fill <- xbox
    }
    do.lines <- (!missing(lty) && (is.character(lty) || any(lty > 
        0))) || !missing(lwd)
    n.legpercol <- if (horiz) {
        if (ncol != 1) 
            warning("horizontal specification overrides: Number of columns := ", 
                n.leg)
        ncol <- n.leg
        1
    }
    else ceiling(n.leg/ncol)
    if (has.pch <- !missing(pch) && length(pch) > 0) {
        if (is.character(pch) && !is.na(pch[1]) && nchar(pch[1], 
            type = "c") > 1) {
            if (length(pch) > 1) 
                warning("not using pch[2..] since pch[1] has multiple chars")
            np <- nchar(pch[1], type = "c")
            pch <- substr(rep.int(pch[1], np), 1:np, 1:np)
        }
        if (!merge) 
            dx.pch <- x.intersp/2 * xchar
    }
    x.off <- if (merge) 
        -0.7
    else 0
    if (is.na(auto)) {
        if (xlog) 
            x <- log10(x)
        if (ylog) 
            y <- log10(y)
    }
    if (nx == 2) {
        x <- sort(x)
        y <- sort(y)
        left <- x[1]
        top <- y[2]
        w <- diff(x)
        h <- diff(y)
        w0 <- w/ncol
        x <- mean(x)
        y <- mean(y)
        if (missing(xjust)) 
            xjust <- 0.5
        if (missing(yjust)) 
            yjust <- 0.5
    }
    else {
        h <- (n.legpercol + (!is.null(title))) * ychar + yc
        w0 <- text.width + (x.intersp + 1) * xchar
        if (mfill) 
            w0 <- w0 + dx.fill
        if (has.pch && !merge) 
            w0 <- w0 + dx.pch
        if (do.lines) 
            w0 <- w0 + (2 + x.off) * xchar
        w <- ncol * w0 + 0.5 * xchar
        if (!is.null(title) && (tw <- strwidth(title, units = "user", 
            cex = cex) + 0.5 * xchar) > w) {
            xextra <- (tw - w)/2
            w <- tw
        }
        if (is.na(auto)) {
            left <- x - xjust * w
            top <- y + (1 - yjust) * h
        }
        else {
            usr <- par("usr")
            inset <- rep(inset, length.out = 2)
            insetx <- inset[1] * (usr[2] - usr[1])
            left <- switch(auto, bottomright = , topright = , 
                right = usr[2] - w - insetx, bottomleft = , left = , 
                topleft = usr[1] + insetx, bottom = , top = , 
                center = (usr[1] + usr[2] - w)/2)
            insety <- inset[2] * (usr[4] - usr[3])
            top <- switch(auto, bottomright = , bottom = , bottomleft = usr[3] + 
                h + insety, topleft = , top = , topright = usr[4] - 
                insety, left = , right = , center = (usr[3] + 
                usr[4] + h)/2)
        }
    }
    if (plot && bty != "n") {
        if (trace) 
            catn("  rect2(", left, ",", top, ", w=", w, ", h=", 
                h, ", ...)", sep = "")
        rect2(left, top, dx = w, dy = h, col = bg, density = NULL, 
            lwd = box.lwd, lty = box.lty)
    }
    xt <- left + xchar + xextra + (w0 * rep.int(0:(ncol - 1), 
        rep.int(n.legpercol, ncol)))[1:n.leg]
    yt <- top - 0.5 * yextra - ymax - (rep.int(1:n.legpercol, 
        ncol)[1:n.leg] - 1 + (!is.null(title))) * ychar
    if (mfill) {
        if (plot) {
            fill <- rep(fill, length.out = n.leg)
            rect2(left = xt, top = yt + ybox/2, dx = xbox, dy = ybox, 
                col = fill, density = density, angle = angle, 
#                border = "black")
#               Change to match the internal colour of the box
                  border=fill)
        }
        xt <- xt + dx.fill
    }
    if (plot && (has.pch || do.lines)) 
        col <- rep(col, length.out = n.leg)
    if (missing(lwd)) 
        lwd <- par("lwd")
    if (do.lines) {
        seg.len <- 2
        if (missing(lty)) 
            lty <- 1
        lty <- rep(lty, length.out = n.leg)
        lwd <- rep(lwd, length.out = n.leg)
        ok.l <- !is.na(lty) & (is.character(lty) | lty > 0)
        if (trace) 
            catn("  segments2(", xt[ok.l] + x.off * xchar, ",", 
                yt[ok.l], ", dx=", seg.len * xchar, ", dy=0, ...)")
        if (plot) 
            segments2(xt[ok.l] + x.off * xchar, yt[ok.l], dx = seg.len * 
                xchar, dy = 0, lty = lty[ok.l], lwd = lwd[ok.l], 
                col = col[ok.l])
        xt <- xt + (seg.len + x.off) * xchar
    }
    if (has.pch) {
        pch <- rep(pch, length.out = n.leg)
        pt.bg <- rep(pt.bg, length.out = n.leg)
        pt.cex <- rep(pt.cex, length.out = n.leg)
        pt.lwd <- rep(pt.lwd, length.out = n.leg)
        ok <- !is.na(pch) & (is.character(pch) | pch >= 0)
        x1 <- (if (merge) 
            xt - (seg.len/2) * xchar
        else xt)[ok]
        y1 <- yt[ok]
        if (trace) 
            catn("  points2(", x1, ",", y1, ", pch=", pch[ok], 
                ", ...)")
        if (plot) 
            points2(x1, y1, pch = pch[ok], col = col[ok], cex = pt.cex[ok], 
                bg = pt.bg[ok], lwd = pt.lwd[ok])
        if (!merge) 
            xt <- xt + dx.pch
    }
    xt <- xt + x.intersp * xchar
    if (plot) {
        if (!is.null(title)) 
            text2(left + w/2, top - ymax, labels = title, adj = c(0.5, 
                0), cex = cex, col = text.col)
        text2(xt, yt, labels = legend, adj = adj, cex = cex, 
            col = text.col)
    }
    invisible(list(rect = list(w = w, h = h, left = left, top = top), 
        text = list(x = xt, y = yt)))
}
mxyline <- function(m, x, y, ...){
  abline(y-m*x, m, ...)
}
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


