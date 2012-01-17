# Sundar Dorai-Raj on R-help 14 Aug 2007
# http://tolstoy.newcastle.edu.au/R/e2/help/07/08/23395.html
panel.sunflowerplot <- function(x, y, number, log = "", digits = 6, rotate = FALSE,
                                cex.fact = 1.5, size = 1/8, seg.col = 2, seg.lwd = 1.5, ...) {
  n <- length(x) 
  if(missing(number)) {
    x <- signif(x, digits = digits)
    y <- signif(y, digits = digits)
    orderxy <- order(x, y)
    x <- x[orderxy]
    y <- y[orderxy]
    first <- c(TRUE, (x[-1] != x[-n]) | (y[-1] != y[-n]))
    x <- x[first]
    y <- y[first]
    number <- diff(c((1:n)[first], n + 1))
  } else {
    if(length(number) != n)
      stop("'number' must have same length as 'x' and 'y'")
    np <- number > 0
    x <- x[np]
    y <- y[np]
    number <- number[np]
  } 
  n <- length(x) 
  n.is1 <- number == 1 
  cex <- trellis.par.get("plot.symbol")$cex
  if(any(n.is1)) 
    lpoints(x[n.is1], y[n.is1], cex = cex, ...)
  if(any(!n.is1)) {
    lpoints(x[!n.is1], y[!n.is1], cex = cex/cex.fact, ...)
    i.multi <- (1:n)[number > 1]
    ppin <- par("pin")
    pusr <- unlist(current.panel.limits())
    xr <- size * abs(pusr[2] - pusr[1])/ppin[1]
    yr <- size * abs(pusr[4] - pusr[3])/ppin[2]
    i.rep <- rep.int(i.multi, number[number > 1])
    z <- numeric()
    for (i in i.multi) z <- c(z, 1:number[i] + if (rotate) 
                              stats::runif(1) else 0)
    deg <- (2 * pi * z)/number[i.rep]
    lsegments(x[i.rep], y[i.rep], x[i.rep] + xr * sin(deg),
              y[i.rep] + yr * cos(deg), col = seg.col, lwd = seg.lwd)
  } 
}
