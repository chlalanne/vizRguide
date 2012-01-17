#! /usr/bin/env Rscript

# Time-stamp: <2012-01-17 17:12:38 chl>

#
# R script to generate figures from vizRguide.tex.
#

library(lattice)
library(latticeExtra)
source("panels.r")
set.seed(101)

trellis.device(device="cairo_pdf", theme=custom.theme.2())
cairo_pdf("figs_lattice.pdf", onefile=TRUE, family="Myriad Sans",
          height=5, width=5)


x <- sample(1:30, 25, replace=TRUE)
p <- stripplot(~ x, jitter.data=TRUE, factor=.8, aspect=.5)
print(p)

p <- stripplot(~ x, jitter.data=TRUE, factor=.8, aspect="xy")
print(p)

p <- stripplot(x ~ 1, horizontal=FALSE, jitter.data=TRUE, aspect=1.2,
               scales=list(x=list(draw=FALSE)), xlab="", pch=15, alpha=.5)
print(p)

p <- stripplot(~ x, panel=HH::panel.dotplot.tb, cex=1.2, factor=.2)
print(p)

x <- sample(seq(1, 60, by=2), 75, replace=TRUE)
p <- stripplot(1 ~ x, panel=panel.sunflowerplot, col="black", 
               seg.col="black", seg.lwd=1, size=.08,
               scales=list(y=list(draw=FALSE)), ylab="")
print(p)

p <- histogram(~ waiting, data=faithful)
print(p)

p <- histogram(~ waiting, data=faithful, type="count")
print(p)

p <- histogram(~ waiting, data=faithful, border=NA, nint=12)
print(p)

x <- rnorm(80, mean=12, sd=2)
p <- histogram(~ x, type="density", border=NA,
               panel=function(x, ...) {
                 panel.histogram(x, ...)
                 panel.mathdensity(dmath=dnorm, col="#BF3030",
                                   args=list(mean=mean(x),sd=sd(x)))
               })
print(p)

p <- densityplot(~ waiting, data=faithful)
print(p)

p <- densityplot(~ waiting, data=faithful, plot.points="rug")
print(p)

p <- densityplot(~ waiting, data=faithful, plot.points=FALSE, ref=TRUE)
print(p)

p <- bwplot(~ waiting, data=faithful, panel=panel.violin)
print(p)

p <- bwplot(~ waiting, data=faithful, 
            panel=function(..., box.ratio) {
              panel.violin(..., col="transparent", varwidth=FALSE, 
                           box.ratio=box.ratio)
              panel.bwplot(..., fill=NULL, box.ratio=.15)
            })
print(p)

p <- qqmath(~ rnorm(60))
print(p)

p <- bwplot(~ height, data=women)
print(p)

p <- bwplot(~ height, data=women, box.ratio=.5)
print(p)

p <- bwplot(~ height, data=women, box.ratio=.5,
            scales=list(x=list(limits=c(50, 80), at=seq(50, 80, by=2))))
print(p)

p <- bwplot(~ height, data=women, aspect=.5, 
            panel=function(x, ...) {
              panel.bwplot(x, pch="|", ...)
              panel.points(mean(x), 1, pch=19, cex=1)
            })
print(p)

p <- xyplot(sunspot.year)
print(p)

p <- xyplot(sunspot.year, aspect=.3, scales=list(y=list(rot=0)))
print(p)

p <- xyplot(sunspot.year, strip=FALSE, cut=list(number=2, overlap=.05))
print(p)

p <- xyplot(sunspot.year, strip=FALSE, strip.left=TRUE, 
            cut=list(number=2, overlap=.05))
print(p)

library(zoo)
p <- xyplot(zoo(discoveries))
print(p)

p <- xyplot(zoo(discoveries), panel=panel.xyarea)
print(p)

xt <- ts(cumsum(rnorm(200 * 12)))
p <- xyplot(xt)
print(p)

xt <- zoo(accdeaths)
p <- xyplot(xt, type=c("l","g"), ylab="Total accidental deaths")
print(p)

p <- xyplot(xt,
            panel=function(x, y, ...) {
              panel.xyplot(x, y, ...)
              panel.lines(rollmean(zoo(y, x), 3), lwd=2, col=1)
            })
print(p)

p <- xyplot(xt) +
  layer(panel.tskernel(x, y, c=3, col=1, lwd=2))
print(p)

p <- xyplot(ts.union(sunspot.year, lag10=lag(sunspot.year, 10)), 
            superpose=TRUE, panel=panel.superpose,
            panel.groups=function(..., group.number) {
              if (group.number == 1) panel.xyarea(...)
              else panel.xyplot(...)
            }, border=NA, cut = list(n=3, overlap=0), aspect="xy",
            par.settings=simpleTheme(col=c("grey","black"), lwd=c(5,2)))
print(p)

flow <- ts(filter(rlnorm(200, mean = 1), 0.8, method = "r"))
p <- xyplot(flow, 
            panel=function(x, y, ...) {
              panel.xblocks(x, y > mean(y), col="lightgray")
              panel.xyplot(x, y, ...)
            })
print(p)

## xt <- ts(cumsum(rnorm(12)), frequency=12, start=c(2010, 1))
## xd <- as.Date("2010-01-01") + as.numeric(time(xt))
## xyplot(xt ~ xd, type="l") + layer_(panel.xblocks(x, months))
       
p <- xyplot(uptake ~ conc, data=CO2, groups=Treatment, type="a")
print(p)

p <- xyplot(dist ~ speed, data=cars)
print(p)

p <- xyplot(dist ~ speed, data=cars, pch=rbinom(nrow(cars), 1, .5)+1)
print(p)

p <- xyplot(dist ~ speed, data=cars, 
            col=with(cars, cut(speed, breaks=quantile(speed), 
              include.lowest=TRUE)))
print(p)

p <- xyplot(dist ~ speed, data=cars, pch=19,
            groups=with(cars, cut(speed, breaks=quantile(speed), 
              include.lowest=TRUE)))
print(p)

xy <- as.data.frame(replicate(2, rnorm(1000)))
p <- xyplot(V1 ~ V2, data=xy, pch=19, alpha=.5)
print(p)

dat <- data.frame(replicate(2, rnorm(500)), z=sample(0:40, 500, T))
library(RColorBrewer)
cols <- colorRampPalette(brewer.pal(11, "RdBu"))(diff(range(dat$z)))
p <- xyplot(X1 ~ X2, data=dat, col=cols[dat$z], pch=19, alpha=.5)
print(p)

p <- xyplot(Sepal.Length ~ Petal.Length, data=iris, jitter.x=TRUE, amount=.2)
print(p)

p <- xyplot(dist ~ speed, data=cars, col.line="grey75",
            panel=function(x, y, ...) {
              panel.xyplot(x, y, ...)
              panel.rug(x, y, ...)
            })
print(p)

p <- xyplot(log(Volume) ~ log(Girth), data=trees)
print(p)

p <- xyplot((1:200)/20 ~ (1:200)/20, type=c("p", "g"),
        scales=list(x=list(log=10), y=list(log=10)),
        xscale.components = xscale.components.log10.3,
        yscale.components = yscale.components.log10.3)
print(p)

p <- xyplot(dist ~ speed, data=cars, scales=list(tck=c(1,0)))
print(p)

p <- xyplot(dist ~ speed, data=cars, scales=list(alternating=3))
print(p)

spray.df <- aggregate(count ~ spray, data=InsectSprays, FUN=mean)
p <- barchart(count ~ spray, data=spray.df)
print(p)

p <- barchart(spray ~ count, data=spray.df)
print(p)

p <- xyplot(dist ~ speed, data=cars, type=c("p","smooth"))
print(p)

p <- xyplot(dist ~ speed, data=cars, type=c("p","smooth"), span=1/3)
print(p)

p <- xyplot(dist ~ speed, data=cars, type=c("p","r"))
print(p)

xt <- ts(matrix(cumsum(rnorm(200 * 12)), ncol=2))
p <- xyplot(xt)
print(p)

p <- xyplot(xt, scales=list(y="same"), type=c("l","g"))
print(p)

p <- xyplot(xt, layout=c(2,1))
print(p)

p <- xyplot(EuStockMarkets, scales=list(y="same"))
print(p)

p <- xyplot(EuStockMarkets, superpose=TRUE, auto.key=list(columns=2))
print(p)

p <- horizonplot(EuStockMarkets, colorkey=TRUE)
print(p)

infolayers <-
  layer(panel.scaleArrow(x = 0.99, digits = 1, col = "grey",
                         srt = 90, cex = 0.7)) +
  layer(lim <- current.panel.limits(),
        panel.text(lim$x[1], lim$y[1], round(lim$y[1],1), font = 2,
                   cex = 0.7, adj = c(-0.5,-0.5), col = "#9FC8DC"))

p <- horizonplot(EuStockMarkets, colorkey=TRUE) + infolayers
print(p)

dev.off()
