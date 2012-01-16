#! /usr/bin/env Rscript

# Time-stamp: <2012-01-16 11:41:17 chl>

#
# R script to generate figures from vizRguide.tex.
#

library(lattice)
library(latticeExtra)
set.seed(101)

trellis.device(device="cairo_pdf", theme=custom.theme.2())
cairo_pdf("figs_lattice.pdf", onefile=TRUE, family="Myriad Sans",
          height=5, width=5)

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

p <- xyplot(dist ~ speed, data=cars, type=c("p","smooth"))
print(p)

p <- xyplot(dist ~ speed, data=cars, type=c("p","smooth"), span=1/3)
print(p)

p <- xyplot(dist ~ speed, data=cars, type=c("p","r"))
print(p)

dev.off()
