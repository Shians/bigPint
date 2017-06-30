library(plotly)
library(shiny)
library(ggplot2)
library(hexbin)

set.seed(1)
dat <- data.frame(ID = paste0("ID", 1:1110), x = c(rep(0.5, 1000), abs(rnorm(10)), rep(2, 100)), y = c(rep(0.5, 1000), abs(rnorm(10)), rep(2,100)), stringsAsFactors = FALSE)
minVal = min(dat[,-1])
maxVal = max(dat[,-1])
maxRange = c(minVal, maxVal)
xbins= 10
buffer = (maxRange[2]-maxRange[1])/(xbins/2)
x <- dat$x
y <- dat$y
h <- hexbin(x=x, y=y, xbins=xbins, shape=1, IDs=TRUE, xbnds=maxRange, ybnds=maxRange)
hexdf <- data.frame (hcell2xy (h),  hexID = h@cell, counts = h@count)
attr(hexdf, "cID") <- h@cID
p <- ggplot(hexdf, aes(x=x, y=y, fill = counts, hexID=hexID)) + geom_hex(stat="identity") + geom_abline(intercept = 0, color = "red", size = 0.25) + coord_cartesian(xlim = c(maxRange[1]-1*buffer, maxRange[2]+buffer), ylim = c(maxRange[1]-1*buffer, maxRange[2]+buffer)) + coord_equal(ratio = 1) + labs(x = "X", y = "Y")
p


my_breaks = c(2, 4, 6, 8, 20, 1000)
p + scale_fill_gradient(name = "count", trans = "log", breaks = my_breaks, labels = my_breaks)

png('/Users/lindz/bigPint/Legend.png', width = 600, height = 600)
p
dev.off()

ggPS <- ggplotly(p)
ggPS




# https://stackoverflow.com/questions/8069837/is-there-a-built-in-way-to-do-a-logarithmic-color-scale-in-ggplot2
require(ggplot2)
n <- 1e5
df <- data.frame(x = rexp(n), y = rexp(n))
p <- ggplot(df, aes(x = x, y = y)) + stat_binhex()

my_breaks = c(2, 10, 50, 100, 200, 6000)
p <- p + scale_fill_gradient(name = "count", trans = "log", breaks = my_breaks, labels = my_breaks, guide="legend")



