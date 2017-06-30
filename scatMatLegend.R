library(plotly)
library(shiny)
library(ggplot2)

set.seed(1)
dat <- data.frame(ID = paste0("ID", 1:1010), x = c(rep(0.5, 1000), abs(rnorm(10))), y = c(rep(0.5, 1000), abs(rnorm(10))), stringsAsFactors = FALSE)
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
p <- ggplot(hexdf, aes(x=x, y=y, fill = counts, hexID=hexID)) +
  geom_hex(stat="identity") + geom_abline(intercept = 0, color = "red", size = 0.25) +
  coord_cartesian(xlim = c(maxRange[1]-1*buffer, maxRange[2]+buffer),
                  ylim = c(maxRange[1]-1*buffer, maxRange[2]+buffer)) +
  coord_equal(ratio = 1) +
  labs(x = "X", y = "Y")
p


ggPS <- ggplotly(p)
