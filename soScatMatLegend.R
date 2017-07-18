library(plotly)
library(ggplot2)
library(hexbin)
library(RColorBrewer)

set.seed(1)
xbins= 20

x=abs(rnorm(10000))
y=abs(rnorm(10000))
minVal = min(x,y)
maxVal = max(x,y)
maxRange = c(minVal, maxVal)
buffer = (maxRange[2]-maxRange[1])/(xbins/2)
h <- hexbin(x=x, y=y, xbins=xbins, shape=1, IDs=TRUE, xbnds=maxRange, ybnds=maxRange)
hexdf <- data.frame (hcell2xy (h),  hexID = h@cell, counts = h@count)
my_breaks = c(2, 4, 6, 8, 20, 1000)
clrs <- brewer.pal(length(my_breaks)+3, "Blues")
clrs <- clrs[3:length(clrs)]
hexdf$countColor <- cut(hexdf$counts, breaks=c(0, my_breaks, Inf), labels=rev(clrs))
ggplot(hexdf, aes(x=x, y=y, hexID=hexID, fill=countColor)) + scale_fill_manual(values=levels(hexdf$countColor)) + geom_hex(stat="identity") + geom_abline(intercept = 0, color = "red", size = 0.25) + coord_fixed(xlim = c(-0.5, (maxRange[2]+buffer)), ylim = c(-0.5, (maxRange[2]+buffer))) + theme(aspect.ratio=1)
