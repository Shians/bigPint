library(plotly)
library(shiny)
library(ggplot2)
library(hexbin)

rm(list=ls())

set.seed(1)
dat <- data.frame(ID = paste0("ID", 1:1010), A.1 = c(rep(0.5, 1000), abs(rnorm(10))), A.2 = c(rep(0.5, 1000), abs(rnorm(10))), B.1 = c(rep(0.5, 1000), abs(rnorm(10))), B.2 = c(rep(0.5, 1000), abs(rnorm(10))), C.1 = c(rep(0.5, 1000), abs(rnorm(10))), C.2 = c(rep(0.5, 1000), abs(rnorm(10))), C.3 = c(rep(0.5, 1000), abs(rnorm(10))), stringsAsFactors = FALSE
)

sampleIndex <- which(sapply(colnames(dat), function(x) unlist(strsplit(x,"[.]"))[1]) %in% c("A", "C"))
datSel <- dat[,c(1, sampleIndex)]

sampleIndex1 <- which(sapply(colnames(datSel), function(x) unlist(strsplit(x,"[.]"))[1]) %in% c("A"))
sampleIndex2 <- which(sapply(colnames(datSel), function(x) unlist(strsplit(x,"[.]"))[1]) %in% c("C"))
minVal = min(datSel[,-1])
maxVal = max(datSel[,-1])
maxRange = c(minVal, maxVal)
xbins= 10
buffer = (maxRange[2]-maxRange[1])/(xbins/2)
x <- c()
y <- c()
for (i in 1:length(sampleIndex1)){
  for (j in 1:length(sampleIndex2)){
    x <- c(x, unlist(datSel[,(sampleIndex1[i])]))
    y <- c(y, unlist(datSel[,(sampleIndex2[j])]))
  }
}

h <- hexbin(x=x, y=y, xbins=xbins, shape=1, IDs=TRUE, xbnds=maxRange, ybnds=maxRange)
hexdf <- data.frame (hcell2xy (h),  hexID = h@cell, counts = h@count)
attr(hexdf, "cID") <- h@cID

my_breaks = c(2, 4, 6, 8, 20, 1000)
p <- ggplot(hexdf, aes(x=x, y=y, fill = counts, hexID=hexID)) + geom_hex(stat="identity") + geom_abline(intercept = 0, color = "red", size = 0.25) + labs(x = "A", y = "C") + coord_fixed(xlim = c(-0.5, (maxRange[2]+buffer)), ylim = c(-0.5, (maxRange[2]+buffer))) + theme(aspect.ratio=1) + scale_fill_gradient(name = "count", trans = "log", breaks = my_breaks, labels = my_breaks, guide="legend")
p

ggplotly(p)
ggplotly(p) %>% layout(height = 200, width = 200)
ggplotly(p, height=400, width=400)




###### Creating legend for hex bins

# https://stackoverflow.com/questions/8069837/is-there-a-built-in-way-to-do-a-logarithmic-color-scale-in-ggplot2
require(ggplot2)
n <- 1e5
df <- data.frame(x = rexp(n), y = rexp(n))
p <- ggplot(df, aes(x = x, y = y)) + stat_binhex()

my_breaks = c(2, 10, 50, 100, 200, 6000)
p <- p + scale_fill_gradient(name = "count", trans = "log", breaks = my_breaks, labels = my_breaks, guide="legend")

ggPS <- ggplotly(p)
ggPS
