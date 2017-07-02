rm(list=ls())

set.seed(1)
dat <- data.frame(ID = paste0("ID", 1:100), A.1 = c(1, abs(rnorm(99))), A.2 = c(1, abs(rnorm(99))), B.1 = c(1, abs(rnorm(99))), B.2 = c(1, abs(rnorm(99))), C.1 = c(1, abs(rnorm(99))), C.2 = c(1, abs(rnorm(99))), C.3 = c(1, abs(rnorm(99))), stringsAsFactors = FALSE
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

p <- ggplot(hexdf, aes(x=x, y=y, fill = counts, hexID=hexID)) + geom_hex(stat="identity") + geom_abline(intercept = 0, color = "red", size = 0.25) + labs(x = "A", y = "C") + coord_fixed(xlim = c(-0.5, (maxRange[2]+buffer)), ylim = c(-0.5, (maxRange[2]+buffer))) + theme(aspect.ratio=1)
ggplotly(p) %>% layout(height = 200, width = 200)
# Warning messages:
#1: Aspect ratios aren't yet implemented, but you can manually set a suitable height/width 
#2: Aspect ratios aren't yet implemented, but you can manually set a suitable height/width 
#3: Specifying width/height in layout() is now deprecated.
#Please specify in ggplotly() or plot_ly() 
