library(hexbin)
library(tidyr)
library(ggplot2)

outDir = "/Users/lindz/bigPint/tblshoot/Volcano"

group1 <- "NP"
group2 <- "NS"

dds <- readRDS("/Users/lindz/bigPint/tblshoot/data_limma.rds")
rld <- readRDS("/Users/lindz/bigPint/tblshoot/genePval_limma.Rds")
dat <- readRDS("/Users/lindz/bigPint/tblshoot/beeVolcanoData_limma.rds")

myLevels <- unique(sapply(colnames(dds[[1]]), function(x) unlist(strsplit(x,"[.]"))[1]))
myPairs <- list()

# Runs exact test on all pairs of groups and saves in list
k=1
for (i in 1:(length(myLevels)-1)){
  for (j in (i+1):(length(myLevels))){
    myPairs[[k]] <- paste0(myLevels[i], " and ", myLevels[j])
    k=k+1
  }
}

# log
dat[, 2:(ncol(dat)-2*length(myPairs))] <- log(dat[, 2:(ncol(dat)-2*length(myPairs))]+1)

nCol = ncol(dat)
datFCP = dat[,(nCol-2*length(myPairs)+1):nCol]

# x-axis FC, y-axis pval
xMax = max(datFCP[,seq(1,ncol(datFCP),by=2)], na.rm=TRUE)
xMin = min(datFCP[,seq(1,ncol(datFCP),by=2)], na.rm=TRUE)
yMax = max(datFCP[,seq(2,ncol(datFCP),by=2)], na.rm=TRUE)
yMin = min(datFCP[,seq(2,ncol(datFCP),by=2)], na.rm=TRUE)
fcMax = ceiling(max(exp(xMax), 1/exp(xMin)))

pairNum <- as.numeric(which(myPairs==paste0(group1, " and ", group2)))
col1 <- colnames(dat)[nCol-2*length(myPairs)+2*pairNum]
col2 <- colnames(dat)[nCol-2*length(myPairs)+2*pairNum-1]

degIndex <- rownames(rld[[paste0(group1,"vs",group2)]][which(rld[[paste0(group1,"vs",group2)]]$adj.P.Val<0.05),])
degData <- dat[which(dat$ID %in% degIndex),]

x = dat[[col2]]
y = dat[[col1]]
x1 = x[which(!is.na(x)&!is.na(y))]
y1 = y[which(!is.na(x)&!is.na(y))]
x2 = degData[[col2]]
y2 = degData[[col1]]
h <- hexbin(x=x, y=y, xbins=10, shape=3, IDs=TRUE, xbnds=c(xMin, xMax), ybnds=c(yMin, yMax))
hexdf <- data.frame (hcell2xy (h),  hexID = h@cell, counts = h@count)


pcpDat <- degData[, 1:(ncol(dat)-2*length(myPairs))] #log2+1

colNms <- colnames(dat[, 2:(ncol(dat)-2*length(myPairs))])
nVar <- length(2:(ncol(dat)-2*length(myPairs)))
boxDat <- dat[, 1:(ncol(dat)-2*length(myPairs))] %>% gather(key, val, -c(ID))
colnames(boxDat)[2:3] <- c("Sample","Count")

pcpDat2 <- pcpDat %>% gather(key, val, -c(ID))
colnames(pcpDat2) <- c("ID", "Sample", "Count")

jpeg(filename=paste0(outDir, "/", group1, "_", group2, ".jpg"), height=700, width=1100)
require(gridExtra)
plot1 <- ggplot(hexdf, aes(x=x, y=y, fill = counts, hexID=hexID)) + geom_hex(stat="identity") + coord_cartesian(xlim = c(xMin, xMax), ylim = c(yMin, yMax)) + geom_point(data=degData, aes(x=x2, y=y2), color = "red", size = 0.5, inherit.aes = FALSE) + xlab("log2(Fold change)") + ylab("-log10(p-value)")
plot2 <- ggplot(boxDat, aes(x = Sample, y = Count)) + geom_boxplot() + geom_line(data=pcpDat2, aes(x = Sample, y = Count, group = ID), size = 0.3, color = "red") + theme(axis.text.x=element_text(angle=90, hjust=1))
grid.arrange(plot1, plot2, ncol=1)
dev.off()
