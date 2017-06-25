# Points that are DEG but not outside PI are plotted blue, points that are DEG and outside PI are plotted red, and points that are not DEG but are outside PI are plotted grey.

library(GGally)
library(tidyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(DESeq2)

# Look at VS and NP normalized/filtered to entire dataset
predLevel = 0.999
size = 0.1
outDir = "/Users/lindz/bigPint/tblshoot/PISM"

my_fn <- function(data, mapping, ...){
  xChar = as.character(mapping$x)
  yChar = as.character(mapping$y)
  x = data[,c(xChar)]
  y = data[,c(yChar)]
  m <- lm(y ~ x, data = data)
  mpi <- cbind(data, predict(m, interval = "prediction", level=predLevel))
  # Keep only points that are outside the prediction interval
  plotPoints <- mpi[which(!(mpi[yChar] > mpi$lwr & mpi[yChar] < mpi$upr)),]
  pred_interval <- predict(m, newdata=data.frame(x=newx), interval="prediction", level = predLevel)
  pred_interval <- as.data.frame(pred_interval)
  pred_interval[xChar] = newx
  
  indexBoth = rownames(plotPoints) %in% rownames(degData)
  indexBlue = rownames(degData) %in% rownames(plotPoints)
  redPoints = plotPoints[indexBoth,]
  greyPoints = plotPoints[!indexBoth,] # problem if indexBoth is integer(0)
  bluePoints = degData[!indexBlue,]
  
  p <- ggplot(data = redPoints, aes_string(x = xChar)) + geom_point(aes_string(y = yChar), size=2, color = "red") + geom_point(data = bluePoints, aes_string(y = yChar), size=1, color = "blue") + geom_point(data = greyPoints, aes_string(y = yChar), size=size, color = "darkgrey") + geom_ribbon(data= pred_interval, aes(ymin = lwr, ymax = upr), fill = "cornflowerblue", alpha = 0.2) + coord_cartesian(xlim = c(minX, maxX), ylim = c(minX, maxX))
  p
}

dds <- readRDS("/Users/lindz/bigPint/tblshoot/data_limma.Rds")
rld <- readRDS("/Users/lindz/bigPint/tblshoot/topGenes_limma.Rds")

# Change group1 and group2 as needed
group1 ="NS"
group2 ="VP"

sampleIndex <- which(sapply(colnames(dds[[1]]), function(x) unlist(strsplit(x,"[.]"))[1]) %in% c(group1, group2))

# Create prediction interval data frame with upper and lower lines corresponding to sequence covering minimum and maximum of x values in original dataset (don't consider last column because it is not on the x axis in any of the individual scatterplots)
lDat = as.data.frame(dds[[1]])
lDat = log2(lDat+1)

minX <- min(lDat[,c(2:(ncol(lDat)-1))])
maxX <- max(lDat[,c(2:(ncol(lDat)-1))])
newx <- seq(minX - (maxX-minX), maxX + (maxX-minX), by=0.05)

sampleIndex <- which(sapply(colnames(dds[[1]]), function(x) unlist(strsplit(x,"[.]"))[1]) %in% c(group1, group2))

bindataSel <- as.data.frame(dds[[1]])[, sampleIndex]
setDT(bindataSel, keep.rownames = TRUE)[]
colnames(bindataSel)[1] <- "ID"
bindataSel$ID <- as.character(bindataSel$ID)
bindataSel <- as.data.frame(bindataSel)

degRows <- rownames(rld[[paste0(group1,"vs",group2)]])
degData <- bindataSel[which(bindataSel$ID %in% degRows),]

# Take logs
bindataSel[,2:ncol(bindataSel)] <- log(bindataSel[,2:ncol(bindataSel)]+1)
degData[,2:ncol(degData)] <- log(degData[,2:ncol(degData)]+1)

maxVal = max(bindataSel[,-1])
minVal = min(bindataSel[,-1])
maxRange = c(minVal, maxVal)

p <- ggpairs(bindataSel[,-1], lower = list(continuous = my_fn))
jpeg(filename=paste0(outDir, "/", group1, "_", group2, "_level999.jpg"), height=1400, width=1400)
print(p)
dev.off()

