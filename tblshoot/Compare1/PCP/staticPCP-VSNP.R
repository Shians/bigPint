library(tidyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(DESeq2)

outDir = "/Users/lindz/bigPint/tblshoot/PCP"

dds <- readRDS("/Users/lindz/bigPint/tblshoot/data_limma.Rds")
rld <- readRDS("/Users/lindz/bigPint/tblshoot/topGenes_limma.Rds")

# Change group1 and group2 as needed
group1 ="NS"
group2 ="VP"

sampleIndex <- which(sapply(colnames(dds[[1]]), function(x) unlist(strsplit(x,"[.]"))[1]) %in% c(group1, group2))

bindataSel <- as.data.frame(dds[[1]])[, sampleIndex]
setDT(bindataSel, keep.rownames = TRUE)[]
colnames(bindataSel)[1] <- "ID"
bindataSel$ID <- as.character(bindataSel$ID)
bindataSel <- as.data.frame(bindataSel)
degRows <- rownames(rld[[paste0(group1,"vs",group2)]])
pcpDat <- bindataSel[which(bindataSel$ID %in% degRows),]

# Take logs
bindataSel[,2:ncol(bindataSel)] <- log(bindataSel[,2:ncol(bindataSel)]+1)
pcpDat[,2:ncol(pcpDat)] <- log(pcpDat[,2:ncol(pcpDat)]+1)

nVar = ncol(bindataSel)
colNms <- colnames(bindataSel[, c(2:nVar)])
    
boxDat <- bindataSel[, c(1:nVar)] %>% gather(key, val, -c(ID))
colnames(boxDat) <- c("ID", "Sample", "Count")
pcpDat2 <- pcpDat[, c(1:nVar)] %>% gather(key, val, -c(ID))
colnames(pcpDat2) <- c("ID", "Sample", "Count")

p <- ggplot(boxDat, aes(x = Sample, y = Count)) + geom_boxplot() + geom_line(data=pcpDat2, aes(x = Sample, y = Count, group = ID), size = 0.3, alpha = 0.5, color = "red")

jpeg(filename=paste0(outDir, "/", group1, "_", group2, ".jpg"), height=1400, width=1400)
print(p)
dev.off()
