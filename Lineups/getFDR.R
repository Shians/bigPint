library(limma)
library(Glimma)
library(GGally)
library(ggplot2)

#Input countTable must be of the following form:
# str(countTable)
#'data.frame':	15314 obs. of  12 variables:
#$ NP.1: int  0 0 0 0 2 4 103 930 331 1016 ...
#$ NP.2: int  0 0 0 0 0 12 90 492 3020 3610 ...
#$ NP.3: int  0 0 0 0 5 17 115 692 265 1179 ...
#$ NS.1: int  0 0 0 0 2 27 158 2042 1608 3653 ...
#$ NS.2: int  0 0 0 0 2 21 327 2119 801 3518 ...
#$ NS.3: int  0 0 0 0 7 33 617 4503 719 7477 ...

countTable <- readRDS("/Users/lindz/bigPint/BeeCountOrig.Rds")
group1 = "NP"
group2 = "NS"
cols = which(as.factor(unlist(lapply(colnames(countTable), function (x) unlist(strsplit(x, "[.]"))[1]))) %in% c(group1, group2))
countTable <- countTable[, cols]

getFDR <- function(countTable){
  x <- DGEList(counts=countTable)
  group <- as.factor(unlist(lapply(colnames(countTable), function (x) unlist(strsplit(x, "[.]"))[1])))
  x$samples$group <- group
  #lane <- as.factor(c("L12","L12","L12","L12","L34","L34","L12","L12","L12","L12","L34","L34","L12","L12","L34","L34","L34","L34","L12","L12","L34","L34","L34","L34","L12","L12","L12","L12","L34","L34","L12","L12","L12","L12","L34","L34","L12","L12","L34","L34","L34","L34","L12","L12","L34","L34","L34","L34"))
  #x$samples$lane <- lane
  
  cpm <- cpm(x)
  lcpm <- cpm(x, log=TRUE)
  keep.exprs <- rowSums(cpm>1)>=8
  x <- x[keep.exprs,, keep.lib.sizes=FALSE]
  x <- calcNormFactors(x, method = "TMM")
  
  design <- model.matrix(~0+group) #+lane
  colnames(design) <- gsub("group", "", colnames(design))
  contr.matrix <- makeContrasts(eval(paste0(group1,"-",group2)), levels = colnames(design))
  
  v <- voom(x, design)
  vfit <- lmFit(v, design)
  vfit <- contrasts.fit(vfit, contrasts=contr.matrix)
  efit <- eBayes(vfit)
  
  pairNames <- paste0(group1,"-",group2)
  topGenes <- list()
  genePval <- list()
  temp <- topTreat(efit, coef=1, n=Inf)
  temp2 <- temp[order(temp[,5]),]
  sigRows <- 1:100 #Keep top 100 lowest FDR
  temp3 <- temp2[sigRows,]
  setDT(temp3, keep.rownames = TRUE)[]
  colnames(temp3)[1] = "ID"
  colnames(temp3)[5] = "pVal" # can't have dots in name
  colnames(temp3)[6] = "adjPVal" # can't have dots in name
  temp3 <- as.data.frame(temp3)
  
  setDT(countTable, keep.rownames = TRUE)[]
  colnames(countTable)[1] = "ID"
  countTable <- as.data.frame(countTable)
  tt <- merge(countTable, temp3, by="ID")
  return(tt)
}









