# This function outputs several items into a folder called PermLineup:
# 1) Triangle lineups of the top 100 DEGs
# 2) Correct locations in Correct.csv
# 3) TopDEG1.csv (real data), TopDEG2.csv (permuted), TopDEG3.csv (permuted)
# 4) Permutations.csv tells the permutation orders

# It is different from sigDiffPerm.R because it does not include FDR value in plot outputs.

library(ggplot2)
library(edgeR)
library(dplyr)
library(gtools)
library(tibble)
library(readr)
library(EDASeq)
library(data.table)

# countTable should be a dataframe with first column (type "chr") named "ID" and rest of columns (type "num") named sample names. Have no extra columns. The sample names must be in the format groupName.repNumber (ex: DU.1).

getLineups <- function(countTable, group1, group2, nRep, nPerm, outDir){
  groupNames = unlist(lapply(colnames(countTable), function (x) unlist(strsplit(x, "[.]"))[1]))
  listCond = groupNames[c(which(unlist(lapply(colnames(countTable), function (x) unlist(strsplit(x, "[.]"))[1]))==group1), which(unlist(lapply(colnames(countTable), function (x) unlist(strsplit(x, "[.]"))[1]))==group2))]
  permCols = colnames(countTable)[which(groupNames %in% c(group1, group2))]
  
  allComb <- getPerms(length(listCond))
  allCombLab <- allComb
  for (i in 1:(length(listCond))){
    allCombLab[which(allCombLab == i)] = permCols[i]
  }

  permList <- list()
  correctPlace <- list()

  if (!dir.exists(paste0(getwd(), "/", outDir))){
    dir.create(paste0(getwd(), "/", outDir))
  }
  
  countTable2 = countTable[,c(which(groupNames %in% "ID"), which(groupNames %in% c(group1, group2)))]
  countTable2 = as.data.frame(countTable2)
  # Keep original order for first row, then obtain random rows from allCombLab
  randPerm = c(1, sample(2:nrow(allCombLab), (nPerm-1)))
  # tt <- merge( countTable2, topGenes[[paste0(group1, "vs", group2)]], by="ID")
  # permList[[1]] = arrange(tt, adjPVal)
  # write.csv(permList[[1]], file= paste0(getwd(), "/", outDir, "/TopDEG1.csv"))
    
  for(i in 1:nPerm){
    # OLD METHOD DOESN'T WORK (CANNOT JUST REORDER COLUMNS, CREATES REPETITIONS)
    # colnames(countTable2) <- allCombLab[randPerm[i],]
    # countTable2 <- as.data.frame(countTable2)
    
    # NEW METHOD
    countTable3 <- countTable2[allCombLab[randPerm[i],]]
    colnames(countTable3) <- colnames(countTable2)
    tt <- getFDR(countTable3)
    permList[[i]] = arrange(tt, adjPVal)
    write.csv(permList[[i]], file= paste(getwd(), "/", outDir, "/TopDEG", i, ".csv", sep=""))
  }

  for (i in 1:25){
    fullDat <- data.frame()
    lineup <- permute(seq(1:nPerm))
    correctPlace[i] <- which(lineup==1)
    for (j in 1:nPerm){
      gene = permList[[j]][i,2:(2*nRep+1)]
      x = unlist(lapply(colnames(gene), function (x) unlist(strsplit(x, "[.]"))[1]))
      x[x==group1] <- 1
      x[x==group2] <- 2
      dat = data.frame(x=x,y=t(gene),z=which(lineup==j))
      colnames(dat)=c("x","y","z")
      dat$x=as.factor(dat$x)
      levels(dat$x)=c(group1,group2)
      dat$meanG1 = mean(filter(dat, x==group1)$y)
      dat$meanG2 = mean(filter(dat, x==group2)$y)
      fullDat <- rbind(fullDat, dat)
    }
    #if (indScale){
      allPlot = ggplot(fullDat, aes(x, y)) + geom_point(aes(colour = factor(x)), shape = 20, size=5, alpha = 0.5) + scale_shape(solid = FALSE) + ggtitle(paste("Transcript: ", i)) + ylab("Read Count") + theme(axis.title.x = element_blank(), legend.position="bottom", axis.text=element_text(size=12), axis.title=element_text(size=12), legend.title=element_text(size=12), legend.text=element_text(size=12), plot.title=element_text(hjust=0.5)) + labs(colour = "Group", size=12) + geom_segment(aes(x = 1, y = meanG1, xend = 2, yend = meanG2), colour="gray25", size = 0.1) + facet_wrap(~ z, ncol = 5, scales = "free_y")
    #}
    #else{
      allPlot2 = ggplot(fullDat, aes(x, y)) + geom_point(aes(colour = factor(x)), shape = 20, size=5, alpha = 0.5) + scale_shape(solid = FALSE) + ggtitle(paste("Transcript: ", i)) + ylab("Read Count") + scale_y_continuous(limits=c(0, max(fullDat$y))) + theme(axis.title.x = element_blank(), legend.position="bottom", axis.text=element_text(size=12), axis.title=element_text(size=12), legend.title=element_text(size=12), legend.text=element_text(size=12), plot.title=element_text(hjust=0.5)) + labs(colour = "Group", size=12) + geom_segment(aes(x = 1, y = meanG1, xend = 2, yend = meanG2), colour="gray25", size = 0.1) + facet_wrap(~ z, ncol = 5)
    #}
    jpeg(file = paste0(getwd(), "/", outDir, "/ind_", "Gene", i, ".jpg"), height = ceiling(nPerm/5)*175, width = 700)
    print(allPlot)
    dev.off()
    jpeg(file = paste0(getwd(), "/", outDir, "/global_", "Gene", i, ".jpg"), height = ceiling(nPerm/5)*175, width = 700)
    print(allPlot2)
    dev.off()
  }

  correctPlace = data.frame(correctPlace)
  colnames(correctPlace) = 1:25
  correctPlace = t(correctPlace)
  colnames(correctPlace) = "DataPlot"
  correctPlace = data.frame(correctPlace)
  correctPlace <- rownames_to_column(correctPlace, "Gene")
  write.csv(correctPlace, row.names = FALSE, file = paste0(getwd(), "/", outDir, "/Correct.csv"))
  write.table(as.data.frame(allCombLab[randPerm[1:nPerm],]), sep=",",  col.names=FALSE, file = paste0(getwd(), "/", outDir, "/Permutations.csv"))
}

countTable <- readRDS("/Users/lindz/bigPint/BeeCountOrig.Rds")
groups <- unique(unlist(lapply(colnames(countTable), function (x) unlist(strsplit(x, "[.]"))[1])))

for (i in 1:(length(groups)-1)){
  for (j in (i+1):length(groups)){
    group1 = groups[i]
    group2 = groups[j]
    getLineups(countTable = countTable, group1=group1, group2=group2, nRep=6, nPerm=20, outDir=paste0(group1, "_", group2))
  }
}

