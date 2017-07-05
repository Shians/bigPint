library(data.table)

dds <- readRDS("/Users/lindz/bigPint/tblshoot/data_limma.Rds")
rld <- readRDS("/Users/lindz/bigPint/tblshoot/genePval_limma.Rds")

dat <- as.data.frame(dds[[1]])
setDT(dat, keep.rownames = TRUE)[]
colnames(dat)[1] <- "ID"
dat$ID <- as.factor(dat$ID)
dat <- as.data.frame(dat)

myLevels <- unique(sapply(colnames(dat)[-1], function(x) unlist(strsplit(x,"[.]"))[1]))
myPairs <- list()

# Runs exact test on all pairs of groups and saves in list
k=1
for (i in 1:(length(myLevels)-1)){
  for (j in (i+1):(length(myLevels))){
    dat[[paste(i,j,"FC",sep="-")]] <- rld[[paste0(myLevels[[i]],"vs",myLevels[[j]])]]$logFC
    dat[[paste(i,j,"pval",sep="-")]] <- -log(rld[[paste0(myLevels[[i]],"vs",myLevels[[j]])]]$P.Value)
    myPairs[[k]] <- paste(myLevels[i], " and ", myLevels[j])
    k=k+1
  }
}

saveRDS(dat, "beeVolcanoData_limma.rds")

